provider "aws" {
  region = var.aws_region
}

# Create a new custom VPC
resource "aws_vpc" "chat_vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_support   = true
  enable_dns_hostnames = true

  tags = {
    Name = "chat-vpc"
  }
}

# Internet Gateway for the VPC
resource "aws_internet_gateway" "chat_igw" {
  vpc_id = aws_vpc.chat_vpc.id

  tags = {
    Name = "chat-igw"
  }
}

# Get available AZs
data "aws_availability_zones" "available" {
  state = "available"
}

# Public Subnet 1
resource "aws_subnet" "public_1" {
  vpc_id                  = aws_vpc.chat_vpc.id
  cidr_block              = "10.0.1.0/24"
  availability_zone       = data.aws_availability_zones.available.names[0]
  map_public_ip_on_launch = true

  tags = {
    Name = "chat-public-subnet"
  }
}

# Public Subnet 2
resource "aws_subnet" "public_2" {
  vpc_id                  = aws_vpc.chat_vpc.id
  cidr_block              = "10.0.2.0/24"
  availability_zone       = data.aws_availability_zones.available.names[1]
  map_public_ip_on_launch = true

  tags = {
    Name = "chat-public-subnet-2"
  }
}

# Route Table
resource "aws_route_table" "public_rt" {
  vpc_id = aws_vpc.chat_vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.chat_igw.id
  }

  tags = {
    Name = "chat-public-rt"
  }
}

# Associate route table with subnet 1
resource "aws_route_table_association" "public_assoc" {
  subnet_id      = aws_subnet.public_1.id
  route_table_id = aws_route_table.public_rt.id
}

# Associate route table with subnet 2
resource "aws_route_table_association" "public_assoc_2" {
  subnet_id      = aws_subnet.public_2.id
  route_table_id = aws_route_table.public_rt.id
}

# EC2 SSH key pair
resource "aws_key_pair" "chat_key" {
  key_name   = var.key_pair_name
  public_key = file(var.public_key_path)
}

# Security Group
resource "aws_security_group" "chat_sg" {
  name        = "erlang-chat-sg"
  description = "Allow SSH and Erlang chat port"
  vpc_id      = aws_vpc.chat_vpc.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 1234
    to_port     = 1234
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Ubuntu AMI
data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["099720109477"]

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

# IAM Role for EC2
resource "aws_iam_role" "ec2_role" {
  name = "erlang-chat-ec2-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = {
        Service = "ec2.amazonaws.com"
      }
      Action = "sts:AssumeRole"
    }]
  })
}

resource "aws_iam_role_policy_attachment" "ssm_access" {
  role       = aws_iam_role.ec2_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "chat_profile" {
  name = "erlang-chat-instance-profile"
  role = aws_iam_role.ec2_role.name
}

# Launch Template
resource "aws_launch_template" "chat_template" {
  name_prefix          = "chat-launch-"
  image_id             = data.aws_ami.ubuntu.id
  instance_type        = "t2.micro"
  key_name             = aws_key_pair.chat_key.key_name
  vpc_security_group_ids = [aws_security_group.chat_sg.id]

  iam_instance_profile {
    name = aws_iam_instance_profile.chat_profile.name
  }

  user_data = base64encode(<<-EOF
              #!/bin/bash
              exec > /var/log/user_data.log 2>&1
              set -x
              apt-get update
              apt-get install -y docker.io git curl
              systemctl start docker
              systemctl enable docker
              git clone https://github.com/giacorri/erlang_chatt.git /opt/chatapp
              cd /opt/chatapp
              docker build -t chatapp .
              docker run -d --name chat -p 1234:1234 chatapp

              # Create data directory
              mkdir -p /var/lib/dynamodb_data
              chown ubuntu:ubuntu /var/lib/dynamodb_data

              docker pull amazon/dynamodb-local

              # Run DynamoDB Local container with persistence
              docker run -d \
                -p 8000:8000 \
                -v /var/lib/dynamodb_data:/home/dynamodblocal/data \
                --name dynamodb_local \
                amazon/dynamodb-local \
                -jar DynamoDBLocal.jar -sharedDb -dbPath /home/dynamodblocal/data
              EOF
  )

  tags = {
    Name = "ErlangChatServer"
  }
}

# Natwork load balancer
resource "aws_lb" "chat_nlb" {
  name               = "chat-nlb"
  internal           = false
  load_balancer_type = "network"
  subnets            = [aws_subnet.public_1.id, aws_subnet.public_2.id] # Add at least 2 subnets in different AZs
}

# Target Group
resource "aws_lb_target_group" "chat_target_group" {
  name     = "chat-targets"
  port     = 1234
  protocol = "TCP"
  vpc_id   = aws_vpc.chat_vpc.id

  health_check {
    protocol = "TCP"
    port     = "traffic-port"
    interval = 30
    timeout  = 10
    healthy_threshold   = 3
    unhealthy_threshold = 3
  }

  lifecycle {
    prevent_destroy = false
  }

}

# Listener
resource "aws_lb_listener" "chat_listener" {
  load_balancer_arn = aws_lb.chat_nlb.arn
  port              = 1234
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.chat_target_group.arn
  }

  depends_on = [aws_lb_target_group.chat_target_group] 
}

# Auto Scaling Group
resource "aws_autoscaling_group" "chat_asg" {
  name                      = "chat-asg"
  min_size                  = 1
  max_size                  = 2
  desired_capacity          = 1
  vpc_zone_identifier       = [aws_subnet.public_1.id]
  health_check_type         = "EC2"
  health_check_grace_period = 300
  target_group_arns         = [aws_lb_target_group.chat_target_group.arn]

  launch_template {
    id      = aws_launch_template.chat_template.id
    version = "$Latest"
  }

  tag {
    key                 = "Name"
    value               = "ChatASGInstance"
    propagate_at_launch = true
  }

  # Trigger rolling update on changes
  instance_refresh {
    strategy = "Rolling"
  }
}
