provider "aws" {
  region = var.aws_region
}

resource "aws_key_pair" "chat_key" {
  key_name   = var.key_pair_name
  public_key = file(var.public_key_path)
}

resource "aws_security_group" "chat_sg" {
  name        = "erlang-chat-sg"
  description = "Allow SSH, and Erlang chat port"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # SSH
  }

  ingress {
    from_port   = 1234
    to_port     = 1234
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Erlang chat port
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["099720109477"] # Canonical

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

data "aws_vpc" "default" {
  default = true
}

resource "aws_eip" "chat_server_eip" {
}

resource "aws_eip_association" "chat_server_assoc" {
  instance_id   = aws_instance.chat_server.id
  allocation_id = aws_eip.chat_server_eip.id
}

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

resource "aws_instance" "chat_server" {
  ami                    = data.aws_ami.ubuntu.id
  instance_type          = "t2.micro"
  key_name               = aws_key_pair.chat_key.key_name
  security_groups        = [aws_security_group.chat_sg.name]
  iam_instance_profile   = aws_iam_instance_profile.chat_profile.name

  user_data = <<-EOF
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

              EOF

  tags = {
    Name = "ErlangChatServer"
  }
}

resource "aws_iam_instance_profile" "chat_profile" {
  name = "erlang-chat-instance-profile"
  role = aws_iam_role.ec2_role.name
}
