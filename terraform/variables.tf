variable "aws_region" {
  default = "us-east-1"
}

variable "key_pair_name" {
  description = "Key pair name for SSH access"
}

variable "public_key_path" {
  description = "Path to local public key"
}

variable "private_key_path" {
  description = "Path to local private key"
}