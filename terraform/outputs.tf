output "nlb_dns_name" {
  description = "DNS name of the Network Load Balancer"
  value       = aws_lb.chat_nlb.dns_name
}
