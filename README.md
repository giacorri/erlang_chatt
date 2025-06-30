# Erlang chat project

## Local deploy

Run tests and compile with

```bash
cd chatt
rebar3 ct
rebar3 release
```

then launch server with

```bash
sh chatt/_build/default/rel/chatt/bin/chatt foreground
```

and from other terminals connect to the chat server with

```bash
nc localhost 1234
```

then users will be able to specify their **username**, and with the following commands it will be possible to manage rooms:

- `/rooms` lists all available rooms;
- `/create <room name>` will create a room and make the user join it;
- `/create_private <private room name>` will create a private room and make the user join it;
- `/invite <username>` invites a user to the private room;
- `/join <room name>` will let a user join the specified room;
- `/leave` will make the user leave the current room;
- `/destroy <room name>` will delete the specified room, only if the user created it;

or send private messages with `@username msg`

## AWS Deploy

A Terraform folder is included, to deploy the project into an AWS EC2, with:

```bash
cd terraform
terraform apply
```

after creating a terraform variable file, in `terraform/terraform.tfvars` with the following structure:

```tfvars
key_pair_name   = "key_pair_name"
public_key_path = "~/path/to/public_key"
private_key_path = "~/path/to/private_key"
```

The deployment log can be found inside the EC2, by connecting through **SSH**, in `/var/log/user_data.log`

The deploy builds and launches a docker container running the chat server.

`nlb_dns_name` is the DNS address of the Network Load Balancer, output of `terraform apply`, which can be used to connect to the chat server after a succesful deploy with

```bash
nc nlb_dns_name 1234
```
