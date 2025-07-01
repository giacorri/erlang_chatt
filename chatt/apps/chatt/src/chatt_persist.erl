-module(chatt_persist).
-export([
    init/0,
    load_rooms/0
]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ddb2.hrl").

init() ->
    Config = #aws_config{
        access_key_id = "fakeMyKeyId",
        secret_access_key = "fakeSecretAccessKey",
        aws_region = "us-east-1",
        ddb_scheme = "http://",
        ddb_host = "localhost",
        ddb_port = 8000
    },
    erlcloud_aws:configure(Config).

load_rooms() ->
    % erlcloud_ddb:scan(<<"ChattRooms">>, []).
    erlcloud_ddb2:list_tables().

