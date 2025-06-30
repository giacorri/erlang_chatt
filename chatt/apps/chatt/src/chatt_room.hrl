-record(state, {
    users = #{},                            %% #{Username => Socket}
    rooms = #{},                            %% #{RoomName => sets:set(Username)}
    user_rooms = #{},                       %% #{Username => RoomName}
    room_creators = #{},                    %% #{RoomName => Creator}
    private_rooms = sets:new(),             %% Set of private room names
    room_invitations = #{}                  %% #{RoomName => sets:users()}
}).