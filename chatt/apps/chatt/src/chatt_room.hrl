-record(state, {
    users = #{},               %% #{Username => Socket}
    rooms = #{},               %% #{RoomName => sets:set(Username)}
    user_rooms = #{},          %% #{Username => RoomName}
    room_creators = #{}        %% #{RoomName => Creator}
}).