-module(response_serializer).

-export([
    serialize_json/1
]).

serialize_json(Data) ->
    jsone:encode(Data).
