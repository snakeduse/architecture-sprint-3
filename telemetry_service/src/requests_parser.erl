-module(requests_parser).

-export([
    parse_json/1
]).

parse_json(Json) ->
    jsone:decode(Json).
