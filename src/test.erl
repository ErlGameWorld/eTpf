-module(test).

-export([test/1]).

test(Args) ->
	test2(Args).

test2(Args) ->
	test3(Args).

test3(Args) ->
	{ok, Args}.
