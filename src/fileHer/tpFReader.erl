-module(tpFReader).

-export([
   fold/3
]).

fold(Fun, Acc, Filename) ->
   {ok, IoDevice} = file:open(Filename, [read, binary]),
   Ret = readEvent(IoDevice, Fun, Acc),
   ok = file:close(IoDevice),
   Ret.

readEvent(IoDevice, Fun, Acc) ->
   case file:read(IoDevice, 4) of
      {ok, <<BinSize:32>>} ->
         case file:read(IoDevice, BinSize) of
            {ok, Data} ->
               EventList = lists:reverse(binary_to_term(zlib:unzip(Data))),
               NewAcc = handleEvent(EventList, Fun, Acc),
               readEvent(IoDevice, Fun, NewAcc);
            eof ->
               {ok, Acc};
            {error, Reason} ->
               {error, Reason, 'readEvent data error'}
         end;
      eof ->
         {ok, Acc};
      {error, Reason} ->
         {error, Reason, 'readEvent size error'}
   end.

handleEvent([], _Fun, Acc) ->
   Acc;
handleEvent([OneEvent | EventList], Fun, Acc) ->
   NewAcc = Fun(OneEvent, Acc),
   handleEvent(EventList, Fun, NewAcc).
