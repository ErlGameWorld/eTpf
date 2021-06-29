-module(tpTermCut).
-include("eTpf.hrl").

-export([cut/1]).
-export([cut/2]).

getTcmValue(Key) ->
	case get(Key) of
		undefined ->
			maps:get(Key, ?defTcmMap);
		Value ->
			Value
	end.

cut(Term) ->
	cut(Term, 1).

cut(Term, Depth) ->
	TcmDepth = getTcmValue(tcmDepth),
	case Depth > TcmDepth of
		true ->
			'$truncated';
		_ ->
			if
				is_bitstring(Term) ->
					case bit_size(Term) > getTcmValue(tcmBitSize) of
						true ->
							TcmBinSize = getTcmValue(tcmBinSize),
							<<CutBin:TcmBinSize/binary, _/bits>> = Term,
							<<CutBin/binary, "$truncated">>;
						_ ->
							Term
					end;
				is_list(Term) ->
					cutList(Term, Depth, 0, getTcmValue(tcmListSize), getTcmValue(tcmNestStruct), 0, []);
				is_map(Term), Depth =:= TcmDepth ->
					#{'$truncated' => '$truncated'};
				is_map(Term) ->
					maps:from_list(cutMap(mapsToList(Term, getTcmValue(tcmMapSize)), Depth, getTcmValue(tcmNestStruct), 0));
				is_tuple(Term), Depth =:= TcmDepth ->
					{'$truncated'};
				is_tuple(Term) ->
					list_to_tuple(cutList(tuple_to_list(Term), Depth, 0, getTcmValue(tcmTupleSize), getTcmValue(tcmNestStruct), 0, []));
				true ->
					Term
			end
	end.

cutList([], _, _, _, _, _, Acc) ->
	lists:reverse(Acc);
cutList([Term | Tail], Depth, Len, TcmTupleSize, TcmNestStruct, NumStructs, Acc) ->
	case Len >= TcmTupleSize orelse NumStructs >= TcmNestStruct of
		true ->
			lists:reverse(['$truncated' | Acc]);
		_ ->
			%% if List was a cons, Tail can be anything
			cutList(Tail, Depth, Len + 1, TcmTupleSize, TcmNestStruct, NumStructs + isStruct(Term), [cut(Term, Depth + 1) | Acc])
	end;
cutList(Term, Depth, _, _, _, _, Acc) -> %% if List was a cons  [[[a,a,a|b],1,2,3]|bbbb]
	lists:reverse(Acc) ++ cut(Term, Depth + 1).

cutMap([], _, _, _) ->
	[];
cutMap(_, _, TcmNestStruct, NumStructs) when NumStructs > TcmNestStruct ->
	[{'$truncated', '$truncated'}];
cutMap([{Key, Value} | Tail], Depth, TcmNestStruct, NumStructs) ->
	AddStruct = isStruct(Key) + isStruct(Value),
	[{cut(Key, Depth + 1), cut(Value, Depth + 1)} | cutMap(Tail, Depth, TcmNestStruct, NumStructs + AddStruct)].

isStruct(Term) ->
	case is_list(Term) orelse is_map(Term) orelse is_tuple(Term) of
		true ->
			1;
		_ ->
			0
	end.

mapsToList(Map, MaxSize) ->
	I = maps:iterator(Map),
	mapsToList(maps:next(I), MaxSize, []).

%% Returns elements in arbitrary order. We reverse when we truncate
%% so that the truncated elements come at the end to avoid having
%% two truncated elements in the final output.
mapsToList(none, _, Acc) ->
	Acc;
mapsToList(_, 0, Acc) ->
	lists:reverse([{'$truncated', '$truncated'} | Acc]);
mapsToList({K, V, I}, N, Acc) ->
	mapsToList(maps:next(I), N - 1, [{K, V} | Acc]).
