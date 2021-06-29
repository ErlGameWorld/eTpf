-module(tpMsgHole).
-behaviour(gen_srv).

-include("eTpf.hrl").

% 此过程的目的是成为跟踪过程发送的消息的目标。消息包含我们要在跟踪时记录的元数据，并在分析消息的发送时稍后使用。
% 这个过程不需要它们，它只需要存在就可以了，因此它丢弃了所有东西。

-export([
   start_link/0
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-spec start_link() -> {ok, pid()}.
start_link() ->
   gen_srv:start_link({local, ?eTpfHole}, ?MODULE, [], []).

init(_Args) ->
   {ok, undefined}.

handleCall(_Msg, _State, _FROM) ->
   {reply, ignored}.

handleCast(_Msg, _State) ->
   kpS.

handleInfo(_Msg, _State) ->
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
