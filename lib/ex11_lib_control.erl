-module(ex11_lib_control).

%% author: joe@sics.se
%% 2004-02-15 Added support for multiple screens
%%            Frej Drejhammar <frej@stacken.kth.se>

%% ex11_connect only handles the connection
%%   connection is complex - mainly because some large data structures need
%%   parsing.

%% ex11_connect:start() -> 
%%    {ok, Pid, Screen}    if the connection works
%%    {error, Why} otherwise

-export([start/0]).

-import(lists, [foldl/3, foreach/2, last/1, member/2]).

-import(dict, [new/0, erase/2, fetch/2, store/3, find/2]).
-import(ex11_lib, [eDestroyWindow/1]).

-include("ex11_lib.hrl").

%% Note most of the RPC routines to this module are defined in
%% ex11_lib.erl

start() ->
    S = self(),
    Pid = spawn_link(fun() -> init(S) end),
    %% io:format("ex11_lib_connect start/1 Pid=~p~n",[Pid]),
    receive
        {Pid, Ack} ->
            Ack
    end.
init(From) ->
    process_flag(trap_exit, true),
    init_reply_list(),
    case ex11_lib_driver:start() of
        {ok, {Driver, Display, Screen}} ->
            %% Screen is the screen we were started with
            %% ie the screen in the DISPLAY variable
            %% This may or may not be the screen that we want to use
            %% io:format("Display=~p~n",[Display]),
            %% ?PRINT_DISPLAY(Display),
            %% The next line is horribly wrong
            %% there is not one color fun
            %% but a large number (one for each visual in each screen)
            ColorFun = ex11_lib_utils:mk_colorfun(Display, Screen),
            DefaultScreen =  Display#display.default_screen,
            DefaultDepth = ?ROOT_DEPTH(Display, DefaultScreen),
            DefaultWin = ?ROOT_ID(Display, DefaultScreen),
            %% io:format("ex11_lib_control: Screen=~p~n",[Screen]),
            %% io:format("ex11_lib_control: default Screen=~p~n",
            %% [DefaultScreen]),
            DictVals =
                [{color, ColorFun},
                 {defaultScreen, DefaultScreen},
                 {defaultWindow, DefaultWin},
                 {defaultDepth, DefaultDepth},
                 {{depth, DefaultWin}, DefaultDepth},
                 {resource_id, Display#display.resource_id},
                 {resource_shift, Display#display.resource_shift},
                 {resource_mask, Display#display.resource_mask},
                 {resource_base, Display#display.resource_base}
                ],
            Db0 = dict:from_list(DictVals),
            reply(From, {ok, self(), Screen}),
            %% io:format("ex11_lib_control DISPLAY==~p~n",[self()]),
            %% io:format("ex11_lib_control ie DRIVER=~p~n",[Driver]),
            %% io:format("Starting a keyboard driver~n"),
            ex11_lib_keyboard_driver:ensure_started(self()),
            timer:send_interval(4000,self(),gc_reply),
            loop(From, Driver, 1, Display, Db0, []);
        Error ->
            reply(From, Error)
    end.


reply(Pid, X) -> Pid ! {self(), X}.

%% FreeIds = Free list of Id's

%% Here Display is the display RECORD
%% In the user programs Dispaly is the Pid of this program

store1(Key, Val, Dict) ->
    case dict:find(Key, Dict) of
        error -> dict:store(Key, [Val], Dict);
        {ok, _} -> dict:append(Key, Val, Dict)
    end.

%% sendLocalCmd can only be used for server casts NOT rpcs

%% sendLocalCmd({cast, Bin}) ->
%%     self() ! {sendCmd, Bin};
%% sendLocalCmd(_Other) ->
%%     io:format("** illegal usage sendLocalCommand ***"),
%%     exit({internalError,ex11_lib_connect,sendLocalCmd}).

sendLocalCmdFlush({cast, Bin}) ->
    self() ! {sendCmdFlush, Bin}.

onExit(Pid, D0) ->
    kill_owned_windows(Pid, D0).

kill_owned_windows(Pid, D0) ->
    %% io:format("kill owned windows Pid=~p~n",[Pid]),
    %% io:format("kill owned windows Pid=~p~n~p~n",[Pid,dict:to_list(D0)]),
    case find(Key={owns,Pid}, D0) of
        error ->
            io:format("nothing to kill~n"),
            D0;
        {ok, Wins} ->
            io:format("~p owns ~p~n~n", [Pid,Wins]),
            D1 = erase(Key, D0),
            foldl(fun kill_win/2, D1, Wins)
    end.

kill_win(Win, D0) ->
    D2 = case find(Key={children,Win}, D0) of
             error ->
                 %% io:format("Win ~p has no children~n",[Win]),
                 D0;
             {ok, Wins} ->
                 %% io:format("~p has children ~p~n", [Win,Wins]),
                 D1 = erase(Key, D0),
                 foldl(fun kill_win/2, D1, Wins)
         end,
    really_kill_window(Win, D2).

really_kill_window(Win, D0) ->
    D1 = case find(Win, D0) of
             error ->
                 %% io:format("Win ~p has no controlling process~n",[Win]),
                 D0;
             {ok, _Pid} ->
                 %% io:format("removing controller for ~p (was ~p)~n",
                 %% [Win,Pid]),
                 erase(Win, D0)
         end,
    %% B1 = <<Win:32>>, io:format("Destroy window:~p~n",[B1]),
    sendLocalCmdFlush(eDestroyWindow(Win)),
    D1.

add_new_window(Pid, Wid, Parent, Depth, D0) ->
    D1 = store1({owns,Pid}, Wid, D0),
    D2 = store1({children,Parent}, Wid, D1),
    D3 = store({depth,Wid}, Depth, D2),
    store(Wid, Pid, D3).

loop(Client, Driver, Seq, Display, D0, FreeIds) ->
    receive
        {addGCtoWin, {Win, GC}} ->
            D1 = case dict:find(Key={gcs,Win}, D0) of
                     {ok, L} ->
                         dict:store(Key, [GC|L], D0);
                     error ->
                         dict:store(Key, [GC], D0)
                 end,
            loop(Client, Driver, Seq, Display, D1, FreeIds);
        {newWindow, Pid, Wid, Parent, Depth} ->
            %% io:format("newWindow Pid=~p Wid=~p Parent=~p~n",
            %% [Pid,Wid,Parent]),
            link(Pid),
            D1 = add_new_window(Pid, Wid, Parent, Depth, D0),
            loop(Client, Driver, Seq, Display, D1, FreeIds);
        {addWindow, _Pid, _Type, _Wid} ->
            io:format("addWindow deprectiated~n"),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, id} ->
            reply(From, {self(), top}),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {_From, {add_widget, _Parent, _Child}} ->
            io:format("add_widget depreciated~n"),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {sendEventsIn, _Win, to, _Pid} ->
            io:format("sendEventsIn depreciated~n"),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, {color, C}} ->
            F = fetch(color, D0),
            reply(From, F(C)),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, create_id} ->
            case FreeIds of
                [Id|FreeIds0] ->
                    reply(From, Id),
                    loop(Client, Driver, Seq, Display, D0, FreeIds0);
                [] ->
                    %% {Id, Display1} = ex11_lib_utils:xalloc_id(Display),
                    {Id, D1} = ex11_lib_utils:xalloc_id1(D0),
                    reply(From, Id),
                    loop(Client, Driver, Seq, Display, D1, FreeIds)
            end;
        {From, {free_id, Id}} ->
            reply(From, ack),
            loop(Client, Driver, Seq, Display, D0, [Id|FreeIds]);
        {From, get_display} ->
            reply(From, Display),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, {get_display, X}} ->
            reply(From, get_display_fact(X, Display)),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, {get_root_of_screen, Screen}} ->
            S = lists:nth(Screen + 1, Display#display.screens),
            reply(From, S#screen.root),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, get_default_depth} ->
            reply(From, fetch(default_depth, D0)),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        flush ->
            %%io:format("lib_connect got flush~n"),
            Driver ! flush,
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {sendCmd, C} ->
            %%io:format("~p(~p) Command:~p ~p~n",[?MODULE,?LINE,Seq,C]),
            ex11_lib_driver:send_cmd(Driver, C),
            loop(Client, Driver, Seq+1, Display, D0, FreeIds);

        {sendCmdFlush, C} ->
            %%io:format("~p(~p) Command:~p ~p~n",[?MODULE,?LINE,Seq,C]),
            ex11_lib_driver:send_cmd_flush(Driver, C),
            loop(Client, Driver, Seq+1, Display, D0, FreeIds);

        %% {From, {cmd, {call, C, ReplyType}}} ->
        %%     io:format("~p(~p) I want a reply type ~w to msg:~p~n",
        %%               [?MODULE,?LINE,ReplyType,Seq]),
        %%     ex11_lib_driver:send_cmd_flush(Driver, C),
        %%     receive
        %%         {reply, Seq, R} ->
        %%             io:format("Reply (~p) was ~p bytes~n",[ReplyType, size(R)]),
        %%             Parse = ex11_lib:pReply(ReplyType, R),
        %%             reply(From, {ok, Parse});
        %%         {reply, Seq1, _R} ->
        %%             io:format("Reply with wrong sequence number:~p , Msg=~p~n",
        %%                       [Seq1,_R]),
        %%             reply(From, {error, sync});
        %%         {error, Seq, E} ->
        %%             reply(From, {error, E})
        %%     after 3000 ->
        %%             reply(From, {error, noReply})
        %%     end,
        %%     loop(Client, Driver, Seq+1, Display, D0, FreeIds);
        {From, {cmd, {call, C, ReplyType}}} ->
            io:format("~p(~p) I want a reply type ~w to msg:~p~n",
                      [?MODULE,?LINE,ReplyType,Seq]),
            push_reply(Seq, From, ReplyType),
            ex11_lib_driver:send_cmd_flush(Driver, C),
            loop(Client, Driver, Seq+1, Display, D0, FreeIds);


        {reply, SeqNo, R} ->
            io:format("Reply (~p) was ~p bytes: ~p~n",[SeqNo, size(R),R]),
            case get_reply(SeqNo) of
                {ok,From,ReplyType} ->
                    Parse = ex11_lib:pReply(ReplyType, R),
                    reply(From, {ok, Parse});
                _ ->
                    io:format("No reply recipient found to Seq(~p)~n",[SeqNo])
            end,
            loop(Client, Driver, Seq, Display, D0, FreeIds);

        {error, SeqNo, E} ->
            case get_reply(SeqNo) of
                {ok,From,_ReplyType} ->
                    reply(From, {error, E});
                _ ->
                    io:format("No error recipient found to Seq(~p)~n",[SeqNo])
            end,
            loop(Client, Driver, Seq, Display, D0, FreeIds);

        gc_reply ->
            gc_reply(),
            loop(Client, Driver, Seq, Display, D0, FreeIds);


        {From, {xGetVar, Key}} ->
            reply(From, find(Key, D0)),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {From, {xSetVar, Key, Val}} ->
            D1 = store(Key, Val, D0),
            reply(From, ack),
            loop(Client, Driver, Seq, Display, D1, FreeIds);
        {event, Type, B} = _E ->
            %% io:format("Event:~p~n",[E]),
            case dispatchable_type(Type) of
                true ->
                    {Win, Parse} = ex11_lib:eParseEvent(Type, B),
                    %% io:format("Event:~p Win = ~p data=~p~n",
                    %% [Type,Win,Parse]),
                    %% io:format("lookup ~p~n",[{actions,Win,Type}]),
                    case find(Win, D0) of
                        error ->
                            io:format("no actions ~p~n",[Type]),
                            true;
                        {ok, Pid} ->
                            %% io:format("sending to:~p~n",[Pid]),
                            Pid ! {event,Win,Type,Parse}
                    end;
                false ->
                    void
            end,
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {Driver, X} ->
            io:format("Driver message:~p~n",[X]),
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        {'EXIT',Pid,Why} ->
            io:format("DISPLAY caught exit:~p why=~p~n",[Pid,Why]),
            %% Delete all managed windows
            D1 = onExit(Pid, D0),
            loop(Client, Driver, Seq, Display, D1, FreeIds);
        {error, _Seq, Res, Args} ->
            %% If this is something we are managing then it's
            %% a real error otherwise ignore
            %% Lookup Res as if it's a window
            case find(Res, D0) of
                error ->
                    %% silently drop this error
                    void;
                {ok, P} when is_pid(P) ->
                    %% its a managed window
                    P ! {error, Res, Seq, Args}
            end,
            loop(Client, Driver, Seq, Display, D0, FreeIds);
        X ->
            io:format("(ex11_lib_connect ie DRIVER) unexpected message:~p~n",
                      [X]),
            loop(Client, Driver, Seq, Display, D0, FreeIds)
    end.

%%
%% Attempt to handle replies out of order
%%
init_reply_list() ->
    put(reply_list, []).

push_reply(Seq, From, ReplyType) ->
    case get(reply_list) of
        undefined ->
            put(reply_list,
                [{Seq,From,ReplyType,erlang:monotonic_time(second)}]);
        L ->
            put(reply_list,
                [{Seq,From,ReplyType,erlang:monotonic_time(second)} | L])
    end.

get_reply(Seq) ->
    case lists:keytake(Seq, 1, get(reply_list)) of
        {value, {Seq,From,ReplyType,_Tc}, NewReplyList} ->
            put(reply_list, NewReplyList),
            {ok, From, ReplyType};
        false ->
            not_found
    end.

gc_reply() ->
    put(reply_list, gc_reply(get(reply_list), erlang:monotonic_time(second))).

gc_reply([{_,_,_,OldTc}|L], NewTc) when NewTc-OldTc > 3 ->
    gc_reply(L, NewTc);
gc_reply([H|L], NewTc) ->
    [H|gc_reply(L, NewTc)];
gc_reply([], _) ->
    [].




dispatchable_type(expose)          -> true;
dispatchable_type(buttonPress)     -> true;
dispatchable_type(configureNotify) -> true;
dispatchable_type(enterNotify)     -> true;
dispatchable_type(leaveNotify)     -> true;
dispatchable_type(motionNotify)    -> true;
dispatchable_type(keyPress)        -> true;
dispatchable_type(_)               -> false.

get_display_fact({depth, S}, D) -> ?ROOT_DEPTH(D, S);
get_display_fact({parent, S}, D) -> ?ROOT_ID(D, S);
get_display_fact({whitePixel, S}, D) -> ?WHITE_PIXEL(D, S);
get_display_fact({blackPixel, S}, D) -> ?BLACK_PIXEL(D, S);
get_display_fact({colormap, S}, D) -> ?COLOR_MAP(D, S);
get_display_fact({defaultVisual, S}, D) -> ?DEFAULT_VISUAL(D, S);
get_display_fact(keycodes, D) ->
    {D#display.min_keycode, D#display.max_keycode};
get_display_fact(default_screen, D) -> D#display.default_screen.
