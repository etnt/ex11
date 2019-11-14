-module(swCanvas).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% Started 2004-02-29 by joe@sics.se joe armstrong

%% A canvus with a clickable handler

-export([cdraw/3
         , delete_obj/2
         , make/7
         , newPen/4
         , draw/3
         , getWin/1
         , delete/2]).

-import(lists, [foreach/2, map/2, reverse/1]).

-include("sw.hrl").

-import(ex11_lib, [ePolyText8/5, rpc/2, sleep/1,
                   xClearArea/1,
                   eCopyArea/9,
                   eFillPoly/5,
                   ePolyArc/3,
                   ePolyFillArc/3,
                   ePolyFillRectangle/3,
                   ePolyLine/4,
                   ePolyRectangle/3,
                   mkArc/6,
                   mkPoint/2,
                   mkRectangle/4,
                   reply/2,
                   xClearArea/1,
                   xColor/2,
                   xCreateGC/2,
                   xCreateGC/3,
                   xCreatePixmap/4,
                   xDo/2,
                   xFlush/1,
                   xFreePixmap/2,
                   xVar/2]).

make(Parent, X, Y, Width, Ht, Border, Color) ->
    spawn_link(fun() -> init(Parent,  X, Y, Width, Ht, Border, Color) end).

newPen(Pid, Pen, Color, Width) ->
    Pid ! {newPen, Pen, Color, Width}.

draw(Pid, Pen, Obj) ->
    rpc(Pid, {draw, Pen, Obj}).

delete(Pid, Index) ->
    rpc(Pid, {delete, Index}).

getWin(Pid) ->
    rpc(Pid, getWin).


init(Parent, X, Y, Width, Ht, Border, Color) ->
    Display = rpc(Parent, display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
                 border=Border,width=Width,ht=Ht,color=Color,
                 type=label,
                 mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS
                     bor ?EVENT_KEY_PRESS},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    %% Make a pixmap
    Canvas =  xCreatePixmap(Display, Win, Width, Ht),
    Pen0 = xCreateGC(Display, [{function,copy},
                               {line_width,1},
                               {line_style,solid},
                               {graphics_exposures, false},
                               {foreground, xColor(Display, Color)}]),
    Clear = fun() ->
                    xDo(Display,
                        ePolyFillRectangle(Canvas, Pen0,
                                           [mkRectangle(0,0,Width, Ht)]))
            end,
    Clear(),
    %% onto the drawable - I *know* the underlying text is OK :-)
    GC = xCreateGC(Display, [{function,copy},
                             {line_width,2},
                             {line_style,solid},
                             {graphics_exposures, false},
                             {foreground, xColor(Display, Color)}]),
    Bin = eCopyArea(Canvas,Win,GC,0,0,0,0,Width, Ht),
    F = fun() ->
                xDo(Display, Bin),
                xFlush(Display)
        end,
    F(),
    BPress = fun(_) -> void end,
    KPress = fun(_) -> void end,
    loop(Display,Pen0,GC,Wargs1,Win,Canvas,dict:new(),
         {[],[]},1,F,BPress,KPress,Clear).


loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,L,Free,F,BPress,KPress,Clear) ->
    receive
        {resize, W, H} ->
            %% io:format("resize~n"),
            %% golly
            %% destroy the canvas
            xFreePixmap(Display, Canvas),
            %% make a new canvas
            Canvas1 =  xCreatePixmap(Display, Win, W, H),
            %% a new clear function
            Clear1 = fun() ->
                             xDo(Display,
                                 ePolyFillRectangle(Canvas1, Pen0,
                                                    [mkRectangle(0,0,W, H)]))
                     end,
            %% and a new copy
            Bin = eCopyArea(Canvas1,Win,GC,0,0,0,0, W, H),
            F1 = fun() ->
                         xDo(Display, Bin),
                         xFlush(Display)
                 end,
            self() ! {event, [], expose,[]},
            Wargs1 = sw:generic({setWidthHt,W,H}, Display, Wargs),
            loop(Display,Pen0,GC,Wargs1,Win,Canvas1,Pens,
                 L,Free,F1,BPress,KPress,
                 Clear1);
        erase ->
            Clear(),
            F(),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 {[],[]},1,F,BPress,KPress,
                 Clear);
        {event,_,expose, _} ->
            F(),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,BPress,KPress,Clear);
        {event,_,buttonPress,Args} ->
            BPress(Args),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,BPress,KPress,Clear);
        {event,_, keyPress, Args} ->
            KPress(Args),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,BPress,KPress,Clear);

        {'EXIT', _Pid, _Why} ->
            true;
        {onClick, Fun1} ->
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,Fun1,KPress,Clear);
        {onKey, Fun1} ->
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,BPress,Fun1,Clear);
        {newPen, Name, Color, Width} ->
            GC1 = xCreateGC(Display, Win,
                            [{function,copy},
                             {line_width,Width},
                             {line_style,solid},
                             {foreground, xColor(Display, Color)}]),
            Pens1 = dict:store(Name, GC1, Pens),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens1,
                 L,Free,F,BPress,KPress,Clear);
        {From, {draw, Pen, Obj}} ->
            case dict:find(Pen, Pens) of
                {ok, PenGC} ->
                    case cdraw(Canvas, PenGC, Obj) of
                        {error, What} ->
                            exit({badArg, What});
                        Bin ->
                            xDo(Display, Bin),
                            F(),
                            reply(From, Free),
                            {L1,L2} = L,
                            Ln = {L1, [{Free,Bin}|L2]},
                            loop(Display,Pen0,GC, Wargs, Win, Canvas, Pens,
                                 Ln,Free+1, F, BPress,KPress, Clear)
                    end;
                error ->
                    io:format("Invalid pen:~p~n",[Pen]),
                    exit({eBadPen, Pen})
            end;
        {From, {delete, Index}} ->
            {L1,L2} = L,
            L1a = L1 ++ reverse(L2),
            L2a = delete_obj(Index, L1a),
            L3 = {L2a, []},
            Clear(),
            foreach(fun({_,Bin}) -> xDo(Display, Bin) end, L2a),
            F(),
            reply(From, ack),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L3,Free,F,BPress,KPress,Clear);

        {From, getWin} ->
            From ! {self(), Win},
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,BPress,KPress, Clear);

        Any ->
            io:format("~p: Any = ~p~n",[?MODULE,Any]),
            %% Now we call the generic operators
            _Wargs1 = sw:generic(Any, Display, Wargs),
            loop(Display,Pen0,GC,Wargs,Win,Canvas,Pens,
                 L,Free,F,BPress,KPress, Clear)
    end.

delete_obj(I, [{I,_}|T]) -> T;
delete_obj(I, [H|T])     -> [H|delete_obj(I, T)];
delete_obj(_I, [])        -> [].

cdraw(Canvas, Pen, {line, X1, Y1, X2, Y2}) ->
    ePolyLine(Canvas, Pen, origin, [mkPoint(X1,Y1), mkPoint(X2,Y2)]);
cdraw(Canvas, Pen, {rectangle, X, Y, Width, Ht}) ->
    ePolyRectangle(Canvas, Pen, [mkRectangle(X,Y,Width,Ht)]);
cdraw(Canvas, Pen, {filledRectangle, X, Y, Width, Ht}) ->
    ePolyFillRectangle(Canvas, Pen, [mkRectangle(X,Y,Width,Ht)]);
cdraw(Canvas, Pen, {filledCircle, X, Y, Radius}) ->
    Xc = X - Radius,
    Yc = Y - Radius,
    Width = 2*Radius,
    ePolyFillArc(Canvas, Pen, [mkArc(Xc,Yc,Width,Width,0,64*360)]);
cdraw(Canvas, Pen, {arc, X, Y, Radius, S1, S2}) ->
    Width = 2 * Radius,
    Xc = X - Radius,
    Yc = Y - Radius,
    ePolyArc(Canvas, Pen, [mkArc(Xc,Yc,Width,Width,S1,S2)]);
cdraw(Canvas, Pen, {circle, X, Y, Radius}) ->
    Xc = X - Radius,
    Yc = Y - Radius,
    Width = 2*Radius,
    ePolyArc(Canvas, Pen, [mkArc(Xc,Yc,Width,Width,0,64*360)]);
cdraw(Canvas, Pen, {filledArc, X, Y, Radius, S1, S2}) ->
    Width = 2 * Radius,
    Xc = X - Radius,
    Yc = Y - Radius,
    ePolyFillArc(Canvas, Pen, [mkArc(Xc,Yc,Width,Width,S1,S2)]);
cdraw(Canvas, Pen, {lines, L}) ->
    ePolyLine(Canvas, Pen, origin,
              map(fun({X,Y}) -> mkPoint(X,Y) end, L));
cdraw(Canvas, Pen, {filledPoly, L}) ->
    eFillPoly(Canvas, Pen, complex, origin,
              map(fun({X,Y}) -> mkPoint(X,Y) end, L));
cdraw(Canvas, Pen, {text, X, Y, Str}) ->
    ePolyText8(Canvas, Pen, X, Y, Str);
cdraw(_Canvas, _Pem, X) ->
    io:format("Cannot draw:~p~n",[X]),
    {error, {eBadObj, X}}.
