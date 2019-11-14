%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2019, Torbjorn Tornkvist
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2019 by Torbjorn Tornkvist <kruskakli@gmail.com>
%%%-------------------------------------------------------------------
-module(xbattle).

-export([start/0]).

-import(sw, [xStart/1]).
-import(swCanvas, [newPen/4, draw/3, delete/2]).


-include("xbattle.hrl").
-include("ex11_lib.hrl").


-define(bg, 16#ffffcc).

-record(cell, {
          type = square,
          size = 60
         }).

-record(board, {
          width  = 660,
          height = 660,
          cell   = #cell{}
         }).

-define(xbattle, xbattle).

new_board() ->
    ets:new(?xbattle, [set,protected,named_table,{read_concurrency,true}]),
    adjust_board_size(#board{}).

%% Adjust the board size to fit the cells.
adjust_board_size(#board{width  = W,
                         height = H,
                         cell   = #cell{type = square,
                                        size = Size}} = B) ->

    Width  = (W div Size) * Size,
    Height = (H div Size) * Size,

    B#board{width  = Width,
            height = Height}.




start() ->
    spawn(fun() -> win() end).

win() ->
    process_flag(trap_exit, true),

    B       = new_board(),
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, B#board.width, B#board.height, ?bg),
    Canvas  = swCanvas:make(Win,0,0,B#board.width,B#board.height,1,?gray88),
    Self    = self(),

    Canvas ! {onClick, fun({_,X,Y,_,_}) ->
                               Self ! {click, X, Y}
                       end},

    CanvasWin = swCanvas:getWin(Canvas),
    Canvas ! {onKey, fun(X) ->
                             {ok,P} = ex11_lib:xQueryPointer(Display,CanvasWin),
                             Kx     = P#pointerInfo.win_x,
                             Ky     = P#pointerInfo.win_y,
                             Cmd    = ex11_lib_keyboard_driver:analyse(X),
                             Self ! {key, Cmd, {Kx,Ky}}
                     end},

    draw_board(Canvas, B),

    start_cell_processes(),

    loop(Canvas, B).


loop(Canvas, B) ->
    receive

        {click, Wx, Wy} = Msg ->
            forward_msg(B, Wx, Wy, Msg),
            loop(Canvas, B);

        {key, _KeyInfo, {Wx, Wy}} = Msg->
            forward_msg(B, Wx, Wy, Msg),
            loop(Canvas, B);

        Any ->
            ?dbg("received:~p~n",[Any]),
            loop(Canvas, B)

    end.

forward_msg(B, Wx, Wy, Msg) ->
    Point = cell_coord(B, Wx, Wy),
    [{_,CellPid}] = ets:lookup(?xbattle, Point),
    CellPid ! Msg.


start_cell_processes() ->
    Self = self(),
    ets:foldl(fun({{_Row,_Col},CellPid}, Acc) ->
                      CellPid ! {Self,start},
                      Acc
              end, ok, ?xbattle).


draw_board(Canvas, #board{width = Width, height = Height, cell = Cell} = B) ->
    newPen(Canvas, thin, ?black, 1),
    newPen(Canvas, blue, ?blue, 1),

    draw_cells(Canvas, B, Width, Height, Cell).


draw_cells(Canvas, Board, Width, Height, #cell{type = square, size = Size}) ->

    Yinit     = 0,
    MaxWidth  = Width,
    MaxHeight = Height,

    for(Yinit, MaxHeight, Size,
        fun(Y) ->
                draw_row(Canvas, Board, Y, MaxWidth, Size, Size)
        end).


draw_row(Canvas, Board, Y, MaxWidth, Width, Height) ->

    for(0, MaxWidth, Width,
        fun(X) ->
                draw(Canvas, thin, {rectangle, X, Y, Width, Height}),

                %% Create the Cell Process
                setup_cell_proc(Canvas, Board, Width, Height, X, Y),

                %% Write out some Cell pos info!
                dbg_cell_pos(Canvas, Width, Height, X, Y)
        end).

setup_cell_proc(Canvas, Board, Width, Height, X, Y) ->
    Self = self(),
    CellCoord = cell_coord(Width, Height, X, Y),
    CellPid = spawn_link(fun() ->
                                 cell_init(Self, Canvas, Board, CellCoord)
                         end),
    ets:insert(?xbattle, {CellCoord,CellPid}),
    put(CellPid, CellCoord).

cell_coord(Width, Height, X, Y) ->
    {cell_div(Y,Height),
     cell_div(X,Width)}.

cell_coord(#board{cell = #cell{type = square, size = Size}} ,X, Y) ->
    {cell_div(Y, Size),  % row
     cell_div(X, Size)}. % col

dbg_cell_pos(Canvas, Width, Height, X, Y) ->
    CoordStr = lists:flatten(io_lib:format("~p,~p",[X,Y])),
    CellStr = lists:flatten(io_lib:format("~p,~p",
                                          [cell_div(Y,Height),
                                           cell_div(X,Width)])),
    draw(Canvas, thin, {text, X, Y+20, CoordStr}),
    draw(Canvas, thin, {text, X, Y+40, CellStr}).

cell_div(_A,0) -> 0;
cell_div(A,B)  -> A div B.


for(I, Max, _Step, _F) when I >= Max ->
    [];
for(I, Max, Step, F) ->
    [F(I)|for(I+Step,Max,Step,F)].

%%
%% C E L L   P R O C E S S
%%

cell_init(Controller, Canvas, Board, Point) ->
    receive
        {Controller, start} ->
            cell_loop(Controller, Canvas, Board, Point)
    end.

cell_loop(Controller, Canvas, Board, {Row,Col} = Point) ->
    receive

        {click, Wx, Wy} ->
            ?dbg("~p Clicked at: (~p,~p) is inside cell: ~p~n",
                 [self(), Wx, Wy, Point]),
            animate_pump(Canvas, Board, Row, Col),
            cell_loop(Controller, Canvas, Board, Point);

        {key, {_State,_Key,_Type,Val}, {_Wx, _Wy} = Pos} ->
            ?dbg("~p Got key: ~p at pos: ~p~n", [self(),Val,Pos]),
            cell_loop(Controller, Canvas, Board, Point)

    end.


animate_pump(Canvas, Board, Row, Col) ->
    {Xorigo, Yorigo, Radius} = get_pump_pos(Board, Row, Col),

    Step = (Radius - 2) div 3,

    for(Step, Radius-2, Step,
        fun(I) ->
                draw_pump(Canvas, Xorigo, Yorigo, I),
                timer:sleep(1000)
        end).

draw_pump(Canvas, X, Y, Radius) ->
    draw(Canvas, blue, {filledCircle, X, Y, Radius}).


get_pump_pos(#board{cell = #cell{type = square, size = Size}},
             Row, Col) ->

    Radius = Size div 2,

    Xorigo = (Col * Size) + Radius,
    Yorigo = (Row * Size) + Radius,

    {Xorigo, Yorigo, Radius}.
