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

-record(cell_square, {
          size = 60,

          origo_x,
          origo_y,
          radius,

          north_pid,
          east_pid,
          south_pid,
          west_pid
         }).

-record(board, {
          width  = 660,
          height = 660,
          cell_type = square,
          cell
         }).

-define(xbattle, xbattle).

new_board() ->
    ets:new(?xbattle, [set,protected,named_table,{read_concurrency,true}]),
    #board{}.

%% Adjust the board size to fit the cells.
adjust_board_size(#board{width  = W,
                         height = H,
                         cell   = #cell_square{size = Size}} = B) ->

    Width  = (W div Size) * Size,
    Height = (H div Size) * Size,

    B#board{width  = Width,
            height = Height}.

new_cell(#board{cell_type = square}) -> #cell_square{}.




start() ->
    spawn(fun() -> win() end).

win() ->
    process_flag(trap_exit, true),

    Board0  = new_board(),
    Cell    = new_cell(Board0),
    Board   = adjust_board_size(Board0#board{cell = Cell}),

    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display,Board#board.width,Board#board.height,?bg),
    Canvas  = swCanvas:make(Win,0,0,
                            Board#board.width,
                            Board#board.height,
                            1,?gray88),
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

    draw_board(Canvas, Board, Cell),

    start_cell_processes(),

    loop(Canvas, Board).


loop(Canvas, Board) ->
    receive

        {click, Wx, Wy} = Msg ->
            forward_msg(Board, Wx, Wy, Msg),
            loop(Canvas, Board);

        {key, _KeyInfo, {Wx, Wy}} = Msg->
            forward_msg(Board, Wx, Wy, Msg),
            loop(Canvas, Board);

        Any ->
            ?dbg("received:~p~n",[Any]),
            loop(Canvas, Board)

    end.

forward_msg(Board, Wx, Wy, Msg) ->
    Point = cell_coord(Board, Wx, Wy),
    [{_,CellPid}] = ets:lookup(?xbattle, Point),
    CellPid ! Msg.


start_cell_processes() ->
    Self = self(),
    ets:foldl(fun({{_Row,_Col},CellPid}, Acc) ->
                      CellPid ! {Self,start},
                      Acc
              end, ok, ?xbattle).


draw_board(Canvas, #board{width = Width, height = Height} = B, Cell) ->
    newPen(Canvas, thin, ?black, 1),
    newPen(Canvas, blue, ?blue, 1),

    draw_cells(Canvas, B, Width, Height, Cell).


draw_cells(Canvas, Board, Width, Height,
           #cell_square{size = Size} = Cell) ->

    Yinit     = 0,
    MaxWidth  = Width,
    MaxHeight = Height,

    for(Yinit, MaxHeight, Size,
        fun(Y) ->
                draw_row(Canvas, Board, Cell, Y, MaxWidth, Size, Size)
        end).


draw_row(Canvas, Board, Cell, Y, MaxWidth, Width, Height) ->

    for(0, MaxWidth, Width,
        fun(X) ->
                draw(Canvas, thin, {rectangle, X, Y, Width, Height}),

                %% Create the Cell Process
                setup_cell_proc(Canvas, Board, Cell, Width, Height, X, Y),

                %% Write out some Cell pos info!
                dbg_cell_pos(Canvas, Width, Height, X, Y)
        end).

setup_cell_proc(Canvas, Board, Cell, Width, Height, X, Y) ->
    Self = self(),
    CellCoord = cell_coord(Width, Height, X, Y),
    CellPid = spawn_link(fun() ->
                                 cell_init(Self, Canvas, Board, Cell, CellCoord)
                         end),
    ets:insert(?xbattle, {CellCoord,CellPid}),
    put(CellPid, CellCoord).

cell_coord(#board{cell = #cell_square{size = Size}} ,X, Y) ->
    {cell_div(Y, Size),  % row
     cell_div(X, Size)}. % col

cell_coord(Width, Height, X, Y) ->
    {cell_div(Y,Height),
     cell_div(X,Width)}.


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

cell_init(Controller, Canvas, Board, Cell0, {Row, Col} = Point) ->
    receive
        {Controller, start} ->
            Cell = set_pump_pos(Cell0, Row, Col),
            cell_loop(Controller, Canvas, Board, Cell, Point)
    end.

cell_loop(Controller, Canvas, Board, Cell, Point) ->
    receive

        {click, Wx, Wy} ->
            ?dbg("~p Clicked at: (~p,~p) is inside cell: ~p~n",
                 [self(), Wx, Wy, Point]),
            animate_pump(Canvas, Cell),
            cell_loop(Controller, Canvas, Board, Cell, Point);

        {key, {_State,_Key,_Type,Val}, {_Wx, _Wy} = Pos} ->
            ?dbg("~p Got key: ~p at pos: ~p~n", [self(),Val,Pos]),
            cell_loop(Controller, Canvas, Board, Cell, Point)

    end.


animate_pump(Canvas, Cell) ->
    {Xorigo, Yorigo, Radius} = get_pump_pos(Cell),

    Step = (Radius - 2) div 3,

    for(Step, Radius-2, Step,
        fun(I) ->
                draw_pump(Canvas, Xorigo, Yorigo, I),
                timer:sleep(1000)
        end).

draw_pump(Canvas, X, Y, Radius) ->
    draw(Canvas, blue, {filledCircle, X, Y, Radius}).


get_pump_pos(#cell_square{origo_x = X, origo_y = Y, radius = R}) ->
    {X, Y, R}.

set_pump_pos(#cell_square{size = Size} = Cell, Row, Col) ->

    Radius = Size div 2,

    Cell#cell_square{radius  = Radius,
                     origo_x = (Col * Size) + Radius,
                     origo_y = (Row * Size) + Radius}.
