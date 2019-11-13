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
-include("sw.hrl").

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


new_board() ->
    adjust_board_size(#board{}).

adjust_board_size(#board{width  = W,
                         height = H,
                         cell   = #cell{type = square,
                                        size = Size}} = B) ->

    Width  = (W div Size) * Size,
    Height = (H div Size) * Size,

    B#board{width  = Width,
            height = Height}.




start() ->
    win().

win() ->
    B       = new_board(),
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, B#board.width, B#board.height, ?bg),
    Canvas  = swCanvas:make(Win,0,0,B#board.width,B#board.height,1,?gray88),
    Self = self(),
    Canvas ! {onClick, fun({_,X,Y,_,_}) ->
                               Self ! {click, X, Y}
                       end},

    draw_board(Canvas, B),

    %%draw_pump(Canvas, B, _Row = 3, _Col = 5),

    loop(Canvas, B).




animate_pump(Canvas, B, Row, Col) ->
    {Xorigo, Yorigo, Radius} = get_pump_pos(B, Row, Col),

    Step = (Radius - 2) div 3,

    for(Step, Radius-2, Step,
        fun(I) ->
                ?dbg("draw_pump: Radius = ~p~n",[I]),
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




draw_board(Canvas, #board{width = Width, height = Height, cell = Cell} = _B) ->
    newPen(Canvas, thin, ?black, 1),
    newPen(Canvas, blue, ?blue, 1),

    draw_cells(Canvas, Width, Height, Cell).


draw_cells(Canvas, Width, Height, #cell{type = square, size = Size}) ->

    Yinit     = 0,
    MaxWidth  = Width,
    MaxHeight = Height,

    for(Yinit, MaxHeight, Size,
        fun(Y) ->
                draw_row(Canvas, Y, MaxWidth, Size, Size)
        end).


draw_row(Canvas, Y, MaxWidth, Width, Height) ->

    for(0, MaxWidth, Width,
        fun(X) ->
                draw(Canvas, thin, {rectangle, X, Y, Width, Height}),
                %% Write out some Cell pos info!
                dbg_cell_pos(Canvas, Width, Height, X, Y)
        end).


dbg_cell_pos(Canvas, Width, Height, X, Y) ->
    CoordStr = lists:flatten(io_lib:format("~p,~p",[X,Y])),
    CellStr = lists:flatten(io_lib:format("~p,~p",
                                          [cell_div(Y,Height),
                                           cell_div(X,Width)])),
    draw(Canvas, thin, {text, X, Y+20, CoordStr}),
    draw(Canvas, thin, {text, X, Y+40, CellStr}).

cell_div(_A,0) -> 0;
cell_div(A,B)  -> A div B.


is_inside_cell(#board{cell = #cell{type = square, size = Size}} ,X, Y) ->
    {cell_div(Y, Size),  % row
     cell_div(X, Size)}. % col

loop(Canvas, B) ->
    receive

        {click, Wx, Wy} ->
            {Row, Col} = Point = is_inside_cell(B, Wx, Wy),
            ?dbg("Clicked at: (~p,~p) is inside cell: ~p~n",
                 [Wx, Wy, Point]),
            spawn(fun() ->
                          animate_pump(Canvas, B, Row, Col)
                  end),
            loop(Canvas, B);

        Any ->
            ?dbg("received:~p~n",[Any]),
            loop(Canvas, B)

    end.


for(I, Max, _Step, _F) when I >= Max ->
    [];
for(I, Max, Step, F) ->
    [F(I)|for(I+Step,Max,Step,F)].
