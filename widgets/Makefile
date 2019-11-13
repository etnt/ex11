CC = gcc
CFLAGS = -I/usr/local/X11/include

LIBS = -L/usr/X11R6/lib -lX11

## make
## make demo

## Note rgb.erl does compile but it takes a very long time???

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W -I ../lib $<

.yrl.erl:
	echo "yecc:yecc($*,$*), halt()." | erl

MODS =  sw swTopLevel swBlinker swButton swCanvas swClock swColorButton \
        swColorText\
        swDragBox swEdText swEmacs swEntry \
        swFlashButton swLabel swLifts swProgressBar swRectangle \
        swScrollbar swSelector swText test1 emacs emacs1 lines map\
	swErlPoint fontSelector lib_emacs lifts lift_socket_server lift_socket_client  

# make      - build and run the stable demo
# make test - run new experimental tests

all:  ${MODS:%=%.beam} mapdata.bin

bang: erl_parse.beam

mapdata.bin: mapdata.dat map.beam
	echo "map:reformat(), erlang:halt(). " | erl

test:  ${MODS:%=%.beam}
	erl -pa '../lib' -s sw_tests start

lifts:  ${MODS:%=%.beam}
	erl -pa '../lib' -s lifts start

lift_socket_server: ${MODS:%=%.beam}
	erl -pa '../lib' -s lift_socket_server start

lift_socket_client: ${MODS:%=%.beam}
	erl -noshell -pa '../lib' -s lift_socket_client start

demo:  ${MODS:%=%.beam}
	erl -pa '../lib' -s demo start

clean:
	rm -f *~ *.beam erl_crash.dump erl_parse.erl
