CC = gcc
CFLAGS = -I/usr/local/X11/include

LIBS = -L/usr/X11R6/lib -lX11

## make
## make demo

## Note rgb.erl does compile but it takes a very long time???

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	echo "yecc:yecc($*,$*), halt()." | erl

MODS =  sw swTopLevel swBlinker swButton swCanvas swClock swColorButton \
        swColorText\
        swDragBox swEdText swEmacs swEntry \
        swFlashButton swLabel swLifts swProgressBar swRectangle \
        swScrollbar swSelector swText test1 emacs emacs1 lines map\
        example0 example1 example2 example3 example4 example5 example6\
	example7 example8 example9 example10 example11 example12 example13\
        example14\
	example15 example16 example17 example18 example19 example20\
	example21 example22 example23 example24 example25 example26 example27\
        example28 example29 example30 example31 example32\
	swErlPoint fontSelector lib_emacs lifts lift_socket_server lift_socket_client  

# make      - build and run the stable demo 
# make test - run new experimental tests

all:  ${MODS:%=%.beam} mapdata.bin
	erl -pa '../lib' -pa '../unixdom-0.1/ebin/' -s example0 start

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









