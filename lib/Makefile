CC = gcc
CFLAGS = -I/usr/local/X11/include

LIBS = -L/usr/X11R6/lib -lX11

## make
## make demo

## Note rgb.erl does compile but it takes a very long time???

.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

MODS = ex11_lib ex11_lib_driver ex11_lib_read_jpeg ex11_lib_connect\
       ex11_lib_control\
       ex11_lib_keyboard_driver ex11_lib_rgb ex11_lib_xauth ex11_lib_utils\
       hello_world hello_buttons ex11_lib_xauth

all:  ${MODS:%=%.beam} ex11_lib_read_jpeg

test: all
	erl -pa '../unixdom-0.1/ebin/' -s hello_world start

test1: all
	erl -pa '../unixdom-0.1/ebin/' -s hello_buttons start

ex11_lib_read_jpeg: ex11_lib_read_jpeg.o port_lib.o
	gcc -o ex11_lib_read_jpeg ex11_lib_read_jpeg.o port_lib.o -ljpeg

clean:
	rm -f *~ *.beam erl_crash.dump ex11_lib_read_jpeg 
	rm -f ex11_lib_read_jpeg.o port_lib.o















