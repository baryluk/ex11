
EMULATOR=beam
ERLDIR=/usr/local/lib/erlang


DRIVER_H_DIR = /usr/local/lib/erlang/usr/include

CC=gcc
CPPFLAGS= 
LDFLAGS= 
LIBS=
CFLAGS=-g -I$(ERLDIR)/usr/include -I$(ERLDIR)/usr/include/obsolete\
          -I$(DRIVER_H_DIR) -DNEED_EXTRA_FUNCS -Wall -pedantic

ERL=erl 
ERLC=erlc
ERL_FLAGS =
ERL_COMPILE_FLAGS = -W 

EBIN = ../ebin
ESRC = .

ERPCGEN = $(BAMS_ROOT)/rpc/priv/erpcgen
ERPCGEN_OPTS = -a '[all,trace]'

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 .fig .dvi .tex .class .java .pdf

$(EBIN)/%.jam: $(ESRC)/%.erl
	$(ERLC) -bjam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

.erl.jam:
	$(ERLC) -bjam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

.erl.beam:
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

MODULES= unixdom exp1 unixdom2

APPS =	unixdom_echosrv unixdom_cat tcp_echoserv
LIBS =	unixdom_drv.so exp1_drv.so

HRL_FILES= nfs_protocol.hrl bamscp.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(APPS) $(LIBS)

ERL_FLAGS = 


.SUFFIXES: .jam .beam .erl .so .c

all: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES)
	rm -f *.o *.so core *.core ktrace.out
	rm -f $(APPS)

../ebin/%.$(EMULATOR): %.erl $(HRL_FILES)

unixdom_echosrv: unixdom_echosrv.c
	$(CC) $(CFLAGS) -o unixdom_echosrv unixdom_echosrv.c `./Is-Solaris-link.sh`

unixdom_cat: unixdom_cat.c
	$(CC) $(CFLAGS) -o unixdom_cat unixdom_cat.c `./Is-Solaris-link.sh`

tcp_echoserv: tcp_echoserv.c
	$(CC) $(CFLAGS) -o tcp_echoserv tcp_echoserv.c `./Is-Solaris-link.sh`

.c.so:
	$(CC) $(CFLAGS) -fpic -shared -o $*.so $<









