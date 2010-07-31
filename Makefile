all:
	cd lib; make
	cd unixdom-0.1/src; make
	cd widgets; make

clean:
	cd lib; make clean
	cd widgets; make clean
	cd unixdom-0.1/src; make clean

