## set the paths for a default setup
all:
	rm -rf */*~ *.beam ebin/* *~ *.dump */*.beam;
	erlc -o ebin src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
#	cp src/*.josca ebin;
	erl -pa ebin -s oam_lib boot -sname oam
test:
	rm -rf */*~ *.beam ebin/* *~ *.dump */*.beam;
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -sname test_oam
