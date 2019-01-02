## set the paths for a default setup
all:
	rm -rf */*~ *.beam ebin/* *~ *.dump */*.beam;
	erlc -o ebin src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s nfv_mgr_lib boot -sname nfv_mgr
test:
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s test_nfv_mgr test -sname test_nfv_mgr
