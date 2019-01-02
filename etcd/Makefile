## set the paths for a default setup
all:
	rm -rf */*~ *.beam ebin/* *~ *.dump */*.beam;
	erlc -o ebin src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s etcd_lib boot -sname repo
test:
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s test_etcd test -sname test_etcd
