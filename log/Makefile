## set the paths for a default setup
all:
	rm -rf */*~ *.beam ebin/* *~ *.dump */*.beam;
	erlc -o ebin src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s log_lib boot -sname log
test:
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s test_log test -sname test_log
