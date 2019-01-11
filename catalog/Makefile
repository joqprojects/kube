## set the paths for a default setup

test:
	rm -rf */*~ *.beam *~ *.dump;
	rm -rf test_ebin/* ebin/*;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erlc -o ebin ../common/src/*.erl;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin -pa service_ebin -s test_repo test -sname test_repo
