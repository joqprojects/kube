## set the paths for a default setup

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump


devops:
	rm -rf ebin/* */*~ src/*.beam test_src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl test_src/*.erl;
	erl -pa ebin -pa ../../repo_mgr/ebin -pa ../cmn/ebin -pa template/ebin -s test_devops test -sname test_devops


repo_test:
	rm -rf ebin/* */*~ src/*.beam test_src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl test_src/*.erl;
	erl -pa ebin -s test_repo_cmn test -sname test_repo_cmn

test:
	rm -rf ebin/* */*~ src/*.beam test_src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl test_src/*.erl;
	erl -pa ebin -s test_common test -sname test_common
