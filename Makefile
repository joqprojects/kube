## set the paths for a default setup
dbase:
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf test/*/nfvi/*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
#	erlc -o test/test_ebin kube/common/test_src/*.erl;
#	erlc -o ebin/etcd/ebin kube/etcd/src/*.erl;
#	erlc -o test/test_ebin kube/etcd/test_src/*.erl;
#	cp kube/etcd/src/*.app ebin/etcd/ebin;
#	cp ebin/common/ebin/* ebin/etcd/ebin;
	erl -pa test/test_ebin -pa ebin/etcd/ebin -s etcd_lib boot -sname etcd -setcookie glurk

clean:
	rm -rf ebin/*/ebin/*; 
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test/*/nfvi/*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
build:
	rm -rf ebin/*/ebin/*; 
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test/*/nfvi/*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
#common
	erlc -o ebin/common/ebin services/common/src/*.erl;
	erlc -o test/test_ebin services/common/test_src/*.erl;
# dns
	erlc -o ebin/dns/ebin services/dns/src/*.erl;
#	erlc -o test/test_ebin services/dns/test_src/*.erl;
	cp services/dns/src/*.app ebin/dns/ebin;
	cp ebin/common/ebin/* ebin/dns/ebin;
# log
	erlc -o ebin/log/ebin services/log/src/*.erl;
#	erlc -o test/test_ebin services/log/test_src/*.erl;
	cp services/log/src/*.app ebin/log/ebin;
	cp ebin/common/ebin/* ebin/log/ebin;
# nfv_mgr
	erlc -o ebin/nfv_mgr/ebin services/nfv_mgr/src/*.erl;
#	erlc -o test/test_ebin services/nfv_mgr/test_src/*.erl;
	cp services/nfv_mgr/src/*.app ebin/nfv_mgr/ebin;
	cp ebin/common/ebin/* ebin/nfv_mgr/ebin;
# repo
	erlc -o ebin/repo/ebin services/repo/src/*.erl;
	erlc -o test/test_ebin services/repo/test_src/*.erl;
	cp services/repo/src/*.app ebin/repo/ebin;
	cp ebin/common/ebin/* ebin/repo/ebin;
#vim
	erlc -o ebin/vim/ebin services/vim/src/*.erl;
#	erlc -o test/test_ebin services/vim/test_src/*.erl;
	cp services/vim/src/*.app ebin/vim/ebin;
	cp ebin/common/ebin/* ebin/vim/ebin;
#oam
	erlc -o ebin/oam/ebin services/oam/src/*.erl;
	erlc -o test/test_ebin services/oam/test_src/*.erl;
	cp services/oam/src/*.app ebin/oam/ebin;
	cp ebin/common/ebin/* ebin/oam/ebin;
# Services for ebin
	erlc -o ebin/adder/ebin services/adder/src/*.erl;
	cp services/adder/src/*.app ebin/adder/ebin;
	cp services/adder/src/*.josca ebin/adder/ebin;
	cp services/adder/src/* ebin/adder/ebin;
	cp ebin/common/ebin/* ebin/adder/ebin;
#
	erlc -o ebin/subtract/ebin services/subtract/src/*.erl;
	cp services/subtract/src/*.app ebin/subtract/ebin;
	cp services/subtract/src/*.josca ebin/subtract/ebin;
	cp services/subtract/src/* ebin/subtract/ebin;
	cp ebin/common/ebin/* ebin/subtract/ebin;
#
	erlc -o ebin/divider/ebin services/divider/src/*.erl;
	cp services/divider/src/*.app ebin/divider/ebin;
	cp services/divider/src/*.josca ebin/divider/ebin;
	cp services/divider/src/* ebin/divider/ebin;
	cp ebin/common/ebin/* ebin/divider/ebin;
#
	erlc -o ebin/calc/ebin services/calc/src/*.erl;
	cp services/calc/src/*.app ebin/calc/ebin;
	cp services/calc/src/*.josca ebin/calc/ebin;
	cp services/calc/src/* ebin/calc/ebin;
	cp ebin/common/ebin/* ebin/calc/ebin;
# Workers for test
	erlc -o test/w1/nfvi/ebin services/nfvi/src/*.erl;
	cp services/nfvi/src/*.app test/w1/nfvi/ebin;
	cp ebin/common/ebin/* test/w1/nfvi/ebin;
	cp test/w1/nfvi/ebin/* test/w2/nfvi/ebin;
	cp test/w1/nfvi/ebin/* test/w3/nfvi/ebin;
# master worker
	cp test/w1/nfvi/ebin/* test/w_master/nfvi/ebin;
	cp ebin/dns/ebin/* test/w_master/nfvi/ebin;
	cp ebin/repo/ebin/* test/w_master/nfvi/ebin;
	cp ebin/vim/ebin/* test/w_master/nfvi/ebin;
	cp ebin/nfv_mgr/ebin/* test/w_master/nfvi/ebin;
	cp ebin/log/ebin/* test/w_master/nfvi/ebin;
	echo ++++++++++++       build_succeded    +++++++++++++++++++++++++++
oam:
	erl -pa ebin/oam/ebin -s oam_lib boot -sname oam
test_oam:
	cp ebin/common/ebin/* test/test_ebin;
	erl -pa test/test_ebin -sname test_oam;
test_dns:
	rm -rf dns/ebin/* ; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf test/*/nfvi/*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
