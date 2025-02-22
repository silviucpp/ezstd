REBAR=rebar3

get_deps:
	@./build_deps.sh

compile_nif: get_deps
	@make V=0 -C c_src -j 8

clean_nif:
	@make -C c_src clean

clean_zstd: clean_nif
	@make -C _build/deps/zstd clean
	@cd _build/deps/zstd && git reset --hard

compile:
	${REBAR} compile

clean:
	${REBAR} clean
