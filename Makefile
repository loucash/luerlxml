PROJECT := luerlxml

ERL := erl
EPATH = -pa ebin -pz deps/*/ebin
TEST_EPATH = -pa .eunit -pz deps/*/ebin
PLT_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')
DIALYZER_OPTS= -Wno_undefined_callbacks --fullpath

.PHONY: all build_plt build-jenkins compile configure console deps doc clean depclean distclean dialyze release telstart test test-console test-erl test-lua

all:
	@rebar skip_deps=true compile

build-jenkins: nuke deps compile test

build_plt:
	@dialyzer --build_plt --apps $(PLT_APPS)

compile:
	@rebar compile

configure:
	@rebar get-deps compile

console:
	$(ERL) -sname $(PROJECT) $(EPATH)

deps:
	@rebar get-deps update-deps

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

distclean:
	@rebar clean delete-deps
	@rm -rf logs

nuke: distclean
	@git clean -f -d -x

dialyze:
	@dialyzer $(DIALYZER_OPTS) -r ebin

start:
	$(ERL) -sname $(PROJECT) $(EPATH) -s $(PROJECT)

test: test-erl test-lua

test-erl:
	@rebar skip_deps=true ct verbose=1

test-lua:
	tsc --luacov -f priv/lua/test/test_*.lua
	luacov

test-console: test
	$(ERL) -sname $(PROJECT)_test $(TEST_EPATH)
