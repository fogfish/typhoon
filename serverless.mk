##
## Copyright (C) 2018 Dmitry Kolesnikov
##
## This Makefile may be modified and distributed under the terms
## of the MIT license.  See the LICENSE file for details.
## https://github.com/fogfish/makefile
##
## @doc
##   This makefile is the wrapper of rebar to build serverless applications
##
## @version 0.2.0
.PHONY: all compile test dist distclean cloud-init cloud-patch cloud

APP    := $(strip $(APP))
VSN    ?= $(shell test -z "`git status --porcelain`" && git describe --tags --long | sed -e 's/-g[0-9a-f]*//' | sed -e 's/-0//' || echo "`git describe --abbrev=0 --tags`-dev")
TEST   ?= tests
REBAR  ?= 3.5.0
REL    ?= ${APP}-${VSN}.zip
DOCKER  = fogfish/erlang-serverless:20.3

## erlang runtime configration flags
ROOT   = $(shell pwd)
ADDR   = localhost.localdomain
EFLAGS = \
	-name ${APP}@${ADDR} \
	-setcookie ${COOKIE} \
	-pa ${ROOT}/_build/default/lib/*/ebin \
	-pa ${ROOT}/_build/default/lib/*/priv \
	-pa ${ROOT}/rel \
	-kernel inet_dist_listen_min 32100 \
	-kernel inet_dist_listen_max 32199 \
	+P 1000000 \
	+K true +A 160 -sbt ts

## erlang common test bootstrap
BOOT_CT = \
   -module(test). \
   -export([run/1]). \
   run(Spec) -> \
      {ok, Test} = file:consult(Spec), \
      case lists:keymember(node, 1, Test) of \
         false -> \
            erlang:halt(element(2, ct:run_test([{spec, Spec}]))); \
         true  -> \
            ct_master:run(Spec), \
            erlang:halt(0) \
      end.

BUILDER = FROM ${DOCKER}\nARG VERSION=\nCOPY _build/default/bin/${APP} /var/task/\nRUN cd /var/task && sed -i -e \"s/APP/${APP}/\" index.js && zip ${APP}-\x24{VERSION}.zip -r * > /dev/null


#####################################################################
##
## build
##
#####################################################################
all: rebar3 compile test

compile: rebar3
	@./rebar3 compile

run: 
	@erl ${EFLAGS}

##
## execute common test and terminate node
test: _build/test.beam
	@mkdir -p /tmp/test/${APP}
	@erl ${EFLAGS} -noshell -pa _build/ -pa test/ -run test run test/${TEST}.config
	@F=`ls /tmp/test/${APP}/ct_run*/all.coverdata | tail -n 1` ;\
	cp $$F /tmp/test/${APP}/ct.coverdata

_build/test.beam: _build/test.erl
	@erlc -o _build $<

_build/test.erl:
	@mkdir -p _build && echo "${BOOT_CT}" > $@

testclean:
	@rm -f  _build/test.beam
	@rm -f  _build/test.erl
	@rm -f  test/*.beam
	@rm -rf test.*-temp-data
	@rm -rf tests

##
## clean 
clean: testclean
	-@./rebar3 clean
	@rm -Rf _build/builder
	@rm -rf log
	@rm -f  *.tar.gz
	@rm -f  *.zip
	@rm -f  *.bundle
	-@rm -Rf _build/default/bin

##
##
dist: ${REL}

${REL}: _build/builder _build/default/bin/${APP}
	docker build --file=$< --force-rm=true --build-arg="VERSION=${VSN}" --tag=build/${APP}:latest . ;\
	I=`docker create build/${APP}:latest` ;\
	docker cp $$I:/var/task/$@ $@ ;\
	docker rm -f $$I ;\
	docker rmi build/${APP}:latest ;\
	test -f $@ && echo "==> tarball: $@"

_build/default/bin/${APP}: all
	@./rebar3 escriptize

_build/builder:
	@mkdir -p _build && echo "${BUILDER}" > $@

##
##
distclean: clean
	-@rm -Rf _build
	-@rm rebar3


function:
	I=`docker create ${DOCKER}` ;\
	docker cp $$I:/function/src . ;\
	docker cp $$I:/function/test . ;\
	docker cp $$I:/function/rebar.config . ;\
	docker rm -f $$I ;\
	sed -i '' -e "s/APP/${APP}/" src/* ;\
	sed -i '' -e "s/APP/${APP}/" test/* ;\
	sed -i '' -e "s/APP/${APP}/g" rebar.config ;\
	mv src/app.app.src src/${APP}.app.src ;\
	mv src/app.erl src/${APP}.erl


#####################################################################
##
## deploy
##
#####################################################################

cloud-init:
	@aws lambda create-function \
		--function-name ${ENV}-${APP} \
		--runtime nodejs8.10 \
		--handler index.handler \
		--role ${ROLE} \
		--timeout ${TIMEOUT} \
		--memory-size ${MEMORY} \
		--publish \
		--zip-file fileb://./${REL} \
		$$R

cloud-patch:
	@aws lambda update-function-code \
	   --function-name ${ENV}-${APP} \
	   --publish \
	   --zip-file fileb://./${REL}

cloud:
	@aws lambda get-function --function-name ${ENV}-${APP} > /dev/null && ${MAKE} cloud-patch || ${MAKE} cloud-init


#####################################################################
##
## dependencies
##
#####################################################################
rebar3:
	@echo "==> install rebar (${REBAR})" ;\
	curl -L -O -s https://github.com/erlang/rebar3/releases/download/${REBAR}/rebar3 ;\
	chmod +x $@
