BEAMS = $(shell ls *.erl | sed -e 's|\.erl$$|.beam|')

ERLC_FLAGS = -W

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc $(ERLC_FLAGS) -I ../../widgets -I ../../lib -o ../ebin $<

.PHONY: all build run debug erl

all: build
	echo $(BEAMS)

build: $(BEAMS)

run:
	erl -pa ../ebin -pa ../../widgets -pa ../../lib -s xbattle

erl:
	erl -pa ../ebin -pa ../../widgets -pa ../../lib

debug:
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -Ddebug"
	erl -noshell -pa ../ebin -pa ../../widgets -pa ../../lib -s xbattle test -s init stop
