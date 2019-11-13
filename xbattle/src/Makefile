BEAMS = $(shell ls *.erl | sed -e 's|\.erl$$|.beam|')


.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W -I ../../widgets -I ../../lib -o ../ebin $<

.PHONY: all build run

all: build
	echo $(BEAMS)

build: $(BEAMS)

run:
	erl -pa ../ebin -pa ../../widgets -pa ../../lib -s xbattle
