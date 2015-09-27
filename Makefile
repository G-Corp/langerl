all: compile

compile:
	mkdir -p build
	cd build ; cmake .. ; make

clean:
	cd build ; make clean
	rm ebin/*.beam

distclean:
	rm -rf priv
	rm -rf ebin
	rm -rf build

dev: compile
	@erl -name test -pa ebin include deps/*/ebin deps/*/include -config config/langerl.config

