test: build
	erl -pa ebin -s test run

build:
	mkdir -p ebin
	erlc -o ebin src/*.erl test/*.erl

clean:
	-rm ebin/*.beam 2> /dev/null || echo > /dev/null