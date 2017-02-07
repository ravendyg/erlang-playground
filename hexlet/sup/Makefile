test: build
	erl -s test run

build: clean
	erlc *.erl

clean:
	-rm *.beam 2> /dev/null || echo > /dev/null