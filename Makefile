
.PHONY: all
all: compile

.PHONY: compile
compile:
	mkdir -p build
	cmake -S . -B build
	cmake --build build

.PHONY: test
test:
	./runtests

.PHONY: clean
clean:
	rm -rf build samples/*.ir samples/*.s samples/*.out 2>/dev/null

