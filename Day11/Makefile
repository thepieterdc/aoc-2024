all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day11.Advanced.main -o Day11/advanced Day11/Advanced.hs 1>&2

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day11.Simple.main -o Day11/simple Day11/Simple.hs 1>&2

dev-advanced:
	cd ../; ghcid Day11/Advanced.hs --setup ":set args Day11/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day11/Simple.hs --setup ":set args Day11/input/simple-example.txt" --test main

run-advanced: compile-advanced
	./advanced input/advanced.txt

run-simple: compile-simple
	./simple input/simple.txt

pretest:
	mkdir -p test/

profile-simple:

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
