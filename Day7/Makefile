all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day7.Advanced.main -o Day7/advanced Day7/Advanced.hs 1>&2

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day7.Simple.main -o Day7/simple Day7/Simple.hs 1>&2

dev-advanced:
	cd ../; ghcid Day7/Advanced.hs --setup ":set args Day7/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day7/Simple.hs --setup ":set args Day7/input/simple-example.txt" --test main

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
