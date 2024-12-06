all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day6.Advanced.main -o Day6/advanced Day6/Advanced.hs 1>&2

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day6.Simple.main -o Day6/simple Day6/Simple.hs 1>&2

dev-advanced:
	cd ../; ghcid Day6/Advanced.hs --setup ":set args Day6/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day6/Simple.hs --setup ":set args Day6/input/simple-example.txt" --test main

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
