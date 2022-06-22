.PHONY: build clean 

build: bin/queryClinVar-exe

test: bin/queryClinVar-exe
	output_file="test_run_annotated.csv" && \
	./bin/queryClinVar-exe example_popseq_file.csv "$${output_file}" && \
	md5sum -c test_run.md5sum && \
	rm -f "$${output_file}" && \
	echo "Test passed"

clean:
	rm -rf  bin build 


bin/queryClinVar-exe: SHELL:=/bin/bash -l
bin/queryClinVar-exe:
	module load gcc/11.2.0 && \
	  module load haskell/9.2.3 && \
		mkdir -p build bin &&\
		stack --stack-root "$${PWD}/build" build  &&\
		stack --stack-root "$${PWD}/build" install --local-bin-path "$${PWD}/bin"
