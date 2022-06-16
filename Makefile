.PHONY: download_stack build clean 

build: bin/queryClinVar-exe

clean:
	rm -rf linux-x86_64.tar.gz bin build stack*linux-x86_64

linux-x86_64.tar.gz:
	wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz


bin/queryClinVar-exe: SHELL:=/bin/bash -l
bin/queryClinVar-exe: linux-x86_64.tar.gz
	tar xvzf linux-x86_64.tar.gz && \
	rm -rf linux-x86_64.tar.gz && \
	module load gcc/11.2.0 && \
		stack_cmd="stack*/stack" &&\
		mkdir -p build bin &&\
		$${stack_cmd} --stack-root "$${PWD}/build" build  &&\
		$${stack_cmd} --stack-root "$${PWD}/build" install --local-bin-path "$${PWD}/bin" &&\
		rm -rf build  stack* 
