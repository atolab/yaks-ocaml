.PHONY: lib examples all clean test doc

EXES=examples/client/simple.exe examples/throughput/ythrp.exe examples/throughput/ythr_put.exe   examples/throughput/ythr_sub.exe examples/latency/ylatp.exe  examples/latency/ylat_ping.exe  examples/latency/ylat_pong.exe

BUILD_LIB=dune build	
BUILD_EXAMPLES=dune build ${EXES}

CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer
DOC=dune build --dev @doc
INSTALL=dune install


all:	
	make lib	
	make examples


lib:
	${BUILD_LIB}		

examples:	
	${BUILD_EXAMPLES}

	
test:
	${TEST}

doc:
	${DOC}

install:
	${INSTALL}

clean:
	${CLEAN}
