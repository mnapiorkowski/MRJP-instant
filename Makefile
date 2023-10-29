SRC_DIR=src
BUILD_DIR=build

.PHONY: clean

all: grammar jvm llvm

grammar: ${SRC_DIR}/Instant.cf
	cd ${SRC_DIR} && bnfc --haskell -m -d Instant.cf && make
	
jvm: ${SRC_DIR}/Compiler/JVM.hs
	ghc ${SRC_DIR}/Compiler/JVM.hs -package mtl -package dlist \
	-i${SRC_DIR}/Compiler -i${SRC_DIR} -outputdir ${BUILD_DIR} -o insc_jvm
	
llvm: ${SRC_DIR}/Compiler/LLVM.hs
	ghc ${SRC_DIR}/Compiler/LLVM.hs -package mtl -package dlist \
	-i${SRC_DIR}/Compiler -i${SRC_DIR} -outputdir ${BUILD_DIR} -o insc_llvm
	
clean:
	rm -rf ${SRC_DIR}/Instant ${SRC_DIR}/Makefile ${BUILD_DIR} insc_jvm insc_llvm
