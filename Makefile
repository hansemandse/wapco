# Environment variables
SBT := sbt

.PHONY : all
all : adders vivado

adders :
	$(SBT) "runMain Wapco"

vivado :
	./genTcl.sh

.PHONY : clean
clean :
	$(SBT) clean
	-rm -rf build
	-git clean -fd

.PHONY : test
test :
	$(SBT) test
