###########################################################################
#                                                                         #
# Usage:                                                                  #
#                                                                         #
#    make [target]                                                        #
#                                                                         #
# where target can be                                                     #
#                                                                         #
#    all: lib                                                             #
#    exe: driver samples in tests                                         #
#    lib: blzpacklib                                                      #
#                                                                         #
###########################################################################

################
include make.inc
################

.SUFFIXES:
.SUFFIXES: .f

all: lib
lib: blzpacklib

# Target blzpacklib ##################################################

blzpacklib: 
	(cd src; $(MAKE) )

# Target exe #########################################################

exe:
	(cd tests/src; $(MAKE) )

###########################################################################

cleanall: clean cleanlib

clean: 
	-@rm -f src/*.o src/aux/*.o
	-@rm -f tests/*.out tests/*.x tests/src/*.o 

cleanlib:
	-@rm -f *.a
