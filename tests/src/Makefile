# 10/10/2015 #########################################################
#                                                                    #
# This Makefile generates test drivers for BLZPACK.                  #
#                                                                    #
######################################################################

######################
include ../../make.inc
######################

all: double single

ALLLIBS = ../../lib/$(BLZPACKLIB) $(SOLVER) $(LAPACKLIB) $(BLASLIB) 

double: ../ddrvsp1.x ../ddrvsp2.x ../ddrvsp3.x \
	../ddrvsp4.x ../ddrvgp1.x ../ddrvgp2.x \
	../ddrvgp3.x ../ddrvgp4.x ../ddrvgp5.x \
	../ddrvgp6.x ../ddrvgp7.x

single: ../sdrvsp1.x ../sdrvsp2.x ../sdrvsp3.x \
	../sdrvsp4.x ../sdrvgp1.x ../sdrvgp2.x \
	../sdrvgp3.x ../sdrvgp4.x ../sdrvgp5.x \
	../sdrvgp6.x ../sdrvgp7.x

ddrvsp1_mpi: ../ddrvsp1_mpi.x
ddrv_debug: ../ddrv_debug.x
 
../ddrvsp1.x: ddrvsp1.o
	$(FC) $(FCLOAD) ddrvsp1.o -o $@ $(ALLLIBS)

../ddrvsp2.x: ddrvsp2.o
	$(FC) $(FCLOAD) ddrvsp2.o -o $@ $(ALLLIBS)

../ddrvsp3.x: ddrvsp3.o
	$(FC) $(FCLOAD) ddrvsp3.o -o $@ $(ALLLIBS)

../ddrvsp4.x: ddrvsp4.o
	$(FC) $(FCLOAD) ddrvsp4.o -o $@ $(ALLLIBS)

../ddrvgp1.x: ddrvgp1.o
	$(FC) $(FCLOAD) ddrvgp1.o -o $@ $(ALLLIBS)

../ddrvgp2.x: ddrvgp2.o
	$(FC) $(FCLOAD) ddrvgp2.o -o $@ $(ALLLIBS)

../ddrvgp3.x: ddrvgp3.o
	$(FC) $(FCLOAD) ddrvgp3.o -o $@ $(ALLLIBS)

../ddrvgp4.x: ddrvgp4.o
	$(FC) $(FCLOAD) ddrvgp4.o -o $@ $(ALLLIBS)

../ddrvgp5.x: ddrvgp5.o
	$(FC) $(FCLOAD) ddrvgp5.o -o $@ $(ALLLIBS)

../ddrvgp6.x: ddrvgp6.o
	$(FC) $(FCLOAD) ddrvgp6.o -o $@ $(ALLLIBS)

../ddrvgp7.x: ddrvgp7.o
	$(FC) $(FCLOAD) ddrvgp7.o -o $@ $(ALLLIBS)

../sdrvsp1.x: sdrvsp1.o
	$(FC) $(FCLOAD) sdrvsp1.o -o $@ $(ALLLIBS)

../sdrvsp2.x: sdrvsp2.o
	$(FC) $(FCLOAD) sdrvsp2.o -o $@ $(ALLLIBS)

../sdrvsp3.x: sdrvsp3.o
	$(FC) $(FCLOAD) sdrvsp3.o -o $@ $(ALLLIBS)

../sdrvsp4.x: sdrvsp4.o
	$(FC) $(FCLOAD) sdrvsp4.o -o $@ $(ALLLIBS)

../sdrvgp1.x: sdrvgp1.o
	$(FC) $(FCLOAD) sdrvgp1.o -o $@ $(ALLLIBS)

../sdrvgp2.x: sdrvgp2.o
	$(FC) $(FCLOAD) sdrvgp2.o -o $@ $(ALLLIBS)

../sdrvgp3.x: sdrvgp3.o
	$(FC) $(FCLOAD) sdrvgp3.o -o $@ $(ALLLIBS)

../sdrvgp4.x: sdrvgp4.o
	$(FC) $(FCLOAD) sdrvgp4.o -o $@ $(ALLLIBS)

../sdrvgp5.x: sdrvgp5.o
	$(FC) $(FCLOAD) sdrvgp5.o -o $@ $(ALLLIBS)

../sdrvgp6.x: sdrvgp6.o
	$(FC) $(FCLOAD) sdrvgp6.o -o $@ $(ALLLIBS)

../sdrvgp7.x: sdrvgp7.o
	$(FC) $(FCLOAD) sdrvgp7.o -o $@ $(ALLLIBS)

######################################################################
../ddrvsp1_mpi.x: ddrvsp1_mpi.o
	$(FC) $(FCLOAD) ddrvsp1_mpi.o -o $@ $(ALLLIBS)
######################################################################
../ddrv_debug.x: ddrv_debug.o
	$(FC) $(FCLOAD) ddrv_debug.o -o $@ $(ALLLIBS)
######################################################################

clean:
	rm -f *.o *.x

.f.o:
	$(FC) $(FCOPT) $(FCFIXDF) -c -o $@ $<

# end of Makefile ####################################################
