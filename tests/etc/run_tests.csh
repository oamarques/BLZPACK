#!/bin/csh
########################################################################
#                                                                      #
# This script runs tests in double and single precisions.              #
#                                                                      #
########################################################################

echo "* running tests in double precision"

ddrvgp1.x < drvgp1.dat >! ddrvgp1.out
ddrvgp2.x < drvgp2.dat >! ddrvgp2.out
ddrvgp3.x < drvgp3.dat >! ddrvgp3.out
ddrvgp4.x < drvgp4.dat >! ddrvgp4.out

ddrvsp1.x < drvsp1.dat >! ddrvsp1.out
ddrvsp2.x < drvsp2.dat >! ddrvsp2.out
ddrvsp3.x < drvsp3.dat >! ddrvsp3.out
ddrvsp4.x < drvsp4.dat >! ddrvsp4.out

echo "* running tests in single precision"

sdrvgp1.x < drvgp1.dat >! sdrvgp1.out
sdrvgp2.x < drvgp2.dat >! sdrvgp2.out
sdrvgp3.x < drvgp3.dat >! sdrvgp3.out
sdrvgp4.x < drvgp4.dat >! sdrvgp4.out

sdrvsp1.x < drvsp1.dat >! sdrvsp1.out
sdrvsp2.x < drvsp2.dat >! sdrvsp2.out
sdrvsp3.x < drvsp3.dat >! sdrvsp3.out
sdrvsp4.x < drvsp4.dat >! sdrvsp4.out

echo "* end of tests"
