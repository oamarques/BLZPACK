FC      = mpxlf90_r
FCOPT   = -O3 -u -qfixed -qstrict
FCNOPT  = -u -qfixed
FCINCL  = -I../ -qmoddir=/scr -I/scr -IMUMPS_4.2_beta/include
FCLOAD  =
PARLIB  =
RANLIB  = ranlib 

#BLAS    = -lblas
EIGTB   = tbcopy.1.f tbtrid.1.f
#LAPACK  = 
SOLVER  = MUMPS_4.2_beta/lib/libdmumps.a MUMPS_4.2_beta/lib/libpord.a \
	$(METIS) -lmetis -lpessl -lblacs -lessl
