########################################################################
# Options for Linux and ifort compiler                              #
########################################################################

# The machine (platform) identifier appended to the library names ######

PLAT    = gfortran

# Fortran compiler #####################################################

FC      = gfortran
FCOPT   = -O2 -mieee-fp -fimplicit-none -Wunused
FCFIXDF = 
FCFREEF = -ffree-form
FCLDOPT = -O2
FCNOOPT = -O0

# BLZPACK library ######################################################

BLZPACKLIB = libblzpack_$(PLAT).a

# VERSION sets the version, sequential (seq) or parallel (mpi)
VERSION = seq
# TRDEIG sets the tridiagonal eigensolver: imql or LAPACK's stegr
TRDEIG = stegr
# STIMER sets the timer
STIMER = etime
# SRANDN sets the random number generator
SRANDN = rand

# MA47 is not distributed with BLZPACK

SOLVER = $(HOME)/LIB/HSL/MA47AD_$(PLAT).o $(HOME)/LIB/HSL/MA47A_$(PLAT).o

# Archiver and flag(s) to use when building archive (library) ##########

ARCH = ar
ARCHFLAGS = cr
RANLIB = touch

# LAPACK and BLAS libraries to be linked to ############################

LAPACKDIR = $(HOME)/LIB/LAPACK/3.5.0
LAPACKLIB = $(LAPACKDIR)/liblapack_$(PLAT).a
BLASLIB   = $(LAPACKDIR)/BLAS/libblas_$(PLAT).a
