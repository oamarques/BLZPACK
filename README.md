# BLZPACK
BLZPACK (for Block LancZos PACKage) is a set of subprograms written
in standard Fortran intended for the computation of eigenvalues
eig and eigenvectors (x) of the "standard" eigenvalue problem

          (A)*(x) - eig*(x) = (0)

or of the "generalized" eigenvalue problem

          (A)*(x) - eig*(B)*(x) = (0)

where (A) and (B) are N x N real, sparse, symmetric matrices, eig an
eigenvalue, and (x) an eigenvector. It is assumed that there exists 
a linear combination of (A) and (B) which is positive definite to
guarantee that all eigenvalues are real.
