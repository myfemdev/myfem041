/*
 *
 *  This file is part of MUMPS 4.9.2, built on Thu Nov  5 07:05:08 UTC 2009
 *
 *
 *  This version of MUMPS is provided to you free of charge. It is public
 *  domain, based on public domain software developed during the Esprit IV
 *  European project PARASOL (1996-1999) by CERFACS, ENSEEIHT-IRIT and RAL.
 *  Since this first public domain version in 1999, the developments are
 *  supported by the following institutions: CERFACS, CNRS, INPT(ENSEEIHT)-
 *  IRIT, and INRIA.
 *
 *  Current development team includes Patrick Amestoy, Alfredo Buttari,
 *  Abdou Guermouche, Jean-Yves L'Excellent, Bora Ucar.
 *
 *  Up-to-date copies of the MUMPS package can be obtained
 *  from the Web pages:
 *  http://mumps.enseeiht.fr/  or  http://graal.ens-lyon.fr/MUMPS
 *
 *
 *   THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
 *   EXPRESSED OR IMPLIED. ANY USE IS AT YOUR OWN RISK.
 *
 *
 *  User documentation of any code that uses this software can
 *  include this complete notice. You can acknowledge (using
 *  references [1] and [2]) the contribution of this package
 *  in any scientific publication dependent upon the use of the
 *  package. You shall use reasonable endeavours to notify
 *  the authors of the package of this publication.
 *
 *   [1] P. R. Amestoy, I. S. Duff, J. Koster and  J.-Y. L'Excellent,
 *   A fully asynchronous multifrontal solver using distributed dynamic
 *   scheduling, SIAM Journal of Matrix Analysis and Applications,
 *   Vol 23, No 1, pp 15-41 (2001).
 *
 *   [2] P. R. Amestoy and A. Guermouche and J.-Y. L'Excellent and
 *   S. Pralet, Hybrid scheduling for the parallel solution of linear
 *   systems. Parallel Computing Vol 32 (2), pp 136-156 (2006).
 *
 */

#ifndef MUMPS_MPI_H
#define MUMPS_MPI_H

/* We define all symbols as extern "C" for users who call MUMPS with its
   libseq from a C++ driver. */
#ifdef __cplusplus
extern "C" {
#endif

/* This is the minimum to have the C interface of MUMPS work.
 * Most of the time, users who need this file have no call to MPI functions in
 * their own code. Hence it is not worth declaring all MPI functions here.
 * However if some users come to request some more stub functions of the MPI
 * standards, we may add them. But it is not worth doing it until then. */

typedef int MPI_Comm; /* Simple type for MPI communicator */
int MPI_COMM_WORLD;

int MPI_Init(int *pargc, char ***pargv);
int MPI_Comm_rank(int  comm, int  *rank);
int MPI_Finalize(void);

#ifdef __cplusplus
}
#endif

#endif /* MUMPS_MPI_H */
