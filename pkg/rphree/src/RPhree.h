/// Rphree library: interface R/PHREEQC (Parkhurst & Appelo)
/// 
/// Marco De Lucia, delucia@gfz-potsdam.de, 2009-2014
/// Time-stamp: "Last modified 2014-01-29 16:43:41 delucia"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Utils.h> // for R_CheckUserInterrupt

#include "global.h"
#include "output.h"
#include "phrqproto.h"
#include "input.h"
#include <stdlib.h>
#include "phqalloc.h"

SEXP R_sol_descr(void);
SEXP R_sol_species (void);
SEXP R_sol_pphases (void);
SEXP R_sol_SI (void);
SEXP R_sol_kin (void);
SEXP R_sol_totals (void);
SEXP R_sol_punch (void);

SEXP R_do_out (void);
SEXP R_return_err(int errors, char * tok);
SEXP R_do_saveSim (void);
SEXP R_run_simulations (PFN_READ_CALLBACK pfn, void *cookie);


// void R_punch (void);
//void R_punch_all (void);
// void Rcleanup (void);
