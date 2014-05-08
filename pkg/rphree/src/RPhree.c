/// Rphree library: interface R/PHREEQC (Parkhurst & Appelo)
/// 
/// Marco De Lucia, delucia@gfz-potsdam.de, 2009-2014
/// Time-stamp: "Last modified 2014-01-29 16:43:47 delucia"

#define EXTERNAL
#include "RPhree.h"

/* ----------------------------------------------------------------------
 *   MAIN
 * ---------------------------------------------------------------------- */

SEXP
RPhreemain (SEXP Rargv, SEXP Rinp_length, SEXP Rinput, SEXP Rdatabase, SEXP Routput_sel)
/*
 *   Main program for PHREEQC
 */
{
  int errors, i, ls;
  int argc = 4; // MDL: ugly: argc is 4, the real dimension of argv is 6 or more...
  char *argv[6] ; // {"RPhree", "FIXME", "mysol1.Rout","phreeqc.dat","T | F (write)","T | F (verb.)")};
  void *db_cookie = NULL;
  void *input_cookie = NULL;
  char ** Rinp;
  char ** Rdb;

  SEXP Rout;

  // MDL: not defined on windows
  //#ifndef DOS
  // MDL: SVN_REV is defined in the Makefile
  // Rprintf(":: Rphree - linux rev %s :: \n", SVN_REV); 
  //  Rprintf(":: SVN id: %s :: \n",svnid);
  //#endif

  R_punch = TRUE;
  // MDL: selection of output
  PROTECT(Routput_sel = AS_INTEGER(Routput_sel));
  Routdim = INTEGER(Routput_sel)[0];

  // MDL: punch
  R_do_punch = FALSE;

  for (i=0; i < 7; i++) {
    selout[i] = INTEGER(Routput_sel)[i+1];
  }

  UNPROTECT(1);

  if (selout[6])
    R_punch = TRUE;
  else
    R_punch = FALSE;

  // MDL: length of input; number of simulations; length of punch vector
  PROTECT(Rinp_length = AS_INTEGER(Rinp_length));
  Rinput_length = INTEGER(Rinp_length)[0];
  Rnsim = INTEGER(Rinp_length)[1];
  Rpunch_dim = INTEGER(Rinp_length)[2];
  Rdb_length = INTEGER(Rinp_length)[3];
  UNPROTECT(1);

  // MDL: alloc the char ** array from R and copy the arguments in it
  PROTECT(Rargv = AS_CHARACTER(Rargv));
  for (i=0; i<6; i++) {
    ls = strlen(CHAR(STRING_ELT(Rargv,i)));
    argv[i] = R_alloc(1+ls,sizeof(char));
    strcpy( argv[i], CHAR(STRING_ELT(Rargv,i)));
    argv[i][ls] = '\0';
  }
  UNPROTECT(1);

  // MDL: should Rphreemain speak on stdout?
  char * dum = "T";
  R_verbose = FALSE;
  if (strncmp(argv[5],dum, 1)==0)
    R_verbose = TRUE;

  if (R_verbose == TRUE)
    Rprintf(":: Rphree called with %i simulations \n", Rnsim);


  // MDL: alloc the char ** for the input and copy everything in it
  PROTECT(Rinput = AS_CHARACTER(Rinput));
  Rinp  = (char **) R_alloc (Rinput_length, sizeof(char *));
  for (i=0; i<Rinput_length; i++) 
    {
      ls = strlen(CHAR(STRING_ELT(Rinput,i)));
      Rinp[i] = R_alloc ((1+ls),sizeof(char));
      strcpy( Rinp[i], CHAR(STRING_ELT(Rinput,i)));
      Rinp[i][ls]='\0';
      //      Rprintf("%3i %s\n",i,Rinp[i]);
    }
  UNPROTECT(1);

  // MDL: old implementation
/*   char *Rinp[Rinput_length] ; */

/*   PROTECT(Rinput = AS_CHARACTER(Rinput)); */
/*   for (i=0; i<Rinput_length; i++) { */
/*     Rinp[i] = R_alloc(strlen(CHAR(STRING_ELT(Rinput,i))),sizeof(char)); */
/*     strcpy( Rinp[i], CHAR(STRING_ELT(Rinput,i))); */
    
/*   } */
/*   UNPROTECT(1); */

  // MDL: Rbuff is the array of arrays containing the input
  Rbuff = Rinp;


  // MDL: alloc the char ** for the input and copy everything in it
  PROTECT(Rdatabase = AS_CHARACTER(Rdatabase));
  Rdb  = (char **) R_alloc (Rdb_length, sizeof(char *));
  for (i=0; i<Rdb_length; i++) 
    {
      ls = strlen(CHAR(STRING_ELT(Rdatabase,i)));
      Rdb[i] = R_alloc ((1+ls),sizeof(char));
      strcpy( Rdb[i], CHAR(STRING_ELT(Rdatabase,i)));
      Rdb[i][ls]='\0';
      //      Rprintf("%3i %s\n",i,Rinp[i]);
    }
  UNPROTECT(1);

  Rdb_buff = Rdb;

  // MDL: default for output to file: FALSE
  R_fileprint = FALSE;

  if (strncmp(argv[4],dum, 1)==0) {
    R_fileprint = TRUE;
    if (R_verbose == TRUE)
      Rprintf(":: Ouput on file %s\n",argv[2]);
  } 

  phast = FALSE;
  
#ifdef RphreeDEBUG
  Rprintf(": deb:: process_file_names con 1: %s, 2: %s, 3: %s, 4: %s\n",
	  argv[0],argv[1],argv[2],argv[3]);
#endif
  //  R_CheckUserInterrupt();

/*
 *   Add callbacks for error_msg and warning_msg
 */
  if (add_output_callback (phreeqc_handler, NULL) != OK)
  {
    //    Rprintf ("RLIB_RLIB_ERROR: NULL pointer returned from calloc or realloc, terminating with error -1");
    return(R_return_err(-1, "malloc"));
  }

/*
 *   Open input/output files
 */ 

#ifdef RphreeDEBUG
  Rprintf(": deb :: Open io files\n");
#endif

  RCountLine=0;
  RCountLine_db=0;
  errors = process_file_names (argc,(char **) argv, &db_cookie, &input_cookie, FALSE);
  if (errors != 0)
    {
      clean_up ();
      return(R_return_err(errors, "process_file_names"));
    }
  
#ifdef RphreeDEBUG
  Rprintf(": deb :: io files ok\n");
#endif

  // MDL: TODO Free argv

  /*
   *   Initialize arrays
   */
  errors = do_initialize ();
  if (errors != 0)
    {
      clean_up ();
      return(R_return_err(errors, "initialize"));
    }
  
  /*
   *   Load database into memory
   */ 
#ifdef RphreeDEBUG
  Rprintf(": deb :: reading database ... \n");
#endif

  errors = read_database (getc_callback, db_cookie);
  if (errors != 0)
    {
	clean_up ();
	return(R_return_err(errors, "database"));
    }  
#ifdef RphreeDEBUG
  Rprintf(": deb :: database loaded.\n");
#endif

/*
 *   Read input data for simulation
 */

  if (Rnsim > 1) 
    {
#ifdef RphreeDEBUG
      Rprintf(": deb :: R_run_simulations (multiple) ...\n");
#endif
      Rout = R_run_simulations (getc_callback, input_cookie);

#ifdef RphreeDEBUG
      Rprintf(": deb :: R_run_simulations (multiple) OK\n");
#endif

/*
 *   Display successful status
 */
      errors = do_status ();
      if (errors != 0)
	{
	  clean_up ();
	  return(R_return_err(errors, "do_status"));
	}

#ifdef RphreeDEBUG
      Rprintf(": deb :: do_status OK\n");
#endif


    }
  else
    { 
#ifdef RphreeDEBUG
      Rprintf(": deb :: normal run_simulations ...\n");
#endif
      errors = run_simulations (getc_callback, input_cookie);
      if (errors != 0)
	{
	  clean_up ();
	  return(R_return_err(errors, "run_simulation"));
	}
      

#ifdef RphreeDEBUG
      Rprintf(": deb :: normal run_simulations OK\n");
#endif


      // MDL: no errors, so form the output list and get back to R
      Rout = R_do_out();
#ifdef RphreeDEBUG
      Rprintf(": deb :: R_do_out OK\n");
#endif


/*
 *   Display successful status
 */
      errors = do_status ();
      if (errors != 0)
	{
	  clean_up ();
	  return(R_return_err(errors, "do_status"));
	}
      
#ifdef RphreeDEBUG
      Rprintf(": deb :: do_status OK\n");
#endif
    }


  clean_up ();
    
#ifdef RphreeDEBUG
  Rprintf(": deb :: clean_up OK\n");
#endif

  close_input_files ();
#ifdef RphreeDEBUG
  Rprintf(": deb :: close_input OK\n");
#endif


  close_output_files ();
#ifdef RphreeDEBUG
  Rprintf(": deb :: close_output OK\n");
#endif



#ifdef RphreeDEBUG
  Rprintf(": deb :: deallocating\n");
#endif

  //  for (i=0; i<Rinput_length; i++) 
  //   Free(Rinp[i]);
  //Rprintf(": deb :: deallocating Rinp\n");
/*   Free(Rinp); */

/*   for (i=0; i<5; i++) */
/*     Free(argv[i]); */
/*   Rprintf(": deb :: deallocating argv\n"); */

  if (R_verbose == TRUE)
    Rprintf(":: Seems ok, bye!\n");
  return(Rout);
}


//
// MDL: R_do_out forma la lista dell'output per R
// 
SEXP
R_do_out (void)
{
  SEXP R_out, R_out_names, Rspecies, Rphases, Rsatind, Rdesc, Rtot, Rkin, Rpunched; 

  int n;
  s_h2o->lm = s_h2o->la;
  species_list_sort ();

  // MDL: alloc the R list "Rout" and the names
  PROTECT(R_out = allocVector(VECSXP, Routdim));
  PROTECT(R_out_names = allocVector(STRSXP, Routdim));

  //#ifdef RphreeDEBUG
  //Rprintf(": deb :: Routdim: %i\n", Routdim);
  //#endif
  // collect solution in R structures
  n=0;

  if (selout[0]) {

#ifdef RphreeDEBUG
  Rprintf(": deb :: kin selected\n");
#endif

    Rkin = R_sol_kin();
    SET_VECTOR_ELT(R_out, n, Rkin);
    SET_STRING_ELT(R_out_names, n, mkChar("kin"));
    n++;
  }

  if (selout[1]) {
#ifdef RphreeDEBUG
  Rprintf(": deb :: totals selected\n");
#endif
    Rtot = R_sol_totals();
    SET_VECTOR_ELT(R_out, n, Rtot);
    SET_STRING_ELT(R_out_names, n, mkChar("elements"));
    n++;
  }

  if (selout[2]) {
#ifdef RphreeDEBUG
  Rprintf(": deb :: desc selected\n");
#endif
    Rdesc = R_sol_descr();
    SET_VECTOR_ELT(R_out, n, Rdesc);
    SET_STRING_ELT(R_out_names, n, mkChar("desc"));
    n++;
  }

  if (selout[3]) {
#ifdef RphreeDEBUG
  Rprintf(": deb :: species selected\n");
#endif
    Rspecies = R_sol_species();
    SET_VECTOR_ELT(R_out, n, Rspecies);
    SET_STRING_ELT(R_out_names, n, mkChar("species"));
    n++;
  }


  // MDL: pphases can be empty !
  if (selout[4]) {
#ifdef RphreeDEBUG
    Rprintf(": deb :: pphases selected\n");
#endif
    Rphases = R_sol_pphases();
    if (Rphases==R_NilValue)
      Rprintf(":: No pure phases in the solution\n");
    else 
      {
	SET_VECTOR_ELT(R_out, n, Rphases);
	SET_STRING_ELT(R_out_names, n, mkChar("pphases"));
	n++;
      }
  }

  if (selout[5]) {
#ifdef RphreeDEBUG
  Rprintf(": deb :: SI selected\n");
#endif
    Rsatind = R_sol_SI();
    SET_VECTOR_ELT(R_out, n, Rsatind);
    SET_STRING_ELT(R_out_names, n, mkChar("SI"));
    n++;
  }

  if (selout[6]) {
#ifdef RphreeDEBUG
  Rprintf(": deb :: PUNCH selected\n");
#endif
    Rpunched = R_sol_punch();
    SET_VECTOR_ELT(R_out, n, Rpunched);
    SET_STRING_ELT(R_out_names, n, mkChar("punch"));
    n++;
  }

  // MDL: assign the names to "Rout"
  setAttrib(R_out, R_NamesSymbol, R_out_names);
  UNPROTECT(2);
  // MDL: R_punch_all();
  return(R_out);
}

//
// MDL: R_return_err
//

SEXP
R_return_err(int errors, char * tok)
{
  SEXP Rout, Rout_names, Rout_err, Rout_tok;
  PROTECT(Rout_err = allocVector(INTSXP, 1));
  PROTECT(Rout_tok = allocVector(STRSXP, 1));
  INTEGER(Rout_err)[0] = errors;
  SET_STRING_ELT(Rout_tok, 0, mkChar(tok));

  Rout = allocVector(VECSXP, 2);
  SET_VECTOR_ELT(Rout, 0, Rout_err);
  SET_VECTOR_ELT(Rout, 1, Rout_tok);

  PROTECT(Rout_names = allocVector(STRSXP, 2));
  SET_STRING_ELT(Rout_names, 0, mkChar("error"));
  SET_STRING_ELT(Rout_names, 1, mkChar("token"));

  setAttrib(Rout, R_NamesSymbol, Rout_names);
  UNPROTECT(3);
  Rprintf("--> BUM! exit on error %s with number: %i\n",tok,errors);

  return(Rout);
}

//
// MDL: R_sol_descr
//

SEXP
R_sol_descr(void)
{
  SEXP Rd_names, Rd_val, Rd_notes, Rd_list, R_list_names, Rdim_desc;
  int i, tot, n;
  tot=14;
  i=0;

  PROTECT(Rdim_desc= NEW_INTEGER(1));
  PROTECT(Rd_names = allocVector(STRSXP, tot));
  PROTECT(Rd_notes = allocVector(STRSXP, tot));
  PROTECT(Rd_val   = allocVector(REALSXP, tot));
  
  // MDL: pH
  REAL(Rd_val)[i] =  (double) (-(s_hplus->la));
  SET_STRING_ELT(Rd_names, i, mkChar("pH"));
  
  if (ph_unknown == NULL)
    {
      SET_STRING_ELT(Rd_notes, i, mkChar(" ")); 
    }
  else if (ph_unknown == charge_balance_unknown)
    {
      SET_STRING_ELT(Rd_notes, i, mkChar("Charge"));
    }
  else if (ph_unknown->type == SOLUTION_PHASE_BOUNDARY)
    {
      //"  Equilibrium with"
      SET_STRING_ELT(Rd_notes, i, mkChar(ph_unknown->phase->name));
    }
  else if (ph_unknown->type == ALK)
    {
      SET_STRING_ELT(Rd_notes, i, mkChar("Adjust alkalinity"));
    }
  i++;
  
  // MDL:  pe
  REAL(Rd_val)[i] = (double) (-(s_eminus->la)); 
  SET_STRING_ELT(Rd_names, i, mkChar("pe"));

  if (pe_unknown == NULL)
    {
      SET_STRING_ELT(Rd_notes, i, mkChar(" ")); 
    }
  else if (pe_unknown == charge_balance_unknown)
    {
      SET_STRING_ELT(Rd_notes, i, mkChar("Charge"));
    }
  else if (pe_unknown->type == SOLUTION_PHASE_BOUNDARY)
    {
      //"  Equilibrium with"
      SET_STRING_ELT(Rd_notes, i, mkChar(pe_unknown->phase->name));
    }
  else if (pe_unknown->type == MH)
    {
      SET_STRING_ELT(Rd_notes, i, mkChar("Adjusted to redox eq"));
    }
  i++;

  // MDL: Others
  double  EC = calc_SC ();
  if (EC > 0) {
    REAL(Rd_val)[i] =  (double) (EC);
    SET_STRING_ELT(Rd_names, i, mkChar("SpecCond"));
    i++;
  }

  REAL(Rd_val)[i] = exp (s_h2o->la * LOG_10);
  SET_STRING_ELT(Rd_names, i, mkChar("WaterActivity"));
  i++;

  REAL(Rd_val)[i] = (double) mu_x;
  SET_STRING_ELT(Rd_names, i, mkChar("IonicStr"));
  i++;

  REAL(Rd_val)[i] = (double) mass_water_aq_x;
  SET_STRING_ELT(Rd_names, i, mkChar("water"));
  i++;

  if (alkalinity_unknown == NULL)
  {
    REAL(Rd_val)[i] = (double) (total_alkalinity / mass_water_aq_x) ;
    SET_STRING_ELT(Rd_names, i, mkChar("Tot_Alk"));
    i++;
  }
  if (carbon_unknown == NULL)
  {
    REAL(Rd_val)[i] = (double) (total_carbon / mass_water_aq_x) ;
    SET_STRING_ELT(Rd_names, i, mkChar("Tot_C"));
    i++;
  }

  REAL(Rd_val)[i] = (double) (total_co2 / mass_water_aq_x) ;
  SET_STRING_ELT(Rd_names, i, mkChar("Tot_CO2"));
  i++;

  REAL(Rd_val)[i] = (double) (tc_x) ;
  SET_STRING_ELT(Rd_names, i, mkChar("temp"));
  i++;

  REAL(Rd_val)[i] = (double) cb_x ;
  SET_STRING_ELT(Rd_names, i, mkChar("ElectrBal"));
  i++;

  REAL(Rd_val)[i] = (double) (100.0 * cb_x / total_ions_x);
  SET_STRING_ELT(Rd_names, i, mkChar("Per_Error"));
  i++;

  REAL(Rd_val)[i] = (double) (iterations);
  SET_STRING_ELT(Rd_names, i, mkChar("iterations"));
  i++;


  REAL(Rd_val)[i] = (double) (total_h_x);
  SET_STRING_ELT(Rd_names, i, mkChar("Tot_H"));
  i++;

  REAL(Rd_val)[i] = (double) (total_o_x);
  SET_STRING_ELT(Rd_names, i, mkChar("Tot_O"));
  i++;
 
  INTEGER(Rdim_desc)[0]=i;
  for (n=i; n<tot; n++)
    {
      REAL(Rd_val)[n] = (double) 0.;
      SET_STRING_ELT(Rd_names, n, mkChar("Bla"));
      SET_STRING_ELT(Rd_notes, n, mkChar("Bla"));
    }
/*   if (pitzer_model == TRUE) */
/*   { */
/*     output_msg (OUTPUT_MESSAGE, "%45s%3d\n", "Gamma iterations  = ", */
/* 		gamma_iterations); */
/*     output_msg (OUTPUT_MESSAGE, "%45s%9.5f\n", "Osmotic coefficient  = ", */
/* 		COSMOT); */
/*     output_msg (OUTPUT_MESSAGE, "%45s%9.5f\n", "Density of water  = ", DW0); */
/*   } */
  

#ifdef RphreeDEBUG
  Rprintf(": deb :: R_sol_descr after for\n");
#endif
  
  PROTECT(R_list_names = allocVector(STRSXP, 4));
  // MDL: names of the out list
  SET_STRING_ELT(R_list_names, 0, mkChar("name"));
  SET_STRING_ELT(R_list_names, 1, mkChar("val"));
  SET_STRING_ELT(R_list_names, 2, mkChar("ann"));
  SET_STRING_ELT(R_list_names, 3, mkChar("dim"));


#ifdef RphreeDEBUG
  Rprintf(": deb :: R_sol_descr after set R_list_names\n");
#endif

  // MDL: form the Rd_list list
  PROTECT(Rd_list = allocVector(VECSXP, 4));
  SET_VECTOR_ELT(Rd_list, 0, Rd_names);
  SET_VECTOR_ELT(Rd_list, 1, Rd_val);
  SET_VECTOR_ELT(Rd_list, 2, Rd_notes);
  SET_VECTOR_ELT(Rd_list, 3, Rdim_desc);


  setAttrib(Rd_list, R_NamesSymbol, R_list_names);
  UNPROTECT(6);

  return(Rd_list);
} 


//
// MDL: R_sol_species
//

SEXP
R_sol_species (void)
{
  SEXP Rspecies, Rspecies_names, Rspecies_act, Rsp_list, Rsp_list_names;
  SEXP Rspecies_mast_names, Rspecies_mast_molal;
  int i, Rmastercount;
  char *name, *name1;
  struct master *master_ptr;
  LDBLE min, lm;
  //  count_species_list =  count_species_list;
  // MDL: alloc the moles and names vectors: species in water solution
  PROTECT(Rspecies = allocVector(REALSXP, count_species_list));
  PROTECT(Rspecies_names = allocVector(STRSXP, count_species_list));
  PROTECT(Rspecies_act = allocVector(REALSXP, count_species_list));
  PROTECT(Rspecies_mast_names = allocVector(STRSXP, MAX_MASTER)); // MDL: MAX_MASTER = 50 (in globals)
  PROTECT(Rspecies_mast_molal = allocVector(REALSXP, MAX_MASTER));

  // MDL: copy the species and their names in the new R objects
  min = -1000;
  s_h2o->lm = s_h2o->la;
  name = s_hplus->secondary->elt->name;

  Rmastercount = 0;
  for(i = 0; i < count_species_list; i++) {  

    // MDL: master species, for trimming
    if (species_list[i].s->type == EX)
      continue;
    if (species_list[i].s->type == SURF)
      continue;
    
    if (species_list[i].master_s->secondary != NULL)
      {
	master_ptr = species_list[i].master_s->secondary;
	name1 = species_list[i].master_s->secondary->elt->name;
      }
    else
      {
	master_ptr = species_list[i].master_s->primary;
	name1 = species_list[i].master_s->primary->elt->name;
      }
    if (name1 != name)
      {
	name = name1;
	REAL(Rspecies_mast_molal)[Rmastercount] = (double) (master_ptr->total / mass_water_aq_x);  
	SET_STRING_ELT(Rspecies_mast_names, Rmastercount, mkChar(name));
	Rmastercount++;   
	min = censor * master_ptr->total / mass_water_aq_x;
	if (min > 0)
	  {
	    min = log10 (min);
	  }
	else
	  {
	    min = -1000.;
	  }
      }
    // MDL: water species  
    if (species_list[i].s->lm > min)
      {
	if (species_list[i].s == s_h2o)
	  {
	    lm = log10 (s_h2o->moles / mass_water_aq_x);
	  }
	else
	  {
	    lm = species_list[i].s->lm;
	  }

	REAL(Rspecies)[i] =  (species_list[i].s->moles) / mass_water_aq_x ;
	REAL(Rspecies_act)[i] =   (double) under (species_list[i].s->lm +
						  species_list[i].s->lg);
	SET_STRING_ELT(Rspecies_names,i,mkChar(species_list[i].s->name));
      }
  }


  // Rprintf(":: Master= %i", Rmastercount);
  // MDL: Return a vector (list)
  PROTECT(Rsp_list = allocVector(VECSXP, 5));
  SET_VECTOR_ELT(Rsp_list, 0, Rspecies_names);
  SET_VECTOR_ELT(Rsp_list, 1, Rspecies);
  SET_VECTOR_ELT(Rsp_list, 2, Rspecies_act);
  SET_VECTOR_ELT(Rsp_list, 3, Rspecies_mast_names);
  SET_VECTOR_ELT(Rsp_list, 4, Rspecies_mast_molal);
  
  PROTECT(Rsp_list_names = allocVector(STRSXP, 5));
  SET_STRING_ELT(Rsp_list_names, 0, mkChar("name"));
  SET_STRING_ELT(Rsp_list_names, 1, mkChar("molal"));
  SET_STRING_ELT(Rsp_list_names, 2, mkChar("act"));
  SET_STRING_ELT(Rsp_list_names, 3, mkChar("Mname"));
  SET_STRING_ELT(Rsp_list_names, 4, mkChar("Mmolal"));
  setAttrib(Rsp_list, R_NamesSymbol, Rsp_list_names);

  UNPROTECT(7);

  return(Rsp_list);

}


//
//  MDL: R_sol_punch
//

SEXP
R_sol_punch (void)
{
  int i;
  SEXP R_punch_out;

#ifdef RphreeDEBUG
  Rprintf(": deb :: R_sol_punch called \n");
#endif
  R_do_punch = TRUE;

  punch.user_punch = TRUE;
  punch_user_punch ();
  punch.user_punch = FALSE;
  // MDL: alloc the vector
  PROTECT(R_punch_out = allocVector(REALSXP, Rpunch_dim));

  for (i=0; i<Rpunch_dim; i++)
    {
      // MDL: copy the vector from the global pointer
      REAL(R_punch_out)[i] =  *(Rpunch_vec_point + i) ;
      // 
    }

  UNPROTECT(1);


  return(R_punch_out);
}

//
// MDL: R_sol_SI
//

SEXP
R_sol_SI (void)
{
  // MDL: saturation indices */
  double si, iap, lk;
  double la_eminus;
  struct rxn_token *rxn_ptr;
  int Rcount_phases, i;
  int indexes[count_phases];
  SEXP Rsatind, Rsatind_names, Rsi_si, Rsi_iap, Rsi_lk, Rsi_names, Rsi_formula;

  la_eminus = s_eminus->la; 

  // MDL: count_phases ist much bigger than the real phases; need to
  // MDL: initialize another counter, later trimmed
  Rcount_phases = 0;
  
  for (i = 0; i < count_phases; i++)
    {
      if (phases[i]->in == FALSE || phases[i]->type != SOLID) {
	continue;
      }
      else
	{
	  indexes[Rcount_phases] = i;
	  Rcount_phases++;
	}
    }

  // MDL: allocate the correct dimension for the vectors
  PROTECT(Rsi_names = allocVector(STRSXP, Rcount_phases));
  PROTECT(Rsi_si = allocVector(REALSXP, Rcount_phases));
  PROTECT(Rsi_iap = allocVector(REALSXP, Rcount_phases));
  PROTECT(Rsi_lk = allocVector(REALSXP, Rcount_phases));
  PROTECT(Rsi_formula = allocVector(STRSXP, Rcount_phases));


  // MDL: check if there is at least a phase!
  if (Rcount_phases > 0)
    {
      for (i = 0; i < Rcount_phases; i++)
	{
	  lk = k_calc (phases[ indexes[i] ]->rxn->logk, tk_x);
	  iap = 0.0;
	  for (rxn_ptr = phases[indexes[i]]->rxn->token + 1; rxn_ptr->s != NULL; rxn_ptr++)
	    {
	      if (rxn_ptr->s != s_eminus)
		{
		  iap += (rxn_ptr->s->lm + rxn_ptr->s->lg) * rxn_ptr->coef;
		}
	      else
		{
		  iap += la_eminus * rxn_ptr->coef;
		}
	    }
	  si = -lk + iap;
	  REAL(Rsi_si)[i] = (double) si;
	  REAL(Rsi_iap)[i] = (double) iap;
	  REAL(Rsi_lk)[i] = (double) lk;
	  SET_STRING_ELT(Rsi_names, i, mkChar(phases[indexes[i]]->name));
	  SET_STRING_ELT(Rsi_formula, i, mkChar(phases[indexes[i]]->formula));
	}
    }
  else
    {
      return(R_NilValue);
    }

  Rsatind = allocVector(VECSXP, 5);

  SET_VECTOR_ELT(Rsatind, 0, Rsi_names);
  SET_VECTOR_ELT(Rsatind, 1, Rsi_si);
  SET_VECTOR_ELT(Rsatind, 2, Rsi_iap);
  SET_VECTOR_ELT(Rsatind, 3, Rsi_lk);
  SET_VECTOR_ELT(Rsatind, 4, Rsi_formula);
  
  Rsatind_names = allocVector(STRSXP, 5);
  SET_STRING_ELT(Rsatind_names, 0, mkChar("names"));
  SET_STRING_ELT(Rsatind_names, 1, mkChar("SI"));
  SET_STRING_ELT(Rsatind_names, 2, mkChar("IAP"));
  SET_STRING_ELT(Rsatind_names, 3, mkChar("logK"));
  SET_STRING_ELT(Rsatind_names, 4, mkChar("formula"));
  setAttrib(Rsatind, R_NamesSymbol, Rsatind_names);

  UNPROTECT(5);  
  //  Rprintf("R_sol_SI finito ... " );

  return(Rsatind);
}


//
// MDL: R_sol_pphases
//

SEXP
R_sol_pphases (void)
{
  int Rcount_unknowns, i;
  int indexes[count_unknowns];

  SEXP Rphases, Rphases_names, Rphases_moles, Rphases_outnames, Rphases_delta;

  struct phase *phase_ptr;

  if (pure_phase_unknown == NULL) {

#ifdef RphreeDEBUG
    Rprintf(": deb :: pure_phase_unknown == NULL\n");
#endif
    return (R_NilValue);
  }

  // MDL: trim the empty names
  Rcount_unknowns = 0;
  for(i = 0; i < count_unknowns; i++) {
    if (x[i]->type != PP)
      continue;

    phase_ptr = x[i]->phase;    
    if (x[i]->phase->rxn_x == NULL || phase_ptr->in == FALSE)
      {
#ifdef RphreeDEBUG
	Rprintf(": deb :: Not present: %s\n",x[i]->phase->name);
#endif
      }
    else
      { 
	indexes[Rcount_unknowns] = i;
	Rcount_unknowns++;
      } 
  }

  if (Rcount_unknowns>0)
    {
      // MDL: pure phases assemblage (names/moles) with correct dimension
      PROTECT(Rphases_moles = allocVector(REALSXP, Rcount_unknowns));
      PROTECT(Rphases_delta = allocVector(REALSXP, Rcount_unknowns));
      PROTECT(Rphases_names = allocVector(STRSXP, Rcount_unknowns));
      // PROTECT(Rphases_SI = allocVector(REALSXP, Rcount_unknowns));
      
      for(i = 0; i < Rcount_unknowns; i++) {
	REAL(Rphases_moles)[i] = x[indexes[i]]->moles ;
	REAL(Rphases_delta)[i] = x[indexes[i]]->moles - 
	  x[indexes[i]]->pure_phase->moles - x[indexes[i]]->pure_phase->delta ;
	SET_STRING_ELT(Rphases_names, i, mkChar(x[indexes[i]]->phase->name));
      }

      // MDL: dim = 3 if _SI is included
      PROTECT(Rphases = allocVector(VECSXP, 3));
      PROTECT(Rphases_outnames = allocVector(STRSXP, 3));
      SET_STRING_ELT(Rphases_outnames, 0, mkChar("names"));
      SET_STRING_ELT(Rphases_outnames, 1, mkChar("moles"));
      SET_STRING_ELT(Rphases_outnames, 2, mkChar("delta"));
      //  SET_STRING_ELT(Rphases_outnames, 2, mkChar("SI"));
      

      SET_VECTOR_ELT(Rphases, 0, Rphases_names);
      SET_VECTOR_ELT(Rphases, 1, Rphases_moles);
      SET_VECTOR_ELT(Rphases, 2, Rphases_delta);
      //  SET_VECTOR_ELT(Rphases, 2, Rphases_SI);

      setAttrib(Rphases, R_NamesSymbol, Rphases_outnames);

      // 5 if _SI is included
      UNPROTECT(5);
      return(Rphases);
    }
  else
    {
      /* PROTECT(Rphases_moles = allocVector(REALSXP, 1)); */
      /*       PROTECT(Rphases_names = allocVector(STRSXP, 1)); */
      /*       // PROTECT(Rphases_SI = allocVector(REALSXP, 1)); */
      
      /*       REAL(Rphases_moles)[0] = 0. ; */
      /*       SET_STRING_ELT(Rphases_names, 0, mkChar("nothing"));    */
      // MDL: Rprintf("No pure phase!\n");
      return(R_NilValue);
    }

}

//
// MDL: R_sol_totals
//

SEXP
R_sol_totals (void)
{
  int i, R_count_unknowns=0; 
  int pure_water = TRUE;
  int indexes[count_unknowns];
  SEXP Relements, Rtotals, Rtotals_ann, Rtot_out, Rtot_out_names;


  // MDL: Trimming
  for (i = 0; i < count_unknowns; i++)
  {
    if (x[i] == ph_unknown)
      continue;
    if (x[i] == pe_unknown)
      continue;
    
    if (x[i] == alkalinity_unknown || 
	x[i] == charge_balance_unknown || 
	x[i]->type == SOLUTION_PHASE_BOUNDARY ||
	x[i]->type == MB)
      {
	pure_water=FALSE;
	indexes[R_count_unknowns] = i;
	R_count_unknowns++;
	continue;
      }
  }

  // MDL: total composition (names/molal)
  PROTECT(Rtotals = allocVector(REALSXP, R_count_unknowns));
  PROTECT(Relements = allocVector(STRSXP, R_count_unknowns));
  PROTECT(Rtotals_ann = allocVector(STRSXP, R_count_unknowns));

  if (pure_water == TRUE)
    {
      REAL(Rtotals)[0] = (double) 0;
      SET_STRING_ELT(Relements, i, mkChar("Pure water"));
    }
  
  for (i = 0; i < R_count_unknowns; i++)
  {
    if (x[indexes[i]] == alkalinity_unknown)
    {
      REAL(Rtotals)[i] = (double) (x[indexes[i]]->f / mass_water_aq_x);
      SET_STRING_ELT(Relements, i, mkChar(x[indexes[i]]->total->description));
      continue;
    }
    if (x[indexes[i]] == charge_balance_unknown)
    {
      REAL(Rtotals)[i] = (double) (x[indexes[i]]->sum / mass_water_aq_x);
      SET_STRING_ELT(Relements, i, mkChar(x[indexes[i]]->description));   
      SET_STRING_ELT(Rtotals_ann, i, mkChar("Charge Balance"));
      continue;
    }
    if (x[indexes[i]]->type == SOLUTION_PHASE_BOUNDARY)
    {
      REAL(Rtotals)[i] = (double) (x[indexes[i]]->sum / mass_water_aq_x);
      SET_STRING_ELT(Relements, i, mkChar(x[indexes[i]]->description));   
      SET_STRING_ELT(Rtotals_ann, i, mkChar("equilibrium with"));   
      // MDL: TODO add "equilibrium with" indication
      continue;
    }
    if (x[indexes[i]]->type == MB)
    {
      REAL(Rtotals)[i] = (double) (x[indexes[i]]->sum / mass_water_aq_x);
      SET_STRING_ELT(Relements, i, mkChar(x[indexes[i]]->description));
    }
  }


  PROTECT(Rtot_out = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(Rtot_out, 0, Relements);
  SET_VECTOR_ELT(Rtot_out, 1, Rtotals);
  SET_VECTOR_ELT(Rtot_out, 2, Rtotals_ann);

  PROTECT(Rtot_out_names = allocVector(STRSXP, 3));
  SET_STRING_ELT(Rtot_out_names, 0, mkChar("elem"));
  SET_STRING_ELT(Rtot_out_names, 1, mkChar("molal"));
  SET_STRING_ELT(Rtot_out_names, 2, mkChar("ann"));
  setAttrib(Rtot_out, R_NamesSymbol, Rtot_out_names);

  UNPROTECT(5);

  return(Rtot_out);
}


/* void */
/* Rcleanup (void) */
/* { */
/*   return(); */
/* } */



/* // */
/* // MDL: R_do_simout forma la lista di una simulazione */
/* //  */
/* SEXP */
/* R_do_saveSim (void) */
/* { */
/*   SEXP Rout, Rout_names, Rspecies, Rphases, Rsatind, Rdesc, Rtot;  */

/*   int n; */

/*   s_h2o->lm = s_h2o->la; */
/*   species_list_sort (); */

/*   // MDL: alloc the R list "Rout" and the names */
/*   Rout = allocVector(VECSXP, Routdim); */
/*   Rout_names = allocVector(STRSXP, Routdim); */


/*   // collect solution in R structures */
/*   n=0; */

/*   if (selout[0]) { */
/*     Rtot = R_sol_totals(); */
/*     SET_VECTOR_ELT(Rout, n, Rtot); */
/*     SET_STRING_ELT(Rout_names, n, mkChar("elements")); */
/*     n++; */
/*   } */

/*   if (selout[1]) { */
/*     Rdesc = R_sol_descr(); */
/*     SET_VECTOR_ELT(Rout, n, Rdesc); */
/*     SET_STRING_ELT(Rout_names, n, mkChar("desc")); */
/*     n++; */
/*   } */

/*   if (selout[2]) { */
/*     Rspecies = R_sol_species(); */
/*     SET_VECTOR_ELT(Rout, n, Rspecies); */
/*     SET_STRING_ELT(Rout_names, n, mkChar("species")); */
/*     n++; */
/*   } */

/*   if (selout[3]) { */
/*     Rphases = R_sol_pphases(); */
/*     SET_VECTOR_ELT(Rout, n, Rphases); */
/*     SET_STRING_ELT(Rout_names, n, mkChar("pphases")); */
/*     n++; */
/*   } */

/*   if (selout[4]) { */
/*     Rsatind = R_sol_SI(); */
/*     SET_VECTOR_ELT(Rout, n, Rsatind); */
/*     SET_STRING_ELT(Rout_names, n, mkChar("SI")); */
/*     n++; */
/*   } */

/*   // MDL: assign the names to "Rout" */
/*   setAttrib(Rout, R_NamesSymbol, Rout_names); */

/*   return(Rout); */
/* } */


//
// MDL: R_sol_kin
//

SEXP
R_sol_kin (void)
{
  int i, nk; 
  LDBLE sim_time;
  struct kinetics *kinetics_ptr;

  SEXP Rkin_moles, Rkin_delta, Rkin_names;
  SEXP Rkin_out, Rkin_out_names;
  
  if (state < REACTION) {
    Rprintf(": kinetics: state not REACTION\n");
    return (R_NilValue);
  }

  kinetics_ptr = NULL;

  kinetics_ptr = kinetics_bsearch (-2, &i);

  if (kinetics_ptr == NULL) {
    Rprintf(": deb :: Error: kinetics_ptr== NULL\n");
    return (R_NilValue);
  }
  /*
   *   determine time step
   */
  // MDL: stripped TRANSPORT, ADVECTION, PHAST!

  sim_time = 0.;
  if (incremental_reactions == TRUE)
    {
      if (kinetics_ptr->count_steps > 0)
	{
	  for (i = 0; i < reaction_step; i++)
	    {
	      if (i < kinetics_ptr->count_steps)
		{
		  sim_time += kinetics_ptr->steps[i];
		}
	      else
		{
		  sim_time += kinetics_ptr->steps[kinetics_ptr->count_steps - 1];
		}
	    }
	}
      else if (kinetics_ptr->count_steps < 0)
	{
	  if (reaction_step > -kinetics_ptr->count_steps)
	    {
	      sim_time = kinetics_ptr->steps[0];
	    }
	  else
	    {
	      sim_time =
		reaction_step * kinetics_ptr->steps[0] /
		((LDBLE) (-kinetics_ptr->count_steps));
	    }
	}
    }
  /*
   *  Print amount of reaction
   */

  nk = kinetics_ptr->count_comps ;
#ifdef RphreeDEBUG
  Rprintf(": deb :: Number of kinetics: %i\n",nk);
#endif

  // MDL: kinetics assemblage (names/moles/delta)
  PROTECT(Rkin_moles = allocVector(REALSXP, nk));
  PROTECT(Rkin_delta = allocVector(REALSXP, nk));
  PROTECT(Rkin_names = allocVector(STRSXP, nk));
  /*
   *  Print reaction
   */
  for (i = 0; i < kinetics_ptr->count_comps; i++)
    {
      REAL(Rkin_moles)[i] = (double) kinetics_ptr->comps[i].m ; 
      REAL(Rkin_delta)[i] = (double) -kinetics_ptr->comps[i].moles ; 
      SET_STRING_ELT(Rkin_names, i, mkChar(kinetics_ptr->comps[i].rate_name));
    }
  

  UNPROTECT(3);

  Rkin_out = allocVector(VECSXP, 3);
  SET_VECTOR_ELT(Rkin_out, 0, Rkin_names);
  SET_VECTOR_ELT(Rkin_out, 1, Rkin_moles);
  SET_VECTOR_ELT(Rkin_out, 2, Rkin_delta);

  Rkin_out_names = allocVector(STRSXP, 3);
  SET_STRING_ELT(Rkin_out_names, 0, mkChar("name"));
  SET_STRING_ELT(Rkin_out_names, 1, mkChar("moles"));
  SET_STRING_ELT(Rkin_out_names, 2, mkChar("delta"));
  setAttrib(Rkin_out, R_NamesSymbol, Rkin_out_names);
  return(Rkin_out);
}
