#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP advance_landscape(SEXP);
extern SEXP carry_landscape(SEXP);
extern SEXP clean_landscape(SEXP);
extern SEXP compress_landscape(SEXP);
extern SEXP extinct_landscape(SEXP);
extern SEXP iterate_landscape(SEXP, SEXP, SEXP, SEXP);
extern SEXP landlambda(SEXP);
extern SEXP num_demo_cols();
extern SEXP num_loci_poss();
extern SEXP populate_Rland(SEXP, SEXP);
extern SEXP relateinternal(SEXP, SEXP);
extern SEXP reproduce_landscape(SEXP);
extern SEXP survive_landscape(SEXP);
extern SEXP test(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"advance_landscape",   (DL_FUNC) &advance_landscape,   1},
    {"carry_landscape",     (DL_FUNC) &carry_landscape,     1},
    {"clean_landscape",     (DL_FUNC) &clean_landscape,     1},
    {"compress_landscape",  (DL_FUNC) &compress_landscape,  1},
    {"extinct_landscape",   (DL_FUNC) &extinct_landscape,   1},
    {"iterate_landscape",   (DL_FUNC) &iterate_landscape,   4},
    {"landlambda",          (DL_FUNC) &landlambda,          1},
    {"num_demo_cols",       (DL_FUNC) &num_demo_cols,       0},
    {"num_loci_poss",       (DL_FUNC) &num_loci_poss,       0},
    {"populate_Rland",      (DL_FUNC) &populate_Rland,      2},
    {"relateinternal",      (DL_FUNC) &relateinternal,      2},
    {"reproduce_landscape", (DL_FUNC) &reproduce_landscape, 1},
    {"survive_landscape",   (DL_FUNC) &survive_landscape,   1},
    {"test",                (DL_FUNC) &test,                2},
    {NULL, NULL, 0}
};

void R_init_rmetasim(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
