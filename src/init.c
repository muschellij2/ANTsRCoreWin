#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP antsImage_GetNeighborhoodMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP iMathInterface(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"antsImage_GetNeighborhoodMatrix", (DL_FUNC) &antsImage_GetNeighborhoodMatrix, 7},
    {"iMathInterface",                  (DL_FUNC) &iMathInterface,                  1},
    {NULL, NULL, 0}
};

void R_init_ANTsRCoreWin(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}