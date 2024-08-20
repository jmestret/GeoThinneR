#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare the C++ function
extern SEXP rounding_hashing_thinning(SEXP, SEXP, SEXP, SEXP, SEXP);

// Register the routines
static const R_CallMethodDef CallEntries[] = {
  {"_GeoThinneR_rounding_hashing_thinning", (DL_FUNC) &rounding_hashing_thinning, 7},
  {NULL, NULL, 0}
};

void R_init_GeoThinneR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
