// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// adj_matrix
NumericMatrix adj_matrix(NumericVector x, NumericVector y, Nullable<int> states);
RcppExport SEXP _seqR_adj_matrix(SEXP xSEXP, SEXP ySEXP, SEXP statesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Nullable<int> >::type states(statesSEXP);
    rcpp_result_gen = Rcpp::wrap(adj_matrix(x, y, states));
    return rcpp_result_gen;
END_RCPP
}
// collapse
List collapse(DataFrame data, bool include_na);
RcppExport SEXP _seqR_collapse(SEXP dataSEXP, SEXP include_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type include_na(include_naSEXP);
    rcpp_result_gen = Rcpp::wrap(collapse(data, include_na));
    return rcpp_result_gen;
END_RCPP
}
// findMovement
NumericVector findMovement(NumericVector v, String direction, int buffer, Nullable<int> lbuffer, Nullable<int> rbuffer, Nullable<int> upper_lim, Nullable<int> lower_lim);
RcppExport SEXP _seqR_findMovement(SEXP vSEXP, SEXP directionSEXP, SEXP bufferSEXP, SEXP lbufferSEXP, SEXP rbufferSEXP, SEXP upper_limSEXP, SEXP lower_limSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< String >::type direction(directionSEXP);
    Rcpp::traits::input_parameter< int >::type buffer(bufferSEXP);
    Rcpp::traits::input_parameter< Nullable<int> >::type lbuffer(lbufferSEXP);
    Rcpp::traits::input_parameter< Nullable<int> >::type rbuffer(rbufferSEXP);
    Rcpp::traits::input_parameter< Nullable<int> >::type upper_lim(upper_limSEXP);
    Rcpp::traits::input_parameter< Nullable<int> >::type lower_lim(lower_limSEXP);
    rcpp_result_gen = Rcpp::wrap(findMovement(v, direction, buffer, lbuffer, rbuffer, upper_lim, lower_lim));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_seqR_adj_matrix", (DL_FUNC) &_seqR_adj_matrix, 3},
    {"_seqR_collapse", (DL_FUNC) &_seqR_collapse, 2},
    {"_seqR_findMovement", (DL_FUNC) &_seqR_findMovement, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_seqR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
