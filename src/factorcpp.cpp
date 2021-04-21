#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x, const Vector<RTYPE>& levs) {
    IntegerVector out = match(x, levs);
    out.attr("levels") = as<CharacterVector>(levs);
    out.attr("class") = "factor";
    return out;
}

// [[Rcpp::export]]
SEXP factorcpp( SEXP x, SEXP levs) {
    switch( TYPEOF(x) ) {
    case INTSXP: return fast_factor_template<INTSXP>(x, levs);
    case REALSXP: return fast_factor_template<REALSXP>(x, levs);
    case STRSXP: return fast_factor_template<STRSXP>(x, levs);
    }
    return R_NilValue;
}

/* https://gallery.rcpp.org/articles/fast-factor-generation/ */
