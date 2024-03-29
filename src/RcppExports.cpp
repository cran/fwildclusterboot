// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/fwildclusterboot.h"
#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// boot_algo3_crv1_denom
arma::vec boot_algo3_crv1_denom(int B, int G, double ssc, arma::mat H, arma::vec Cg, arma::mat v, int cores);
RcppExport SEXP _fwildclusterboot_boot_algo3_crv1_denom(SEXP BSEXP, SEXP GSEXP, SEXP sscSEXP, SEXP HSEXP, SEXP CgSEXP, SEXP vSEXP, SEXP coresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type G(GSEXP);
    Rcpp::traits::input_parameter< double >::type ssc(sscSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type H(HSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Cg(CgSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    rcpp_result_gen = Rcpp::wrap(boot_algo3_crv1_denom(B, G, ssc, H, Cg, v, cores));
    return rcpp_result_gen;
END_RCPP
}
// boot_algo3_crv3
List boot_algo3_crv3(const int B, const int G, const int k, arma::mat v, arma::mat scores_mat, arma::mat scores_boot, arma::cube inv_tXX_tXgXg, int cores, arma::mat R, arma::mat delta_b_star);
RcppExport SEXP _fwildclusterboot_boot_algo3_crv3(SEXP BSEXP, SEXP GSEXP, SEXP kSEXP, SEXP vSEXP, SEXP scores_matSEXP, SEXP scores_bootSEXP, SEXP inv_tXX_tXgXgSEXP, SEXP coresSEXP, SEXP RSEXP, SEXP delta_b_starSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type B(BSEXP);
    Rcpp::traits::input_parameter< const int >::type G(GSEXP);
    Rcpp::traits::input_parameter< const int >::type k(kSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type v(vSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type scores_mat(scores_matSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type scores_boot(scores_bootSEXP);
    Rcpp::traits::input_parameter< arma::cube >::type inv_tXX_tXgXg(inv_tXX_tXgXgSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R(RSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type delta_b_star(delta_b_starSEXP);
    rcpp_result_gen = Rcpp::wrap(boot_algo3_crv3(B, G, k, v, scores_mat, scores_boot, inv_tXX_tXgXg, cores, R, delta_b_star));
    return rcpp_result_gen;
END_RCPP
}
// eigenMapMatMult
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B, int nthreads);
RcppExport SEXP _fwildclusterboot_eigenMapMatMult(SEXP ASEXP, SEXP BSEXP, SEXP nthreadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type nthreads(nthreadsSEXP);
    rcpp_result_gen = Rcpp::wrap(eigenMapMatMult(A, B, nthreads));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_nb_threads
int cpp_get_nb_threads();
RcppExport SEXP _fwildclusterboot_cpp_get_nb_threads() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cpp_get_nb_threads());
    return rcpp_result_gen;
END_RCPP
}
// sample_weights
arma::mat sample_weights(int G, int type);
RcppExport SEXP _fwildclusterboot_sample_weights(SEXP GSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type G(GSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(sample_weights(G, type));
    return rcpp_result_gen;
END_RCPP
}
// wildboottestHC
List wildboottestHC(const arma::vec& y, const arma::mat& X, const arma::mat& R, const double& r, const int& B, const int& N_G_bootcluster, const int& cores, const int& type, const double& small_sample_correction, const int bootstrap_type);
RcppExport SEXP _fwildclusterboot_wildboottestHC(SEXP ySEXP, SEXP XSEXP, SEXP RSEXP, SEXP rSEXP, SEXP BSEXP, SEXP N_G_bootclusterSEXP, SEXP coresSEXP, SEXP typeSEXP, SEXP small_sample_correctionSEXP, SEXP bootstrap_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const double& >::type r(rSEXP);
    Rcpp::traits::input_parameter< const int& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const int& >::type N_G_bootcluster(N_G_bootclusterSEXP);
    Rcpp::traits::input_parameter< const int& >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< const int& >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const double& >::type small_sample_correction(small_sample_correctionSEXP);
    Rcpp::traits::input_parameter< const int >::type bootstrap_type(bootstrap_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(wildboottestHC(y, X, R, r, B, N_G_bootcluster, cores, type, small_sample_correction, bootstrap_type));
    return rcpp_result_gen;
END_RCPP
}
// wildboottestCL
List wildboottestCL(const arma::vec& y, const arma::mat& X, const arma::mat& R, const double& r, const int& B, const int& N_G_bootcluster, const int& cores, const int& type, const arma::vec& cluster, const double& small_sample_correction);
RcppExport SEXP _fwildclusterboot_wildboottestCL(SEXP ySEXP, SEXP XSEXP, SEXP RSEXP, SEXP rSEXP, SEXP BSEXP, SEXP N_G_bootclusterSEXP, SEXP coresSEXP, SEXP typeSEXP, SEXP clusterSEXP, SEXP small_sample_correctionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const double& >::type r(rSEXP);
    Rcpp::traits::input_parameter< const int& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const int& >::type N_G_bootcluster(N_G_bootclusterSEXP);
    Rcpp::traits::input_parameter< const int& >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< const int& >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type cluster(clusterSEXP);
    Rcpp::traits::input_parameter< const double& >::type small_sample_correction(small_sample_correctionSEXP);
    rcpp_result_gen = Rcpp::wrap(wildboottestCL(y, X, R, r, B, N_G_bootcluster, cores, type, cluster, small_sample_correction));
    return rcpp_result_gen;
END_RCPP
}
// wildboottestCL_enum
List wildboottestCL_enum(const arma::vec& y, const arma::mat& X, const arma::mat& R, const double& r, const int& B, const int& N_G_bootcluster, const int& cores, const arma::vec& cluster, const double& small_sample_correction, const arma::mat& v);
RcppExport SEXP _fwildclusterboot_wildboottestCL_enum(SEXP ySEXP, SEXP XSEXP, SEXP RSEXP, SEXP rSEXP, SEXP BSEXP, SEXP N_G_bootclusterSEXP, SEXP coresSEXP, SEXP clusterSEXP, SEXP small_sample_correctionSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type R(RSEXP);
    Rcpp::traits::input_parameter< const double& >::type r(rSEXP);
    Rcpp::traits::input_parameter< const int& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const int& >::type N_G_bootcluster(N_G_bootclusterSEXP);
    Rcpp::traits::input_parameter< const int& >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type cluster(clusterSEXP);
    Rcpp::traits::input_parameter< const double& >::type small_sample_correction(small_sample_correctionSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(wildboottestCL_enum(y, X, R, r, B, N_G_bootcluster, cores, cluster, small_sample_correction, v));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fwildclusterboot_boot_algo3_crv1_denom", (DL_FUNC) &_fwildclusterboot_boot_algo3_crv1_denom, 7},
    {"_fwildclusterboot_boot_algo3_crv3", (DL_FUNC) &_fwildclusterboot_boot_algo3_crv3, 10},
    {"_fwildclusterboot_eigenMapMatMult", (DL_FUNC) &_fwildclusterboot_eigenMapMatMult, 3},
    {"_fwildclusterboot_cpp_get_nb_threads", (DL_FUNC) &_fwildclusterboot_cpp_get_nb_threads, 0},
    {"_fwildclusterboot_sample_weights", (DL_FUNC) &_fwildclusterboot_sample_weights, 2},
    {"_fwildclusterboot_wildboottestHC", (DL_FUNC) &_fwildclusterboot_wildboottestHC, 10},
    {"_fwildclusterboot_wildboottestCL", (DL_FUNC) &_fwildclusterboot_wildboottestCL, 10},
    {"_fwildclusterboot_wildboottestCL_enum", (DL_FUNC) &_fwildclusterboot_wildboottestCL_enum, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_fwildclusterboot(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
