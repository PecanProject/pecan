#include <Rcpp.h>
using namespace Rcpp;

// 4SAIL2 Model
//[Rcpp::export]]
NumericMatrix FourSAIL2_cpp(){
    //// Input parameters ////
    double lai;             // Leaf area index
    double fb;              // Fraction brown LAI
    double diss;            // Dissosciation factor [0,1]
    double LIDFa;           // Average leaf slope indicator [-1, +1]
    double LIDFb;           // Bimodality parameter of LIDF [-1, +1]
    double hot;             // Hot spot effect parameter; ratio of avg. leaf width and canopy height
    NumericVector rg;       // Green leaf reflectance
    NumericVector rb;       // Brown leaf reflectance
    NumericVector tg;       // Green leaf transmittance
    NumericVector tb;       // Brown leaf transmittance
    NumericVector rsosoil;  // Background rso reflectance
    NumericVector rdosoil;  // Background rdo reflectance
    NumericVector rsdsoil;  // Background rsd reflectance
    NumericVector rddsoil;  // Background rdd reflectance
    double tts;             // Solar zenith angle (degrees)
    double tto;             // Viewing zenith angle (degrees)
    double psi;             // Relative azimuth (degrees)
    double Cv;              // Vertical crown coverage [0,1]
    double zeta;            // Tree shape factor (diameter/height)

    
    // Result is a WL x 4 matrix:
    //      [,0] rsot:  bidirectional reflectance
    //      [,1] rdot:  directional reflectance for diffuse incidence
    //      [,2] rsdt:  diffuse reflectance for direct solar incidence
    //      [,3] rddt:  diffuse reflectance for diffuse incidence
    NumericMatrix result(nwl, 4);

    // Angular factors

    cts = cos(rd



