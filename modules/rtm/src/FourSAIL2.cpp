#include "sail_common.h"
double rd = 180.0/M_PI;

/* 4SAIL2 Model
 *      NOTE the following terminology:
 *          so  :   bidirectional reflectance
 *          ss  :   direct transmittance in the direction of the solar beam
 *          sd  :   directional-hemispherical (for solar flux)
 *          dd  :   bihemispherical
 *          do  :   hemispherical-directional (in viewing direction)
 *          oo  :   direct transmittance in direction of observation
 *
 *      References:
 *          Verhoef et al. 2007. Unified optical-thermal four-stream radiative
 *              transfer theory for homogenous vegetation canopies. IEEE Transactions
 *              on geoscience and remote sensing 45:6. 1808-1822.
 *
 */

NumericVector FourSAIL2_cpp(
        // Structural parameters
        double LAI,             // Leaf area index
        double LIDFa,           // Average leaf slope indicator [-1, +1]
        double LIDFb,           // Bimodality parameter of LIDF [-1, +1]
        double hot,               // Hot spot effect parameter; ratio of avg. leaf width and canopy height
        double f_brown,         // Fraction brown LAI
        double diss,            // Dissosciation factor for brown/green [0,1]
        double soil_rso,        // Background (soil) bidirectional reflectance
        double soil_rdo,        // Background hemispherical-directional reflectance
        double soil_rsd,        // Background directional-hemispherical reflectance
        double soil_rdd,        // Background bihemispherical reflectance [[***Where is soil moisture?***]]
        double crown_cover,     // Vertical crown coverage [0,1]
        double zeta,            // Tree shape factor (diameter/height)

        // Spectral parameters
        double rho_g,         // Green leaf reflectance
        double rho_b,         // Brown leaf reflectance
        double tau_g,         // Green leaf transmittance
        double tau_b,         // Brown leaf transmittance

        // Observational parameters
        double sol_zenith,      // Solar zenith angle (degrees)
        double view_zenith,     // Viewing zenith angle (degrees)
        double azimuth          // Relative azimuth (degrees)
        )
{
    double zero = 1e-10;
//    double rd = M_PI/180;

    // Result is a length 4 vector:
    //      [,0] rsot:  bidirectional reflectance
    //      [,1] rdot:  directional reflectance for diffuse incidence
    //      [,2] rsdt:  diffuse reflectance for direct solar incidence
    //      [,3] rddt:  diffuse reflectance for diffuse incidence
    double rsot, rdot, rsdt, rddt;
    NumericVector result(4);

    NumericVector li = NumericVector::create(
            5, 15, 25, 35, 45, 55, 65, 75, 81, 83, 85, 87, 89
            );
    int nli = li.size();

    // Angular factors
    double cos_SZ = cos(rd * sol_zenith);
    double cos_VZ = cos(rd * view_zenith);
    double cos_SZVZ = cos_SZ * cos_VZ;
    double tan_SZ = tan(rd * sol_zenith);
    double tan_VZ = tan(rd * view_zenith);
    double cos_azimuth = cos(rd * azimuth);
    double sunsensor_angle = sqrt(tan_SZ*tan_SZ + tan_VZ*tan_VZ - 2 * tan_SZ * tan_VZ * cos_azimuth);


    // Clumping effects
    double Cs = 1;
    double Co = 1;
    if (crown_cover < 1) {
        Cs = 1 - pow(1-crown_cover, 1.0/cos_SZ);
        Co = 1 - pow(1-crown_cover, 1.0/cos_VZ);
    }
    double Overlap = 0;
    if (zeta > 0){
        Overlap = min(Cs * (1-Co), Co*(1-Cs)) * exp(-sunsensor_angle/zeta);
    }
    double Fcd = Cs * Co + Overlap;
    double Fcs = (1-Cs) * Co - Overlap;
    double Fod = Cs * (1-Co) - Overlap;
    double Fos = (1-Cs) * (1-Co) + Overlap;
    double Fcdc = 1 - pow(1-Fcd, 0.5/cos_SZ + 0.5/cos_VZ);

    // Reflectance for first layer
    //      Based on f_brown : fraction of brown leaves 
    //      If f_brown is zero or one, set refl/trans green to brown
    //          (or vice versa) and f_brown to 0.5 to simply average

    if (f_brown < zero){
        f_brown = 0.5;
        rho_b = rho_g;
        tau_b = tau_g;
    }
    else if (f_brown > 1-zero){
        f_brown = 0.5;
        rho_g = rho_b;
        tau_g = tau_b;
    }

    double s = (1 - diss) * f_brown * (1 - f_brown);

    double rho1 = ((1 - f_brown - s) * rho_g + s * rho_b) / (1 - f_brown);
    double tau1 = ((1 - f_brown - s) * tau_g + s * tau_b) / (1 - f_brown);
    double rho2 = (s*rho_g + (f_brown-s) * rho_b) / f_brown;
    double tau2 = (s*tau_g + (f_brown-s) * tau_b) / f_brown;
    
    // Generate LIDF from input parameters
    NumericVector lidf = ladgen(LIDFa, LIDFb);

    // Geometric factors for scattering
    //      Initialize sums
    double ks = 0;
    double ko = 0;
    double bf = 0;
    double sob = 0;
    double sof = 0;

    // Weighted sums over LIDF
    double ttl, ctl, chi_s, chi_o, frho, ftau,
           ksli, koli, sobli, sofli, bfli;

    for(int i=0; i<nli; i++){
        ttl = li[i];
        ctl = cos(rd*ttl);

        // Volume scattering function 
        volscatt(sol_zenith, view_zenith, azimuth, ttl,
                chi_s, chi_o, frho, ftau);

        // Extinction coefficients
        ksli = chi_s / cos_SZ;
        koli = chi_o / cos_VZ;

        // Area scattering coefficient fractions
        sobli = frho * M_PI / cos_SZVZ;
        sofli = ftau * M_PI / cos_SZVZ;
        bfli = ctl * ctl;

        ks = ks + ksli * lidf[i];
        ko = ko + koli * lidf[i];
        bf = bf + bfli * lidf[i];
        sob = sob + sobli * lidf[i];
        sof = sof + sofli * lidf[i];
    }

    // Geometric factors to be used later in combination with rho and tau
    double sdb = 0.5 * (ks + bf);
    double sdf = 0.5 * (ks - bf);
    double dob = 0.5 * (ko + bf);
    double dof = 0.5 * (ko - bf);
    double ddb = 0.5 * (1 + bf);
    double ddf = 0.5 * (1 - bf);

    // LAI in two layers
    double LAI_1 = (1 - f_brown) * LAI;     // Top layer
    double LAI_2 = f_brown * LAI;           // Bottom layer


    // Hotspot effect for 2 layers

    double tau_ss = exp(-ks * LAI);         // Transmittance through canopy
    double tau_ss_1 = exp(-ks * LAI_1);     // Transmittance through top canopy layer

    double alf = 1;
    if (hot > 0) {
        alf = (sunsensor_angle / hot) * 2 / (ks + ko);
        alf = min(alf, 200.0);
    }

    double tau_ssoo, s1, s2;        // tau_ssoo :   Bidirectional gap fraction
    if (alf < zero){    
        // Pure hotspot
        tau_ssoo = tau_ss;
        s1 = (1 - tau_ss_1) / (ks * LAI);
        s2 = (tau_ss_1 - tau_ss) / (ks * LAI);
    }
    else {
        // Outside the hotspot
        double fhot = LAI * sqrt(ko * ks);

        //  Integrate 2 layers by exponential simpson method in 20 steps
        //	the steps are arranged according to equal partitioning
        //	of the derivative of the joint probability function
        int nstep = 20;
        double x1, y1, f1, x2, y2, f2, ca, fint;

        x1 = 0;
        y1 = 0;
        f1 = 1;
        ca = exp(alf * (f_brown - 1.0));
        fint = (1 - ca) * 0.05;
        s1 = 0;

        for (int i = 0; i<nstep; i++){
            if (i < (nstep - 1)){
                x2 = -log(1 - i * fint) / alf;
            } else {
                x2 = 1 - f_brown;
            }
            y2 = -(ko + ks) * LAI * x2 + fhot * (1 - exp(-alf*x2)) / alf;
            f2 = exp(y2);
            s1 = s1 + (f2 - f1) * (x2 -x1) / (y2 - y1);
            x1 = x2;
            y1 = y2;
            f1 = f2;
        }

        fint = (ca - exp(-alf)) * 0.05;
        s2 = 0;

        for (int i = 0; i<nstep; i++){
            if (i < (nstep - 1)){
                x2 = -log(ca - i * fint) / alf;
            } else {
                x2 = 1;
            }
            y2 = -(ko + ks) * LAI * x2 + fhot * (1-exp(-alf*x2)) / alf;
            f2 = exp(y2);
            s2 = s2 + (f2 - f1) * (x2 - x1) / (y2 - y1);
            x1 = x2;
            y1 = y2;
            f1 = f2;
        }

        tau_ssoo = f1;
    }
    
    // Calculate reflectance and transmittance
    // Bottom layer
    double tau_oo, w1, w2, tdd, rdd, tsd, rsd, tdo, rdo, rsod;
    refl_trans(rho2, tau2, LAI_2,
            ks, ko, sdb, sdf, dob, dof, sob, sof, ddb, ddf,
            tau_ss, tau_oo, w2, tdd, rdd, tsd, rsd, tdo, rdo, rsod);
    
    // Set background properties equal to those of the bottom layer on a black soil
    double rddb = rdd;
    double rsdb = rsd;
    double rdob = rdo;
    double rsodb = rsod;
    double tddb = tdd;
    double tsdb = tsd;
    double tdob = tdo;
    double toob = tau_oo;
    double tssb = tau_ss;

    // Top layer
    refl_trans(rho1, tau1, LAI_1,
            ks, ko, sdb, sdf, dob, dof, sob, sof, ddb, ddf,
            tau_ss, tau_oo, w1, tdd, rdd, tsd, rsd, tdo, rdo, rsod);


    // Combine with bottom layer reflectances and transmittances (adding method)

    double rn = 1-rdd*rddb;
    double tup = (tau_ss*rsdb + tsd*rddb)/rn;
    double tdn = (tsd + tau_ss*rsdb*rdd)/rn;
    rsdt = rsd + tup*tdd;
    rdot = rdo + tdd*(rddb*tdo + rdob*tau_oo)/rn;

    double rsodt = rsod + (tau_ss*rsodb + tdn*rdob)*tau_oo + tup*tdo;
    double rsost = (w1*s1 + w2*s2)*LAI;
    rsot = rsost + rsodt;

    // Diffuse reflectances at the top and the bottom are now different 

    double rddt_t = rdd + tdd*rddb*tdd/rn;
    double rddt_b = rddb + tddb*rdd*tddb/rn;

    // Transmittances of the combined canopy layers

    double tsst = tau_ss*tssb;
    double toot = tau_oo*toob;
    double tsdt = tau_ss*tsdb + tdn*tddb;
    double tdot = tdob*tau_oo + tddb*(tdo + rdd*rdob*tau_oo)/rn;
    double tddt = tdd*tddb/rn;

    // Apply clumping effects to vegetation layer

    double rddcb = crown_cover*rddt_b;
    double rddct = crown_cover*rddt_t;
    double tddc = 1 - crown_cover + crown_cover*tddt;
    double rsdc = Cs*rsdt;
    double tsdc = Cs*tsdt;
    double rdoc = Co*rdot;
    double tdoc = Co*tdot;
    double tssc = 1 - Cs + Cs*tsst;
    double tooc = 1 - Co + Co*toot;

    // New weight function Fcdc for crown contribution (W. Verhoef, 22-05-08)

    double rsoc = Fcdc*rsot;
    double tssooc = Fcd*tau_ssoo + Fcs*toot + Fod*tsst + Fos;

    // Canopy absorptance for black background (W. Verhoef, 02 - 03 - 04)

    double alfas = 1 - tssc - tsdc - rsdc;
    double alfad = 1 - tddc - rddct;

    // Add the soil background 

    rn = 1 - rddcb*soil_rdd;
    tup = (tssc*soil_rsd + tsdc*soil_rdd)/rn;
    tdn = (tsdc + tssc*soil_rsd*rddcb)/rn;

    rddt = rddct + tddc*soil_rdd*tddc/rn;
    rsdt = rsdc + tup*tddc;
    rdot = rdoc + tddc*(soil_rdd*tdoc + soil_rdo*tooc)/rn;
    rsot = rsoc + tssooc*soil_rso + tdn*soil_rdo*tooc + tup*tdoc;

    // Effect of soil background on canopy absorptances (W. Verhoef, 02-03-04)
    /*      Not sure why this is in here because it isn't returned...?

    double alfast = alfas + tup*alfad;
    double alfadt = alfad*(1. + tddc*soil_rdd/rn);
    */

    /* Before returning, save current input parameters as old ones

       LAI_old=LAI;
       LIDFa_old=LIDFa;
       LIDFb_old=LIDFb;
       fb_old=f_brown;
       hot_old=hot;
       Cv_old=crown_cover;
       zeta_old=zeta;
       tts_old=sol_zenith;
       tto_old=view_zenith;
       psi_old=azimuth;

       ifirst=0;
       */
    result = NumericVector::create(rsot, rdot, rsdt, rddt);
    return result;
}

// [[Rcpp::export]]
NumericVector fs(NumericVector param){
    double a = param[0], b = param[1], c = param[2], d=param[3],
           e = param[4], f=param[5], g=param[6], h=param[7], i=param[8],
           j=param[9], k=param[10], l=param[11], m=param[12], n=param[13],
           o=param[14], p=param[15], q=param[16], r=param[17], s=param[18];

    return FourSAIL2_cpp(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s);
}

