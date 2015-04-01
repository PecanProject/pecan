#include <Rcpp.h>
#include "sail_common.h"
using namespace Rcpp;

//4SAIL model support functions

double min(double a, double b){
    if(a < b) {
        return a;
    } else {
        return b;
    }
}

// J1 function with avoidance of singularity problem
double Jfunc1(double k, double l, double t){
    double del = (k-l)*t;
    if (abs(del) > 1e-3){ 
        return (exp(-l*t)-exp(-k*t))/(k-l);
    } else {
        return 0.5*t*(exp(-k*t)+exp(-l*t))*(1.-del*del/12.);
    }
}

// J2 function
double Jfunc2(double k, double l, double t){
    return (1.-exp(-(k+l)*t))/(k+l);
}

// J3 function for treating (near) conservative scattering
double Jfunc3(double m, double t){
    double del, e;
    del=m*t;
    if (del > 1e-3){
        e=exp(-del);
        return (1-e)/(m*(1+e));
    } else {
        return 0.5*t*(1.-del*del/12.);
    }
}

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
// [Rcpp::export]]
NumericVector fourSAIL2_cpp(
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
        double green_r,         // Green leaf reflectance
        double brown_r,         // Brown leaf reflectance
        double green_t,         // Green leaf transmittance
        double brown_t,         // Brown leaf transmittance

        // Observational parameters
        double sol_zenith,      // Solar zenith angle (degrees)
        double view_zenith,     // Viewing zenith angle (degrees)
        double azimuth          // Relative azimuth (degrees)
        )
{
    double zero = 1e-10;
    double rd = 180/M_PI;

    // Result is a length 4 vector:
    //      [,0] rsot:  bidirectional reflectance
    //      [,1] rdot:  directional reflectance for diffuse incidence
    //      [,2] rsdt:  diffuse reflectance for direct solar incidence
    //      [,3] rddt:  diffuse reflectance for diffuse incidence
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
        brown_r = green_r;
        brown_t = green_t;
    }
    else if (f_brown > 1-zero){
        f_brown = 0.5;
        green_r = brown_r;
        green_t = brown_t;
    }

    double s = (1 - diss) * f_brown * (1 - f_brown);

    double rho1 = ((1 - f_brown - s) * green_r + s * brown_r) / (1 - f_brown);
    double tau1 = ((1 - f_brown - s) * green_t + s * brown_t) / (1 - f_brown);
    double rho2 = (s*green_r + (f_brown-s) * brown_r) / f_brown;
    double tau2 = (s*green_t + (f_brown-s) * brown_r) / f_brown;

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
    NumericVector vs_vec(4);

    for(int i=0; i<nli; i++){
        ttl = li[i];
        ctl = cos(rd*ttl);

        // Volume scattering function 
        vs_vec = volscatt(sol_zenith, view_zenith, azimuth, ttl);
        chi_s = vs_vec[0];
        chi_o = vs_vec[1];
        frho = vs_vec[2];
        ftau = vs_vec[3];

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

    if (alf < zero){                // No hotspot effect -- simple case
        tssto = tau_ss;
        s1 = (1 - tau_ss_1) / (ks * LAI);
        s2 = (tau_ss_1 - tau_ss) / (ks * LAI);
    }
    else {                          // Yes -- hotspot effect
        // Outside the hotspot
        fhot = LAI * sqrt(ko * ks);

        //  Integrate 2 layers by exponential simpson method in 20 steps
        //	the steps are arranged according to equal partitioning
        //	of the derivative of the joint probability function
        int nstep = 20;

        x1 = 0;
        y1 = 0;
        f1 = 0;
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

        tsstoo = f1;
    }

    // Calculate reflectance and transmittance

    // Bottom layer

    tau_ss = exp(-ks * LAI_2);
    too = exp(-ko * LAI_2);

    sb = sdb*rho2 + sdf*tau2;
    sf = sdf*rho2 + sdb*tau2;

    vb = dob*rho2 + dof*tau2;
    vf = dof*rho2 + dob*tau2;

    w2 = sob*rho2 + sof*tau2;

    sigb = ddb*rho2 + ddf*tau2;
    sigf = ddf*rho2 + ddb*tau2;
    att = 1 - sigf;

    m2 = (att+sigb) * (att - sigb);
    if(m2 < 0) m2 = 0;
    m = sqrt(m2);

    if (m > 0.01){
        // Normal case
        e1 = exp(-m * LAI_2);
        e2 = e1*e1;
        rinf = (att - m) / sigb;
        rinf2 = rinf*rinf;
        re = rinf * e1;
        denom = 1 - rinf2 * e2;

        J1ks = Jfunc1(ks, m ,LAI_2);
        J2ks = Jfunc2(ks, m, LAI_2);
        J1ko = Jfunc1(ko, m, LAI_2);
        J2ko = Jfunc2(ko, m, LAI_2);

        Ps=(sf+sb*rinf)*J1ks;
        Qs=(sf*rinf+sb)*J2ks;
        Pv=(vf+vb*rinf)*J1ko;
        Qv=(vf*rinf+vb)*J2ko;

        tdd=(1-rinf2)*e1/denom;
        rdd=rinf*(1-e2)/denom;
        tsd=(Ps-re*Qs)/denom;
        rsd=(Qs-re*Ps)/denom;
        tdo=(Pv-re*Qv)/denom;
        rdo=(Qv-re*Pv)/denom;

        z=Jfunc2(ks,ko,LAI_2);
        g1=(z-J1ks*too)/(ko+m);
        g2=(z-J1ko*tau_ss)/(ks+m);

        Tv1=(vf*rinf+vb)*g1;
        Tv2=(vf+vb*rinf)*g2;

        T1=Tv1*(sf+sb*rinf);
        T2=Tv2*(sf*rinf+sb);
        T3=(rdo*Qs+tdo*Ps)*rinf;

        // Multiple scattering contribution to bidirectional canopy reflectance

        rsod=(T1+T2-T3)/(1-rinf2);

    } else {

        //Near or complete conservative scattering

        J3=Jfunc3(m,LAI_2);
        amsig=att-sigb;
        apsig=att+sigb;
        rtp=(1-amsig*J3)/(1+amsig*J3);
        rtm=(-1+apsig*J3)/(1+apsig*J3);
        rdd=.5*(rtp+rtm);
        tdd=.5*(rtp-rtm);

        dns=ks*ks-m*m;
        dno=ko*ko-m*m;
        cks=(sb*(ks-att)-sf*sigb)/dns;
        cko=(vb*(ko-att)-vf*sigb)/dno;
        dks=(-sf*(ks+att)-sb*sigb)/dns;
        dko=(-vf*(ko+att)-vb*sigb)/dno;
        ho=(sf*cko+sb*dko)/(ko+ks)				;

        rsd=cks*(1-tau_ss*tdd)-dks*rdd;
        rdo=cko*(1-too*tdd)-dko*rdd;
        tsd=dks*(tau_ss-tdd)-cks*tau_ss*rdd;
        tdo=dko*(too-tdd)-cko*too*rdd;

        // Multiple scattering contribution to bidirectional canopy reflectance

        rsod=ho*(1-tau_ss*too)-cko*tsd*too-dko*rsd;
    }

    // Set background properties equal to those of the bottom layer on a black soil

    rddb=rdd;
    rsdb=rsd;
    rdob=rdo;
    rsodb=rsod;
    tddb=tdd;
    tsdb=tsd;
    tdob=tdo;
    toob=too;
    tssb=tau_ss;

    // Top layer
    // *** Same code as above, starting at l.286, 
    // *** but with rho1/tau1 instead of rho2/tau2

    tau_ss=exp(-ks*LAI_1);
    too=exp(-ko*LAI_1);

    sb=sdb*rho1+sdf*tau1;
    sf=sdf*rho1+sdb*tau1;

    vb=dob*rho1+dof*tau1;
    vf=dof*rho1+dob*tau1;

    w1=sob*rho1+sof*tau1;

    sigb=ddb*rho1+ddf*tau1;
    sigf=ddf*rho1+ddb*tau1;
    att=1-sigf;

    m2=(att+sigb)*(att-sigb);
    if (m2 < 0) m2=0;
    m=sqrt(m2);

    if (m > 0.01) {

        // Normal case

        e1=exp(-m*LAI_1);
        e2=e1*e1;
        rinf=(att-m)/sigb;
        rinf2=rinf*rinf;
        re=rinf*e1;
        denom=1.-rinf2*e2;

        J1ks=Jfunc1(ks,m,LAI_1);
        J2ks=Jfunc2(ks,m,LAI_1);
        J1ko=Jfunc1(ko,m,LAI_1);
        J2ko=Jfunc2(ko,m,LAI_1);

        Ps=(sf+sb*rinf)*J1ks;
        Qs=(sf*rinf+sb)*J2ks;
        Pv=(vf+vb*rinf)*J1ko;
        Qv=(vf*rinf+vb)*J2ko;

        tdd=(1.-rinf2)*e1/denom;
        rdd=rinf*(1.-e2)/denom;
        tsd=(Ps-re*Qs)/denom;
        rsd=(Qs-re*Ps)/denom;
        tdo=(Pv-re*Qv)/denom;
        rdo=(Qv-re*Pv)/denom;

        z=Jfunc2(ks,ko,LAI_1);
        g1=(z-J1ks*too)/(ko+m);
        g2=(z-J1ko*tau_ss)/(ks+m);

        Tv1=(vf*rinf+vb)*g1;
        Tv2=(vf+vb*rinf)*g2;

        T1=Tv1*(sf+sb*rinf);
        T2=Tv2*(sf*rinf+sb);
        T3=(rdo*Qs+tdo*Ps)*rinf;

        // Multiple scattering contribution to bidirectional canopy reflectance

        rsod=(T1+T2-T3)/(1.-rinf2);

    } else {

        // Near or complete conservative scattering

        J3=Jfunc3(m,LAI_1);
        amsig=att-sigb;
        apsig=att+sigb;
        rtp=(1-amsig*J3)/(1+amsig*J3);
        rtm=(-1+apsig*J3)/(1+apsig*J3);
        rdd=.5*(rtp+rtm);
        tdd=.5*(rtp-rtm);

        dns=ks*ks-m*m;
        dno=ko*ko-m*m;
        cks=(sb*(ks-att)-sf*sigb)/dns;
        cko=(vb*(ko-att)-vf*sigb)/dno;
        dks=(-sf*(ks+att)-sb*sigb)/dns;
        dko=(-vf*(ko+att)-vb*sigb)/dno;
        ho=(sf*cko+sb*dko)/(ko+ks)				;

        rsd=cks*(1-tau_ss*tdd)-dks*rdd;
        rdo=cko*(1-too*tdd)-dko*rdd;
        tsd=dks*(tau_ss-tdd)-cks*tau_ss*rdd;
        tdo=dko*(too-tdd)-cko*too*rdd;

        // Multiple scattering contribution to bidirectional canopy reflectance

        rsod=ho*(1-tau_ss*too)-cko*tsd*too-dko*rsd;
    }

    // Combine with bottom layer reflectances and transmittances (adding method)

    rn=1-rdd*rddb;
    tup=(tau_ss*rsdb+tsd*rddb)/rn;
    tdn=(tsd+tau_ss*rsdb*rdd)/rn;
    rsdt=rsd+tup*tdd;
    rdot=rdo+tdd*(rddb*tdo+rdob*too)/rn;
    rsodt=rsod+(tau_ss*rsodb+tdn*rdob)*too+tup*tdo;

    rsost=(w1*s1+w2*s2)*LAI;

    rsot=rsost+rsodt;

    // Diffuse reflectances at the top and the bottom are now different 

    rddt_t=rdd+tdd*rddb*tdd/rn;
    rddt_b=rddb+tddb*rdd*tddb/rn;

    // Transmittances of the combined canopy layers

    tsst=tau_ss*tssb;
    toot=too*toob;
    tsdt=tau_ss*tsdb+tdn*tddb;
    tdot=tdob*too+tddb*(tdo+rdd*rdob*too)/rn;
    tddt=tdd*tddb/rn;

    // Apply clumping effects to vegetation layer

    rddcb=crown_cover*rddt_b;
    rddct=crown_cover*rddt_t;
    tddc=1-crown_cover+crown_cover*tddt;
    rsdc=Cs*rsdt;
    tsdc=Cs*tsdt;
    rdoc=Co*rdot;
    tdoc=Co*tdot;
    tssc=1-Cs+Cs*tsst;
    tooc=1-Co+Co*toot;

    // New weight function Fcdc for crown contribution (W. Verhoef, 22-05-08)

    rsoc=Fcdc*rsot;
    tssooc=Fcd*tsstoo+Fcs*toot+Fod*tsst+Fos;

    // Canopy absorptance for black background (W. Verhoef, 02-03-04)

    alfas=1-tssc-tsdc-rsdc;
    alfad=1-tddc-rddct;

    // Add the soil background 

    rn=1-rddcb*soil_rdd;
    tup=(tssc*soil_rsd+tsdc*soil_rdd)/rn;
    tdn=(tsdc+tssc*soil_rsd*rddcb)/rn;

    rddt=rddct+tddc*soil_rdd*tddc/rn;
    rsdt=rsdc+tup*tddc;
    rdot=rdoc+tddc*(soil_rdd*tdoc+soil_rdo*tooc)/rn;
    rsot=rsoc+tssooc*soil_rso+tdn*soil_rdo*tooc+tup*tdoc;

    // Effect of soil background on canopy absorptances (W. Verhoef, 02-03-04)

    alfast=alfas+tup*alfad;
    alfadt=alfad*(1.+tddc*soil_rdd/rn);

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

    return fourSAIL2_cpp(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s);
}

