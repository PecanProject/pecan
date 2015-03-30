#include <Rcpp.h>
using namespace Rcpp;

//4SAIL model support functions

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
    double del;
    del=m*t;
    if (del > 1e-3){
        e=exp(-del);
        return (1-e)/(m*(1+e));
    } else {
        return 0.5*t*(1.-del*del/12.);
    }
}

    // 4SAIL2 Model
    //[Rcpp::export]]
    NumericMatrix FourSAIL2_cpp(){
        //// Input parameters ////
        double lai;             // Leaf area index
        double fb;              // Fraction brown LAI
        double diss;            // Dissosciation factor for brown/green [0,1]
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

        double zero = 1e-10;
        double PI = atan(1/4);
        double rd = 180/PI;

        // Result is a WL x 4 matrix:
        //      [,0] rsot:  bidirectional reflectance
        //      [,1] rdot:  directional reflectance for diffuse incidence
        //      [,2] rsdt:  diffuse reflectance for direct solar incidence
        //      [,3] rddt:  diffuse reflectance for diffuse incidence
        NumericMatrix result(nwl, 4);

        // Angular factors
        cts = cos(rd*tts);
        cto = cos(rd*tto);
        ctscto = cts*cto;
        tants = tan(rd * tts);
        tanto = tan(rd * tto);
        cspsi = cos(rd * psi);
        dso = sqrt(tants*tants + tanto*tanto - 2.0*tants*tanto*cspsi);

        // Clumping effects
        Cs = 1;
        Co = 1;
        if (Cv < 1) {
            Cs = 1 - pow(1-Cv, 1.0/cts);
            Co = 1 - pow(1-Cv, 1.0/cto);
        }

        Overlap = 0;
        if (zeta > 0){
            Overlap = min(Cs * (1-Co), Co*(1-Cs)) * exp(-dso/zeta);
        }

        Fcd = Cs * Co + Overlap;
        Fcs = (1-Cs) * Co - Overlap;
        Fod = Cs * (1-Co) - Overlap;
        Fos = (1-Cs) * (1-Co) + Overlap;

        Fcdc = 1 - pow(1-Fcd, 0.5/cts + 0.5/cto);

        // Reflectance for first layer
        //      Based on fb : fraction of brown leaves 
        //      If fb is zero or one, set refl/trans green to brown
        //          (or vice versa) and fb to 0.5 to simply average
        if (fb < zero){
            fb = 0.5;
            rb = rg;
            tb = tg;
        }
        else if (fb > 1-zero){
            fb = 0.5;
            rg = rb;
            tg = tb;
        }

        s = (1 - diss) * fb * (1 - fb);

        rho1 = ((1 - fb - s)*rg + s*rb) / (1 - fb);
        tau1 = ((1 - fb - s)*tg + s*tb) / (1 - fb);
        rho2 = (s*rg + (fb-s)*rb) / fb;
        tau2 = (s*tg + (fb-s)*rb) / fb;

        // Generate LIDF from (a,b) parameters
        //      External function
        lidf = ladgen(LIDFa, LIDFb);
        
        // Geometric factors for scattering
        // Initialize sums
        ks = 0;
        ko = 0;
        bf = 0;
        sob = 0;
        sof = 0;

        // Weighted sums over LIDF
        for(int i=0; i<nli; i++){
            ttl = li[i];
            ctl = cos(rd*ttl);

            // Volume scattering function (VOID - inputs 1-4, rest out)
            volscatt(tts, tto, psi, ttl, chi_s, chi_o, frho, ftau);

            // Extinction coefficients
            ksli = chi_s / cts;
            koli = chi_o / cto;

            // Area scattering coefficient fractions

            sobli = frho * PI / ctscto;
            sofli = ftau * PI / ctscto;

            bfli = ctl * ctl;

            ks = ks + ksli * lidf[i];
            ko = ko + koli * lidf[i];
            bf = bf + bfli * lidf[i];
            sob = sob + sobli * lidf[i];
            sof = sof + sofli * lidf[i];
        }

        // Geometric factors to be used later in combination with rho and tau
        sdb = 0.5 * (ks + bf);
        sdf = 0.5 * (ks - bf);
        dob = 0.5 * (ko + bf);
        dof = 0.5 * (ko - bf);
        ddb = 0.5 * (1 + bf);
        ddf = 0.5 * (1 - bf);

        // LAI in two layers
        lai1 = (1 - fbu) * lai;
        lai2 = fbu * lai;

        // Hotspot effect for 2 layers
        tss = exp(-ks * lai);
        ck = exp(-ks * lai1);

        alf = 1;
        if (hot > 0) {
            alf = (dso / hot) * 2 / (ks + ko);
            alf = min(alf, 200);
        }

        if (alf < zero){
            tssto = tss;
            s1 = (1 - ck) / (ks * lai);
            s2 = (ck - tss) / (ks * lai);
        }
        else {
            // Outside the hotspot
            fhot = lai * sqrt(ko * ks);

            //  Integrate 2 layers by exponential simpson method in 20 steps
            //	the steps are arranged according to equal partitioning
            //	of the derivative of the joint probability function
            nstep = 20;

            x1 = 0;
            y1 = 0;
            f1 = 0;
            ca = exp(alf * (fbu - 1.0));
            fint = (1 - ca) * 0.05;
            s1 = 0;

            for (int i = 0; i<nstep; i++){
                if (i < (nstep - 1)){
                    x2 = -log(1 - i * fint) / alf;
                } else {
                    x2 = 1 - fbu;
                }
                y2 = -(ko + ks) * lai * x2 + fhot * (1 - exp(-alf*x2)) / alf;
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
                y2 = -(ko + ks) * lai * x2 + fhot * (1-exp(-alf*x2)) / alf;
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

        tss = exp(-ks * lai2);
        too = exp(-ko * lai2);

        sb = sdb*rho2 + sdf*tau2;
        sf = sdf*rho2 + sdb*tau2;

        vb = dob*rho2 + dof*tau2;
        vf = dof*rho2 + dob*tau2;

        w2 = sob*rho2 + sof*tau2;

        sigb = ddb*rho2 + ddf*tau2;
        sigf = ddf*rho2 + ddb*tau2;
        att = 1 - sigf;

        m2 = max(0, (att+sigb) * (att - sigb));
        m = sqrt(m2);

        if (m > 0.01){
            // Normal case
            e1 = exp(-m * lai2);
            e2 = e1*e1;
            rinf = (att - m) / sigb;
            rinf2 = rinf*rinf;
            re = rinf * e1;
            denom = 1 - rinf2 * e2;

            J1ks = Jfunc1(ks, m ,lai2);
            J2ks = Jfunc2(ks, m, lai2);
            J1ko = Jfunc1(k0, m, lai2);
            J2ko = Jfunct2(ko, m, lai2);

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

            z=Jfunc2(ks,ko,lai2);
            g1=(z-J1ks*too)/(ko+m);
            g2=(z-J1ko*tss)/(ks+m);

            Tv1=(vf*rinf+vb)*g1;
            Tv2=(vf+vb*rinf)*g2;

            T1=Tv1*(sf+sb*rinf);
            T2=Tv2*(sf*rinf+sb);
            T3=(rdo*Qs+tdo*Ps)*rinf;

            // Multiple scattering contribution to bidirectional canopy reflectance

            rsod=(T1+T2-T3)/(1-rinf2);

        } else {

            //Near or complete conservative scattering

            J3=Jfunc3(m,lai2);
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

            rsd=cks*(1-tss*tdd)-dks*rdd;
            rdo=cko*(1-too*tdd)-dko*rdd;
            tsd=dks*(tss-tdd)-cks*tss*rdd;
            tdo=dko*(too-tdd)-cko*too*rdd;

            // Multiple scattering contribution to bidirectional canopy reflectance

            rsod=ho*(1-tss*too)-cko*tsd*too-dko*rsd;
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
        tssb=tss;

        // Top layer

        tss=exp(-ks*lai1);
        too=exp(-ko*lai1);

        sb=sdb*rho1+sdf*tau1;
        sf=sdf*rho1+sdb*tau1;

        vb=dob*rho1+dof*tau1;
        vf=dof*rho1+dob*tau1;

        w1=sob*rho1+sof*tau1;

        sigb=ddb*rho1+ddf*tau1;
        sigf=ddf*rho1+ddb*tau1;
        att=1.-sigf;

        m2=(att+sigb)*(att-sigb);
        if (m2 < 0){
            m2=0;
        }
        m=sqrt(m2);

        if (m > 0.01) {

            // Normal case

            e1=exp(-m*lai1);
            e2=e1*e1;
            rinf=(att-m)/sigb;
            rinf2=rinf*rinf;
            re=rinf*e1;
            denom=1.-rinf2*e2;

            J1ks=Jfunc1(ks,m,lai1);
            J2ks=Jfunc2(ks,m,lai1);
            J1ko=Jfunc1(ko,m,lai1);
            J2ko=Jfunc2(ko,m,lai1);

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

            z=Jfunc2(ks,ko,lai1);
            g1=(z-J1ks*too)/(ko+m);
            g2=(z-J1ko*tss)/(ks+m);

            Tv1=(vf*rinf+vb)*g1;
            Tv2=(vf+vb*rinf)*g2;

            T1=Tv1*(sf+sb*rinf);
            T2=Tv2*(sf*rinf+sb);
            T3=(rdo*Qs+tdo*Ps)*rinf;

            // Multiple scattering contribution to bidirectional canopy reflectance

            rsod=(T1+T2-T3)/(1.-rinf2);

        } else {

            // Near or complete conservative scattering

            J3=Jfunc3(m,lai1);
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

            rsd=cks*(1-tss*tdd)-dks*rdd;
            rdo=cko*(1-too*tdd)-dko*rdd;
            tsd=dks*(tss-tdd)-cks*tss*rdd;
            tdo=dko*(too-tdd)-cko*too*rdd;

            // Multiple scattering contribution to bidirectional canopy reflectance

            rsod=ho*(1-tss*too)-cko*tsd*too-dko*rsd;
        }

        // Combine with bottom layer reflectances and transmittances (adding method)

        rn=1.-rdd*rddb;
        tup=(tss*rsdb+tsd*rddb)/rn;
        tdn=(tsd+tss*rsdb*rdd)/rn;
        rsdt=rsd+tup*tdd;
        rdot=rdo+tdd*(rddb*tdo+rdob*too)/rn;
        rsodt=rsod+(tss*rsodb+tdn*rdob)*too+tup*tdo;

        rsost=(w1*s1+w2*s2)*lai;

        rsot=rsost+rsodt;

        // Diffuse reflectances at the top and the bottom are now different 

        rddt_t=rdd+tdd*rddb*tdd/rn;
        rddt_b=rddb+tddb*rdd*tddb/rn;

        // Transmittances of the combined canopy layers

        tsst=tss*tssb;
        toot=too*toob;
        tsdt=tss*tsdb+tdn*tddb;
        tdot=tdob*too+tddb*(tdo+rdd*rdob*too)/rn;
        tddt=tdd*tddb/rn;

        // Apply clumping effects to vegetation layer

        rddcb=Cv*rddt_b;
        rddct=Cv*rddt_t;
        tddc=1-Cv+Cv*tddt;
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

        rn=1-rddcb*rddsoil;
        tup=(tssc*rsdsoil+tsdc*rddsoil)/rn;
        tdn=(tsdc+tssc*rsdsoil*rddcb)/rn;

        rddt=rddct+tddc*rddsoil*tddc/rn;
        rsdt=rsdc+tup*tddc;
        rdot=rdoc+tddc*(rddsoil*tdoc+rdosoil*tooc)/rn;
        rsot=rsoc+tssooc*rsosoil+tdn*rdosoil*tooc+tup*tdoc;

        // Effect of soil background on canopy absorptances (W. Verhoef, 02-03-04)

        alfast=alfas+tup*alfad;
        alfadt=alfad*(1.+tddc*rddsoil/rn);

        // Before returning, save current input parameters as old ones

        lai_old=lai;
        LIDFa_old=LIDFa;
        LIDFb_old=LIDFb;
        fb_old=fb;
        hot_old=hot;
        Cv_old=Cv;
        zeta_old=zeta;
        tts_old=tts;
        tto_old=tto;
        psi_old=psi;

        ifirst=0;
        return ;
    }

