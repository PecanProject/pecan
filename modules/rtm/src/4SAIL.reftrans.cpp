#include <Rcpp.h>
#include "sail_common.h"
using namespace Rcpp;

// Calculate reflectance and transmittance
void refl_trans(
        // Inputs
        double rho,
        double tau,
        double LAI,
        double ks,      // Geometric factors ... 
        double ko,
        double sdb,         
        double sdf,
        double dob,
        double dof,
        double sob,
        double sof,
        double ddb,
        double ddf,
        
        // Outputs
        double &tau_ss,
        double &tau_oo,
        double &w,
        double &tdd,
        double &rdd,
        double &tsd,
        double &rsd,
        double &tdo,
        double &rdo,
        double &rsod
        )
{
    tau_ss = exp(-ks * LAI);
    tau_oo = exp(-ko * LAI);

    double sb = sdb*rho + sdf*tau;
    double sf = sdf*rho + sdb*tau;

    double vb = dob*rho + dof*tau;
    double vf = dof*rho + dob*tau;

    w = sob*rho + sof*tau;

    double sigb = ddb*rho + ddf*tau;
    double sigf = ddf*rho + ddb*tau;
    double att = 1 - sigf;

    double m2 = (att+sigb) * (att - sigb);

    if(m2 < 0) m2 = 0;
    double m = sqrt(m2);

    if (m > 0.01){

        // Normal case
        double e1 = exp(-m * LAI);
        double e2 = e1*e1;
        double rinf = (att - m) / sigb;
        double rinf2 = rinf*rinf;
        double re = rinf * e1;
        double denom = 1 - rinf2 * e2;

        double J1ks = Jfunc1(ks, m, LAI);
        double J2ks = Jfunc2(ks, m, LAI);
        double J1ko = Jfunc1(ko, m, LAI);
        double J2ko = Jfunc2(ko, m, LAI);

        double Ps=(sf+sb*rinf)*J1ks;
        double Qs=(sf*rinf+sb)*J2ks;
        double Pv=(vf+vb*rinf)*J1ko;
        double Qv=(vf*rinf+vb)*J2ko;

        tdd=(1-rinf2)*e1/denom;
        rdd=rinf*(1-e2)/denom;
        tsd=(Ps-re*Qs)/denom;
        rsd=(Qs-re*Ps)/denom;
        tdo=(Pv-re*Qv)/denom;
        rdo=(Qv-re*Pv)/denom;

        double z=Jfunc2(ks,ko,LAI);
        double g1=(z-J1ks*tau_oo)/(ko+m);
        double g2=(z-J1ko*tau_ss)/(ks+m);

        double Tv1=(vf*rinf+vb)*g1;
        double Tv2=(vf+vb*rinf)*g2;

        double T1=Tv1*(sf+sb*rinf);
        double T2=Tv2*(sf*rinf+sb);
        double T3=(rdo*Qs+tdo*Ps)*rinf;

        // Multiple scattering contribution to bidirectional canopy reflectance

        rsod=(T1+T2-T3)/(1-rinf2);

    } else {

        //Near or complete conservative scattering

        double J3=Jfunc3(m,LAI);
        double amsig=att-sigb;
        double apsig=att+sigb;
        double rtp=(1-amsig*J3)/(1+amsig*J3);
        double rtm=(-1+apsig*J3)/(1+apsig*J3);
        double rdd=.5*(rtp+rtm);
        double tdd=.5*(rtp-rtm);

        double dns=ks*ks-m*m;
        double dno=ko*ko-m*m;
        double cks=(sb*(ks-att)-sf*sigb)/dns;
        double cko=(vb*(ko-att)-vf*sigb)/dno;
        double dks=(-sf*(ks+att)-sb*sigb)/dns;
        double dko=(-vf*(ko+att)-vb*sigb)/dno;
        double ho=(sf*cko+sb*dko)/(ko+ks)				;

        rsd=cks*(1-tau_ss*tdd)-dks*rdd;
        rdo=cko*(1-tau_oo*tdd)-dko*rdd;
        tsd=dks*(tau_ss-tdd)-cks*tau_ss*rdd;
        tdo=dko*(tau_oo-tdd)-cko*tau_oo*rdd;

        // Multiple scattering contribution to bidirectional canopy reflectance

        rsod=ho*(1-tau_ss*tau_oo)-cko*tsd*tau_oo-dko*rsd;
    }
    return ;
}

