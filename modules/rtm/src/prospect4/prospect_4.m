% _______________________________________________________________________
%
% prospect_4.m
% version 4 (february, 21th 2008)
% subroutines required: tav.m, dataSpec_P4.m
% _______________________________________________________________________
%
% Plant leaf reflectance and transmittance are calculated from 400 nm to
% 2500 nm (1 nm step) with the following parameters:
%
%       - N   = leaf structure parameter
%       - Cab = chlorophyll a+b content in µg/cm²
%       - Cw  = equivalent water thickness in g/cm² or cm
%       - Cm  = dry matter content in g/cm²
%
% Here are some examples observed during the LOPEX'93 experiment on
% fresh (F) and dry (D) leaves :
%
% ---------------------------------------------
%                N     Cab     Cw        Cm    
% ---------------------------------------------
% min          1.000    0.0  0.004000  0.001900
% max          3.000  100.0  0.040000  0.016500
% corn (F)     1.518   58.0  0.013100  0.003662
% rice (F)     2.275   23.7  0.007500  0.005811
% clover (F)   1.875   46.7  0.010000  0.003014
% laurel (F)   2.660   74.1  0.019900  0.013520
% ---------------------------------------------
% min          1.500    0.0  0.000063  0.0019
% max          3.600  100.0  0.000900  0.0165
% bamboo (D)   2.698   70.8  0.000117  0.009327
% lettuce (D)  2.107   35.2  0.000244  0.002250
% walnut (D)   2.656   62.8  0.000263  0.006573
% chestnut (D) 1.826   47.7  0.000307  0.004305
% ---------------------------------------------
% _______________________________________________________________________

function LRT=prospect_4(N,Cab,Cw,Cm)

% ***********************************************************************
% Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G.,
% Hosgood B. (1996), Estimating leaf biochemistry using the PROSPECT
% leaf optical properties model, Remote Sens. Environ., 56:194-202.
% Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical
% properties spectra, Remote Sens. Environ., 34:75-91.
% Féret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical
% Properties Model Separating Photosynthetic Pigments, Remote Sensing of
% Environment
% ***********************************************************************

data=dataSpec_P4;
l=data(:,1);
n=data(:,2);
k=(Cab*data(:,3)+Cw*data(:,5)+Cm*data(:,6))./N;
k(find(k==0))=eps;
trans=(1-k).*exp(-k)+k.^2.*expint(k);

% ***********************************************************************
% reflectance and transmittance of one layer
% ***********************************************************************
% Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969),
% Interaction of isotropic ligth with a compact plant leaf, J. Opt.
% Soc. Am., 59(10):1376-1379.
% ***********************************************************************

% reflectivity and transmissivity at the interface
%-------------------------------------------------
alpha=40;
t12=tav(alpha,n);
t21=tav(90,n)./n.^2;
r12=1-t12;
r21=1-t21;
x=tav(alpha,n)./tav(90,n);
y=x.*(tav(90,n)-1)+1-tav(alpha,n);

% reflectance and transmittance of the elementary layer N = 1
%------------------------------------------------------------

ra=r12+(t12.*t21.*r21.*trans.^2)./(1-r21.^2.*trans.^2);
ta=(t12.*t21.*trans)./(1-r21.^2.*trans.^2);
r90=(ra-y)./x;
t90=ta./x;

% ***********************************************************************
% reflectance and transmittance of N layers
% ***********************************************************************
% Stokes G.G. (1862), On the intensity of the light reflected from
% or transmitted through a pile of plates, Proc. Roy. Soc. Lond.,
% 11:545-556.
% ***********************************************************************

delta=sqrt((t90.^2-r90.^2-1).^2-4*r90.^2);
beta=(1+r90.^2-t90.^2-delta)./(2*r90);
va=(1+r90.^2-t90.^2+delta)./(2*r90);
if (va.*(beta-r90)<=1e-14)
    vb=sqrt(beta.*(va-r90)./(1e-14));
else
    vb=sqrt(beta.*(va-r90)./(va.*(beta-r90)));
end
vbNN = vb.^(N-1);
vbNNinv = 1./vbNN;
vainv = 1./va;
s1=ta.*t90.*(vbNN-vbNNinv);
s2=ta.*(va-vainv);
s3=va.*vbNN-vainv.*vbNNinv-r90.*(vbNN-vbNNinv);

RN=ra+s1./s3;
TN=s2./s3;
LRT=[l RN TN];