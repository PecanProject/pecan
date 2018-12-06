! ********************************************************************************
! prospect.D.f90
! ********************************************************************************
!
! Adapted by Alexey Shiklomanov
! Department of Earth and Environment, Boston University
! E-mail: ashiklom@bu.edu
!
! Original author:
! Jean-Baptiste FERET
! 
! Irtsea
! UMR TETIS
! Maison de la Teledetection
! 500 Rue Jean Francois Breton
! 3400 Montpellier, France
! E-mail: jean-baptiste.feret@teledetection.fr
!
! http://teledetection.ipgp.fr/prosail/
!
!
!
! ********************************************************************************
! Féret, J.-B., Gitelson, A.A., Noble, S.D., Jacquemoud, S., (2017), PROSPECT-D: 
! Towards modeling leaf optical properties through a complete lifecycle. Remote 
! Sensing of Environment 193, 204-215. doi:10.1016/j.rse.2017.03.004
!
! Féret J.B., François C., Asner G.P., Gitelson A.A., Martin R.E., Bidel L.P.R.,
! Ustin S.L., le Maire G., Jacquemoud S. (2008), PROSPECT-4 and 5: Advances in the
! leaf optical properties model separating photosynthetic pigments, Remote Sensing
! of Environment, 112:3030-3043.
!
! Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B.
! (1996), Estimating leaf biochemistry using the PROSPECT leaf optical properties
! model, Remote Sensing of Environment, 56:194-202.
!
! Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical properties
! spectra, Remote Sensing of Environment, 34:75-91.
! ********************************************************************************
! version (11 April 2017)
! ********************************************************************************

SUBROUTINE prospect_d(N,Cab,Car,Canth,Cbrown,Cw,Cm,RT)

    USE MOD_dataSpec_wavelength
    USE MOD_dataSpec_prospectD
    IMPLICIT none

    REAL*8, intent(in) :: N,Cab,Car,Canth,Cbrown,Cw,Cm
    REAL*8, intent(out) :: RT(nw,2)

    REAL*8 :: k(nw)

    k = (Cab*k_Cab+Car*k_Car+Canth*k_Canth+Cbrown*k_Cbrown+Cw*k_Cw+Cm*k_Cm)/N

    Call gpm(k,refractive,N,RT)

end subroutine
