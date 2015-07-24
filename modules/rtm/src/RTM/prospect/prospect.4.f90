! ********************************************************************************
! prospect.4.f90
! ********************************************************************************
!
! Adapted by Alexey Shiklomanov
! Department of Earth and Environment / Boston University
! E-mail: ashiklom@bu.edu
!
!
! Original authors:
! Jean-Baptiste FERET
! 
! Department of Global Ecology / Carnegie Institution for Sciences
! 260 Panama Street
! Stanford, CA 94305, USA
! E-mail: jbferet@stanford.edu
!
! Stéphane JACQUEMOUD
!
! Université Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue Hélène Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!
!
!
! ********************************************************************************
! Féret J.B., François C., Asner G.P., Gitelson A.A., Martin R.E., Bidel L.P.R.,
! Ustin S.L., le Maire G., Jacquemoud S. (2008), PROSPECT-4 and 5: Advances in the
! leaf optical properties model separating photosynthetic pigments, Remote Sensing
! of Environment, 112:3030-3043.
! Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B.
! (1996), Estimating leaf biochemistry using the PROSPECT leaf optical properties
! model, Remote Sensing of Environment, 56:194-202.
! Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical properties
! spectra, Remote Sensing of Environment, 34:75-91.
! ********************************************************************************
! version (15 May 2015)
! ********************************************************************************

SUBROUTINE prospect_4(N,Cab,Cw,Cm,RT)

    USE MOD_dataSpec_wavelength
    USE MOD_dataSpec_prospect5B
    IMPLICIT none

    REAL*8, intent(in) :: N,Cab,Cw,Cm
    REAL*8, intent(out) :: RT(nw,2)

    REAL*8 :: k(nw)

    k = (Cab*k_Cab+Cw*k_Cw+Cm*k_Cm)/N

    Call gpm(k,N,RT)

end subroutine
