!!!!!
! This subroutine performs a calculation of the incomplete Gamma 
! function required for calculating the transmission of isotropic 
! radiation. For more information on the derivation, see Pinty et al. 
! (2006) equations (16) and (17) and surrounding text.
!
!  Pinty, B., Lavergne, T., Dickinson, R.E., Widlowski, J.-L., Gobron, 
!  N., Verstraete, M.M., 2006. Simplifying the interaction of land 
!  surfaces with radiation for relating remote sensing products to 
!  climate models. J. Geophys. Res. 111, D02116.  
!  doi:10.1029/2005JD005952
!!!!!

module mod_biggamma
contains
function BigGamma(tau)
    implicit none
    real*8, intent(in) :: tau
    real*8 :: BigGamma

    integer :: order, i

    order = 20
    BigGamma = 0.0d0

    do i=order, 1, -1
        BigGamma = i / (1.0d0 + i / (tau + BigGamma))
    end do 

    BigGamma = 1.0d0 / (tau + BigGamma)
end function BigGamma
end module mod_biggamma
