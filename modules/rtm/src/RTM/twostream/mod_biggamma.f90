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
