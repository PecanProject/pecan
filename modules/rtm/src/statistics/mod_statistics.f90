module mod_statistics
    use mod_types
    use random
    implicit none

    contains 

    ! Random number generation
    function rnorm(mu, sigma)
        real(kind=r2), intent(in) :: mu, sigma
        real(kind=r2) :: z
        real(kind=r2) :: rnorm

        rnorm = mu + random_normal() * sigma
        return
    end function

    function rgamma(shp, scl)
        ! NOTE: random_gamma takes real(4) as argument for shape.
        ! Therefore, coerce real(8) (r2) argument `shp` to real(4) (r1)
        ! by copying.
        real(kind=r2), intent(in) :: shp, scl
        real(kind=r1) :: shp2 
        real(kind=r2) :: rgamma

        shp2 = shp
        rgamma = random_gamma(shp2, .true.) * scl
        return
    end function

    ! Density calculation
    function ldnorm(x, mu, sigma)
        real(kind=r2), intent(in) :: x, mu, sigma
        real(kind=r2) :: s2, xm, d1, d2, ex, exx, d
        real(kind=r2) :: pi
        real(kind=r2) :: ldnorm

        pi = 3.1415926539
        s2 = 2 * sigma * sigma
        xm = x - mu
        ex = -xm * xm / s2
        exx = exp(ex)
        d1 = sqrt(pi * s2)
        d2 = 1/d1
        d = d2 * exx
        ldnorm = log(d)
        return
    end function

end module        
