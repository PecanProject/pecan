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
        real(kind=r2) :: hlog2pi, lsig, xm, s2, ex
        real(kind=r2) :: ldnorm

        hlog2pi = -0.39908993417 !! -0.5 * log(2pi)
        lsig = -log(sigma)
        xm = x - mu
        s2 = 2 * sigma * sigma
        ex = -xm * xm / s2
        ldnorm = hlog2pi + lsig + ex
        return
    end function

end module        
