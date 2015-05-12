! Predefined priors for PROSPECT 5B
subroutine p5b_prior(par, x, d)
    use mod_types
    use mod_statistics
    
    ! Inputs
    integer(kind=i1), intent(in) :: par
    real(kind=r2), intent(in) :: x

    ! Outputs
    real(kind=r2), intent(out) :: d

    select case (par)
        case (1)        ! N
            d = ldnorm(log(x-1), 0d0, 10d0)
        case (2)        ! Cab
            d = ldnorm(log(x), 0d0, 10d0)
        case (3)        ! Car
            d = ldnorm(log(x), 0d0, 10d0)
        case (4)        ! Cbrown
            d = ldnorm(log(x), 0d0, 10d0)
        case (5)        ! Cw
            d = ldnorm(log(x), 0d0, 10d0)
        case (6)        ! Cm
            d = ldnorm(log(x), 0d0, 10d0)
        case default
            print *, "!!!! ERROR: WRONG PRIOR INDEX !!!!"
            stop
    end select
end subroutine

