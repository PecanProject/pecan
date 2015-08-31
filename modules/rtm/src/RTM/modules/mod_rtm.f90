module mod_rtm
    use mod_types
    use mod_dataspec_wavelength
    use mod_combine
    contains

        !!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!! PROSPECT family !!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine prospect5b_inv(params, np, indp, &
                constants, nc, indc, Refl)
            implicit none

            ! Inputs
            integer(kind=i2), intent(in) :: np, nc, indp(np), indc(nc)
            real(kind=r2), intent(in) :: params(np), constants(nc)

            ! Outputs
            real(kind=r2), intent(out) :: Refl(nw)

            ! Internals
            real(kind=r2) :: RT(nw,2), allparams(np+nc)

            call combine_params(params, np, indp, constants, nc, indc, &
                        allparams)
            call prospect_5b(allparams(1), allparams(2), allparams(3), &
                        allparams(4), allparams(5), allparams(6), RT)
            Refl = RT(:,1)
            return
        end subroutine
        subroutine prospect5_inv(params, np, indp, &
                constants, nc, indc, Refl)
            implicit none

            ! Inputs
            integer(kind=i2), intent(in) :: np, nc, indp(np), indc(nc)
            real(kind=r2), intent(in) :: params(np), constants(nc)

            ! Outputs
            real(kind=r2), intent(out) :: Refl(nw)

            ! Internals
            real(kind=r2) :: RT(nw,2), allparams(np+nc)

            call combine_params(params, np, indp, constants, nc, indc, &
                        allparams)
            call prospect_5(allparams(1), allparams(2), allparams(3), &
                        allparams(4), allparams(5), RT)
            Refl = RT(:,1)
            return
        end subroutine
        subroutine prospect4_inv(params, np, indp, &
                constants, nc, indc, Refl)
            implicit none

            ! Inputs
            integer(kind=i2), intent(in) :: np, nc, indp(np), indc(nc)
            real(kind=r2), intent(in) :: params(np), constants(nc)

            ! Outputs
            real(kind=r2), intent(out) :: Refl(nw)

            ! Internals
            real(kind=r2) :: RT(nw,2), allparams(np+nc)

            call combine_params(params, np, indp, constants, nc, indc, &
                        allparams)
            call prospect_4(allparams(1), allparams(2), allparams(3), &
                        allparams(4), RT)
            Refl = RT(:,1)
            return
        end subroutine

        !!!!!!!!!!!!!!!!!!!!!!!
        !!!!! Sail family !!!!!
        !!!!!!!!!!!!!!!!!!!!!!!
        subroutine pro4sail_inv(params, np, indp, &
                constants, nc, indc, Refl)
            implicit none

            ! Inputs
            integer(kind=i2), intent(in) :: np, nc, indp(np), indc(nc)
            real(kind=r2), intent(in) :: params(np), constants(nc)

            ! Outputs
            real(kind=r2), intent(out) :: Refl(nw)

            ! Internals
            real(kind=r2) :: rddt(nw), rsdt(nw), rdot(nw), rsot(nw)
            real(kind=r2) :: allparams(np+nc)

            call combine_params(params, np, indp, constants, nc, indc, &
                        allparams)
            call pro4sail(allparams(1), allparams(2), allparams(3), &
                        allparams(4), allparams(5), allparams(6), &
                        allparams(7), allparams(8), allparams(9), &
                        allparams(10), allparams(11), allparams(12), &
                        allparams(13), allparams(14), allparams(15), &
                        rddt, rsdt, rdot, rsot)
            Refl = rddt
            return
        end subroutine

end module
