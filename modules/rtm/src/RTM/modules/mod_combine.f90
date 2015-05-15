module mod_combine
    use mod_types
    contains
        subroutine combine_params(params, np, indp, & 
                constants, nc, indc, allparams)

            ! Inputs
            integer(kind=i2), intent(in) :: np, nc, indp(np), indc(nc)
            real(kind=r2), intent(in) :: params(np), constants(nc)

            ! Internals
            real(kind=r2) :: allparams(np+nc)

            allparams(indp) = params
            if(nc > 0) allparams(indc) = constants
            return
        end subroutine
end module
