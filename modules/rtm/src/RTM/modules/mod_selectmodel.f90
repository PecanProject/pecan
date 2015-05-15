module mod_selectmodel
    use mod_types
    use mod_rtm
    contains
        subroutine model_select(modcode, model)
            implicit none
            integer(kind=i2) :: modcode
            procedure(), pointer :: model => null()

            select case (modcode)
                case(1152)
                    print *, "FORTRAN model: PROSPECT 5B"
                    model => prospect5b_inv
                case default
                    print *, "!!! ERROR: Invalid function name !!!"
                    stop
            end select
        end subroutine
end module
