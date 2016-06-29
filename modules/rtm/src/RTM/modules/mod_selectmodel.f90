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
                case(1151)
                    print *, "Fortran model: PROSPECT 5"
                    model => prospect5_inv
                case(1141)
                    print *, "Fortran model: PROSPECT 4"
                    model => prospect4_inv
                case(2111)
                    print *, "Fortran model: PRO4SAIL"
                    model => pro4sail_inv
                case default
                    print *, "!!! ERROR: Invalid function name !!!"
                    stop
            end select
        end subroutine
end module
