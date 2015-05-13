module mod_selectmodel
    use mod_types
    use mod_rtm
    contains
        subroutine model_select(modname, model)
            implicit none
            character(255) :: modname
            procedure(), pointer :: model => null()

            print *, "Given model: ", modname
            select case (modname)
                case("prospect_5b")
                    print *, "FORTRAN model: PROSPECT 5B"
                    model => prospect5b_inv
                case default
                    print *, "!!! ERROR: Invalid function name !!!"
                    stop
            end select
        end subroutine
end module
