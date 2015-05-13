module mod_selectmodel
    use mod_types
    use mod_rtm
    contains
        subroutine model_select(modname, model)
            implicit none
            character(20) :: modname
            procedure(), pointer :: model => null()

            select case(modname)
            case ("prospect_5b")
                model => prospect5b_inv
            case default
                print *, "!!! ERROR: Invalid function name !!!"
                stop
            end select
        end subroutine
end module
