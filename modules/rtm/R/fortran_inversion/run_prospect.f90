program main
    double precision N, Cab, Cw, Cm
    double precision, dimension(2101) :: Refl
    N = 1.4
    Cab = 50
    Cw = 0.01
    Cm = 0.01
    call PROSP(N, Cab, Cw, Cm, Refl)
    open(unit=12, file="spectest.txt", action="write")
    do 1210 i=1, 2101
        write(12, *) Refl(i)
1210 continue
    end program main

include "prospect.f90"
