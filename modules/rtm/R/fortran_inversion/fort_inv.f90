! Bayesian inversion of PROSPECT
program ProspectBayes
    implicit none

    double precision, dimension(2101) :: CabAbs, CwAbs, CmAbs
    double precision, dimension(2101) :: tao1, tao2, rho1, rho2
    double precision, dimension(2101) :: x, y
    double precision, dimension(2101) :: k, theta, Refl

    common CabAbs, CwAbs, CmAbs, tao1, tao2, rho1, rho2, x, y

    real start, finish
    double precision N, Cab, Cw, Cm, rsd
    double precision logN, logCab, logCw, logCm
    double precision TN, TCab, TCw, TCm
    double precision TlogN, TlogCab, TlogCw, TlogCm
    double precision dnorm, rnorm, rgamma
    double precision priorN, priorCab, priorCw, priorCm
    double precision likelihood
    integer wl, nspec, ngibbs, i, j, row, col
    double precision, dimension(4) :: JumpSD
    double precision, dimension(2101) :: PrevSpec, TrySpec
    double precision, dimension(2101, 78) :: Observed
    double precision, dimension(2101, 78) :: PrevError, TryError
    double precision PrevPost, TryPost
    double precision JN, JD, a, unif
    double precision alpha, d, c, rp2, rinv

    call random_seed

    ! Open file for diagnostics
    open(unit = 9, file = "diag.txt", action="write")

    ! Open file for writing results
    open(unit = 7, file = "testout.dat", action = "write")

    ! Open file for input spectra
    open(unit = 8, file = "testdata.dat", action="read")

    ! Open file for PROSPECT data
    open(unit = 10, file = "data_prospect.dat", action="read")

    ! Read in PROSPECT data
    do 900, i = 1, 2101
        read(10, *) CabAbs(i), CwAbs(i), CmAbs(i), tao1(i), tao2(i), rho1(i), rho2(i), x(i), y(i)
900 continue
    close(10)

    wl = 2101
    nspec = 78
    ngibbs = 5

    ! Read in spectra
    read(8, *) ((Observed(row, col), col=1, 78), row=1, 2101)
!    do 1010 i = 1, wl
!        do 1020 j = 1, nspec
!            read(8, *) Observed(i, j)
!1020    continue
!1010 continue

    ! Initial conditions
    N = 1.4
    Cab = 30
    Cw = 0.017
    Cm = 0.006
    rsd = 1.0
    logN = log(N - 1)
    logCab = log(Cab)
    logCw = log(Cw)
    logCm = log(Cm)

    JumpSD = (/ logN, logCab, logCw, logCm /)
    JumpSD = JumpSD * 0.1

    ! Precalculate values for RGamma
    alpha = 0.001 + nspec*wl/2.0
    d = alpha - 1.0/3.0
    c = 1.0 / sqrt(9.0*d)

    ! Precalculate first model
    call PROSP(N, Cab, Cw, Cm, PrevSpec)
    write(9, *) PrevSpec(1:5)
    call SpecError(PrevSpec, Observed, PrevError)
    write(9, *) PrevError(1:5, 1:5)


    ! MCMC Loop
    do 1000 i = 1, ngibbs
    write(*,*) i
       ! Sample N

       TlogN = rnorm(logN, JumpSD(1))
       write(*,*) finish - start
       
       TN = exp(TlogN) + 1
       call PROSP(TN, Cab, Cw, Cm, TrySpec)
       write(*,*) finish - start
       call SpecError(TrySpec, Observed, TryError)
       TryPost = likelihood(TryError, rsd) +  priorN(TlogN)
       PrevPost = likelihood(PrevError, rsd) + priorN(logN)

       JN = dnorm(TlogN, logN, JumpSD(1))
       write(*,*) finish - start

       JD = dnorm(logN, TlogN, JumpSD(1))
       a = exp((TryPost - JN) - (PrevPost - JD))
       call random_number(unif)
       if(a .ge. unif) then
           logN = TlogN
           N = TN
           PrevError = TryError
       end if

       ! Sample Cab
       TlogCab = rnorm(logCab, JumpSD(2))
       TCab = exp(TlogCab) + 1
       call PROSP(N, TCab, Cw, Cm, TrySpec)
       call SpecError(TrySpec, Observed, TryError)
       TryPost = likelihood(TryError, rsd) +  priorCab(TlogCab)
       PrevPost = likelihood(PrevError, rsd) + priorCab(logCab)
       JN = dnorm(TlogCab, logCab, JumpSD(1))
       JD = dnorm(logCab, TlogCab, JumpSD(1))
       a = exp((TryPost - JN) - (PrevPost - JD))
       call random_number(unif)
       if(a .ge. unif) then
           logCab = TlogCab
           Cab = TCab
           PrevError = TryError
       end if

       ! Sample Cw
       TlogCw = rnorm(logCw, JumpSD(3))
       TCw = exp(TlogCw) + 1
       call PROSP(N, Cab, TCw, Cm, TrySpec)
       call SpecError(TrySpec, Observed, TryError)
       TryPost = likelihood(TryError, rsd) +  priorCw(TlogCw)
       PrevPost = likelihood(PrevError, rsd) + priorCw(logCw)
       JN = dnorm(TlogCw, logCw, JumpSD(1))
       JD = dnorm(logCw, TlogCw, JumpSD(1))
       a = exp((TryPost - JN) - (PrevPost - JD))
       call random_number(unif)
       if(a .ge. unif) then
           logCw = TlogCw
           Cw = TCw
           PrevError = TryError
       end if

       ! Sample Cm
       TlogCm = rnorm(logCm, JumpSD(4))
       TCm = exp(TlogCm) + 1
       call PROSP(N, Cab, Cw, TCm, TrySpec)
       call SpecError(TrySpec, Observed, TryError)
       TryPost = likelihood(TryError, rsd) +  priorCm(TlogCm)
       PrevPost = likelihood(PrevError, rsd) + priorCm(logCm)
       JN = dnorm(TlogCm, logCm, JumpSD(1))
       JD = dnorm(logCm, TlogCm, JumpSD(1))
       a = exp((TryPost - JN) - (PrevPost - JD))
       call random_number(unif)
       if(a .ge. unif) then
           logCm = TlogCm
           Cm = TCm
           PrevError = TryError
       end if

       ! Sample rsd
       rp2 = 0.001 + sum(PrevError**2) / 2
       rinv = rgamma(c, rp2)
       write(*,*) finish - start
       rsd = 1/sqrt(rinv)

       ! Write to file
       write(7,*) N, Cab, Cw, Cm, rsd
    
1000 continue
end program ProspectBayes

include 'prospect.f90'
include 'distributions.f90'

!!!!!!!!!! Function definitions !!!!!!!!!!!!!!!!
double precision function priorN(logN)
    double precision logN, dnorm
    priorN = dnorm(logN, -0.916d0, 2.2d0)
    return
    end function priorN

double precision function priorCab(logCab)
    double precision logCab, dnorm
    priorCab = dnorm(logCab, 3.4d0, 0.9d0)
    return
    end function priorCab

double precision function priorCw(logCw)
    double precision logCw, dnorm
    priorCw = dnorm(logCw, -6.377d0, 0.5d0)
    return
    end function priorCw
    
double precision function priorCm(logCm)
    double precision logCm, dnorm
    priorCm = dnorm(logCm, -5.116d0, 0.9d0)
    return
    end function priorCm

subroutine SpecError(Model, Observed, Error)
    integer nspec, wl, i, j
    double precision, dimension(2101) :: Model
    double precision, dimension(2101,78) :: Error, Observed
    wl = 2101
    nspec = 78
    do 200 i = 1, wl
        do 210 j = 1, nspec
            Error(i,j) = Model(i) - Observed(i,j)
210     continue
200 continue
    return
    end

double precision function likelihood(error, sd)
    integer nspec, wl
    double precision sd, dnorm
    double precision, dimension(2101, 78) :: error
    wl = 2101
    nspec = 78
    likelihood = 0.0
    do 300 i = 1, wl
        do 310 j = 1, nspec
            likelihood = likelihood + dnorm(error(i,j), sd)
310     continue
300 continue
    return
    end function likelihood

