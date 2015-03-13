double precision function dnorm(x, mu, sd)
    double precision x, mu, sd, f, pi
    pi = 4 * atan(1.0)
    f = 1/(sd * sqrt(2*pi)) * exp(-(x - mu)**2 / (2*sd**2))
    dnorm = log(f)
    return
    end function dnorm

double precision function rnorm(mu, sd)
    double precision mu, sd, pi
    double precision z
    double precision U1, U2, R
    pi = 4 * atan(1.0)
    call random_number(U1)
    call random_number(U2)
    call random_number(R)
    
    if(R .le. 0.5) then
    z = sqrt(-2.0 * log(U1)) * cos(2.0 * pi * U2)
else if(R .gt. 0.5) then
    z = sqrt(-2.0 * log(U1)) * sin(2.0 * pi * U2)
end if
    rnorm = mu + z * sd
    return
    end function rnorm


double precision function rgamma(c, scl)
    !!! NOTE: Assumes a > 1.
    !!! If this isn't true, need to do a recursive function
    !!! Setup step:
    !!! d = a - 1.0/3.0
    !!! c = 1.0 / sqrt(9.0 * d)
    double precision d, c, scl
    double precision x, v, u, rnorm
    do while(.true.)
        x = rnorm(0.0d0, 1.0d0)
        v = 1.0 + c*x
        do while(v <= 0.0)
            x = rnorm(0.0d0, 1.0d0) 
            v = 1.0 + c*x
        end do
        v = v*v*v
        call random_number(u)
        if(u .lt. 1.0 - 0.0331*(x*x)*(x*x)) then
            rgamma = scl * d*v
            return
        end if
        if(log(u) .lt. 0.5 * x*x + d*(1.0 - v + log(v))) then
            rgamma = scl * d*v
            return
        end if
    end do
    end function rgamma

