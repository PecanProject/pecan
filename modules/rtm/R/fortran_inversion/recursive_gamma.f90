!!! Recursive gamma from Singh
RECURSIVE FUNCTION rgamma(shp, scl) result(ans)
    DOUBLE PRECISION shp,scl,u,w,d,c,x,xsq,g
    !
    ! ## Implementation based on "A Simple Method for Generating Gamma Variables"
    ! ## by George Marsaglia and Wai Wan Tsang.
    ! ## ACM Transactions on Mathematical Software

    ! ## Vol 26, No 3, September 2000, pages 363-372.
    !
    IF (shp >= 1.0d0) THEN
        d = shp - 1.0d0/3.0d0
        c = 1.0d0/(9.0d0*d)**0.5
        DO while (.true.)
            x = rnorm(0.0d0, 1.0d0)
            v = 1.0 + c*x
            DO while (v <= 0.0d0)
                x = rand_normal(0.0d0,1.0d0)
                v = 1.0d0 + c*x
            END DO

            v = v*v*v
            CALL RANDOM_NUMBER(u)
            xsq = x*x
            IF ((u < 1.0d0 -.0331d0*xsq*xsq) .OR.  &
                (log(u) < 0.5d0*xsq + d*(1.0d0 - v + log(v)))) then
                ans = scl*d*v
            RETURN
            END IF

        END DO
    ELSE
        g = rand_gamma(shp+1.0d0, 1.0d0)
        CALL RANDOM_NUMBER(w)
        ans=shp*g*(w)**(1.0d0/shp)
        RETURN
        END IF

        END FUNCTION rgamma
