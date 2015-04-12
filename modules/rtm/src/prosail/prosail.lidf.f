subroutine LIDF_fun(TypeLIDF, na, LIDFa, LIDFb, &
        lidf)
    INTEGER, INTENT(in) :: TypeLidf, na
    REAL*8, INTENT(in) :: LIDFa, LIDFb
    REAL*8, INTENT(out) :: lidf(na)

    IF(TypeLidf == 1) THEN
        CALL dladgen(LIDFa,LIDFb,lidf)
    ELSEIF(TypeLidf == 2) THEN
        CALL calc_LIDF_ellipsoidal(na,LIDFa,lidf)
    ENDIF

    return
end subroutine LIDF_fun

!********************************************************************************
!*                          Campbell.f                            
!*     
!*    Computation of the leaf angle distribution function value (freq) 
!*    Ellipsoidal distribution function caracterised by the average leaf 
!*    inclination angle in degree (ala)                                     
!*    Campbell 1986                                                      
!*                                                                              
!********************************************************************************
SUBROUTINE campbell(n,ala,freq)

    IMPLICIT NONE

    INTEGER*4 :: i,n
    REAL*8 :: ala,freq(n),dum
    REAL*8 :: excent,tl1(n),tl2(n),x1,x2,alpha,alpha2
    REAL*8 :: tx1(13),tx2(13)
    REAL*8 :: x12,x22,almx1,almx2,alpx1,alpx2,sum0
    DATA tx2 /0, 10, 20, 30, 40, 50, 60, 70, 80, 82, 84, 86, 88/
    DATA tx1 /10, 20, 30, 40, 50, 60, 70, 80, 82, 84, 86, 88, 90/

    DO i=1,n
    tl1(i)=tx1(i)*ATAN(1.)/45.
    tl2(i)=tx2(i)*ATAN(1.)/45.
    ENDDO
    excent=EXP(-1.6184e-5*ala**3+2.1145e-3*ala**2-1.2390e-1*ala+3.2491)
    sum0 = 0.
    DO i=1,n
    x1  = excent/(SQRT(1.+excent**2*TAN(tl1(i))**2))
    x2  = excent/(SQRT(1.+excent**2*TAN(tl2(i))**2))
    IF (excent.eq.1.) THEN
        freq(i) = ABS(COS(tl1(i))-COS(tl2(i)))
    ELSE
        alpha  = excent/SQRT(ABS(1.-excent**2))
        alpha2 = alpha**2
        x12 = x1**2
        x22 = x2**2
        IF (excent.gt.1) THEN
            alpx1 = SQRT(alpha2+x12)
            alpx2 = SQRT(alpha2+x22)
            dum   = x1*alpx1+alpha2*LOG(x1+alpx1)
            freq(i) = ABS(dum-(x2*alpx2+alpha2*LOG(x2+alpx2)))
        ELSE
            almx1 = SQRT(alpha2-x12)
            almx2 = SQRT(alpha2-x22)
            dum   = x1*almx1+alpha2*ASIN(x1/alpha)
            freq(i) = ABS(dum-(x2*almx2+alpha2*ASIN(x2/alpha)))
        ENDIF
    ENDIF
    sum0 = sum0+freq(i)
    ENDDO
    DO i=1,n
    freq(i)=freq(i)/sum0!*100.
    ENDDO

    RETURN
END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE calc_LIDF_ellipsoidal(na,alpha,freqvar)
    IMPLICIT NONE

    INTEGER*4 :: na,ia,icou,k
    REAL*8 :: opt,bemu,benu,alphadeg,alpha,freqvar(na),freq(na),pi
    !...............................................................................
    !     Call leaf angle distribution fonction                  
    !...............................................................................

    alphadeg=alpha

    CALL campbell(na,alphadeg,freq)
    DO ia=1,na
    freqvar(ia)=freq(ia)
    ENDDO

    RETURN
END SUBROUTINE


subroutine dladgen(a,b,freq)

    real*8 a,b,freq(13),t
    real*8 dcum

    do i1=1,8
    t=i1*10. 
    freq(i1)=dcum(a,b,t)
    end do

    do i2=9,12
    t=80.+(i2-8)*2.
    freq(i2)=dcum(a,b,t)
    end do

    freq(13)=1.

    do i=13,2,-1
    freq(i)=freq(i)-freq(i-1)
    end do

    return
end subroutine dladgen


function dcum(a,b,t)

    real*8 a,b,t,pi,rd,eps,delx,p,y,x,dx,dcum
    pi=atan(1.)*4.
    rd=pi/180.

    if (a.gt.1.) then

        dcum=1.-cos(rd*t)

    else

        eps=1.e-8
        delx=1.

        x=2*rd*t
        p=x

        do while (delx.gt.eps) 
        y = a*sin(x)+.5*b*sin(2.*x)
        dx=.5*(y-x+p) 
        x=x+dx
        delx=abs(dx)
        end do

        dcum=(2.*y+p)/pi

    endif

    return
end function dcum
