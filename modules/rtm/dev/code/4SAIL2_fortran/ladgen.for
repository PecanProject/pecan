      subroutine ladgen(a,b,freq)
c
      real a,b,freq(13)
c
      do i1=1,8
	 t=i1*10.
	 freq(i1)=cum(a,b,t)
      end do
c
      do i2=9,12
	 t=80.+(i2-8)*2.
	 freq(i2)=cum(a,b,t)
      end do
c
      freq(13)=1.
c
      do i=13,2,-1
	 freq(i)=freq(i)-freq(i-1)
      end do
c
      return
      end


      function cum(a,b,t)
c
      real*8 pi,rd

	pi= 3.14159265358979
	rd=pi/180.
c
      eps=1.e-6
      delx=1.
c
      x=2*rd*t
      p=x
c
      do while (delx.gt.eps) 
	 y = a*sin(x)+.5*b*sin(2.*x)
	 dx=.5*(y-x+p) 
	 x=x+dx
	 delx=abs(dx)
      end do
c
      cum=(2.*y+p)/pi
c
      return
      end
