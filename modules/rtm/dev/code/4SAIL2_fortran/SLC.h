c     Common-Block for SLC
      

      real*8    pi,rd
      
      real*4    tts,tto,psi,lai,fb,diss,LIDFa,LIDFb,rg,rb,tg,tb,
     +          rs,hot,Cab,Cv,zeta,
     +          rdot,rsot,rsdt,rddt,rswet,
     +          rsosoil,rdosoil,rsdsoil,rddsoil,alfast,alfadt

      real*4    rddcb,rddct,tddc,rsdc,tsdc,rdoc,rsodt,rsost,
     +          tdoc,tssc,tooc,rsoc,tssooc
c
      common /misc/pi,rd

      common /inputs/lai,fB,diss,LIDFa,LIDFb,hot,rg,rb,tg,tb,
     +               Cab,Cv,zeta,
     +               rsosoil,rdosoil,rsdsoil,rddsoil,
     +               tts,tto,psi
      
      common /rfltrn/tss,too,tsstoo,rdd,tdd,rsd,tsd,rdo,tdo,rso,rsos,
     +               rsod,rddt_t,rddt_b,rddt,rsdt,rdot,rsot,rsodt,
     +               rsost,tsst,toot,tsdt,tdot,tddt,alfast,alfadt,tooc
