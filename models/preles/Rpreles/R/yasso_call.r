#------------------------------------------------------------------------
yasso07=function(input)
{
# ORIGINAL FORTRAN CODE FROM YASSO DEVELOPERS (/Aleksi)
# Changed to R BY SANNA HÄRKÖNEN 5.2.2014

t=input[1]
cl=input[2:4]
init=input[5:9]
inf=input[10:14]
s=input[15]
z=input[16:20]
paramset=input[21]

if (paramset==1) ### Scandinavian new paraneter set7.5.2012    #Added 26.11.2013, from Aleksi
{
a <- c( -0.5172509,-3.551512,-0.3458914,-0.2660175,0.044852223,0.0029265443,0.9779027,0.6373951,0.3124745,0.018712098, 0.022490378,0.011738963,0.00099046889,0.3361765,0.041966144,0.089885026,0.089501545,-0.0022709155,0.17,-0.0015,0.17,-0.0015,    0.17,-0.0015,0,-2.935411,0,101.8253, 260,-0.080983594,-0.315179,-0.5173524,0, 0,-0.00024180325,0.0015341907, 101.8253,260,-0.5391662,1.18574,-0.2632936,0,0,0)
}

m<- matrix(nrow=5, ncol=5);
mt<- matrix(nrow=5, ncol=5);
m2<- matrix(nrow=5, ncol=5);
mi<- matrix(nrow=5, ncol=5);

te<-rep(NA,5)
z1<-rep(NA,5)
z2<-rep(NA,5)

#temperature annual cycle approximation
te[1]=cl[1]+4*cl[3]*(1/sqrt(2.0)-1)/pi
te[2]=cl[1]-4*cl[3]/sqrt(2.0)/pi
te[3]=cl[1]+4*cl[3]*(1-1/sqrt(2.0))/pi
te[4]=cl[1]+4*cl[3]/sqrt(2.0)/pi
tem=0.0


for (i in 1:4) #Annual cycle, different models
{
    tem=tem+exp(a[17]*te[i]+a[18]*te[i]**2.0)/4.0 #Gaussian
}

#Precipitation dependence
tem=tem*(1.0-exp(a[26]*cl[2]/1000))

#Size class dependence -- no effect if sc = 0.0
m[1,1]=a[1]*tem*(s*a[39]+s**2.0*a[40]+1.0)**a[41]
m[2,2]=a[2]*tem*(s*a[39]+s**2.0*a[40]+1.0)**a[41]
m[3,3]=a[3]*tem*(s*a[39]+s**2.0*a[40]+1.0)**a[41]
m[4,4]=a[4]*tem*(s*a[39]+s**2.0*a[40]+1.0)**a[41]

#Calculating matrix M, normal
m[2,1]=a[5]*abs(m[2,2])
m[3,1]=a[6]*abs(m[3,3])
m[4,1]=a[7]*abs(m[4,4])
m[5,1]=0.0
m[1,2]=a[8]*abs(m[1,1])
m[3,2]=a[9]*abs(m[3,3])
m[4,2]=a[10]*abs(m[4,4])
m[5,2]=0.0
m[1,3]=a[11]*abs(m[1,1])
m[2,3]=a[12]*abs(m[2,2])
m[4,3]=a[13]*abs(m[4,4])
m[5,3]=0.0
m[1,4]=a[14]*abs(m[1,1])
m[2,4]=a[15]*abs(m[2,2])
m[3,4]=a[16]*abs(m[3,3])
m[5,4]=0.0
m[5,5]=a[35]*tem #no size effect in humus


for (i in 1:4)
{
    m[i,5]=a[36]*abs(m[i,i]) #mass flows EWAN -> H
}


for (i in 1:5)
{
    #z1[i]=DOT_PRODUCT(m[,i],init]+inf[i]
    z1[i]=(m[,i] %*% init)+inf[i]
}

mt=m*t

#matrixexp(mt,m2)
m2=expm(mt, method="Pade")

for (i in 1:5)
{
    #z2[i]=DOT_PRODUCT(m2[,i],z1)-inf(i)
    z2[i]=(m2[,i] %*% z1)-inf[i]
}


#inverse(m,mi)
mi=solve(m)

for (i in 1:5)
{
    #z1(i]=DOT_PRODUCT(mi(,i),z2)
    z1[i]=(mi[,i] %*% z2)
}

z=z1
return(z)

}

