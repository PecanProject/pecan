#**************************************************************
#* Biomassamallit (Multivariate models)
#*=============================================================*

#  #MUUTUJAT
#   # Biomassa, kg
#   # di = rinnankorkeusläpimitta, cm
#   # dk = 2+1.25*d            
#   # h = pituus, m
#   # ika = rinnankorkeusikä
#   # cl = latvuksen pituus, m
#   # cr = latvussuhde
#   # id5 = 5-vuotiskauden sädekasvu, cm
#   # ig5 = 5-vuotiskauden pohjapinta-alan kasvu, cm2 
#   # kuorr13 = kaksinkertainen kuoren paksuus rinnankorkudelta, cm              

#        #KOIVUN MALLIT (multivariate)       
#            #Rungon puuaine
ko.ru.m1 = function(dk,h) exp(-4.879+9.651*dk/(dk+12)+1.012*log(h)+(0.00263+0.00544)/2)#Model 1
ko.ru.m2 = function(dk, h, di) exp(-4.886+9.965*dk/(dk+12)+0.966*log(h)-0.135*di+(0.00160+0.00537)/2)         #Model 2
ko.ru.m3 = function(dk, h, di) exp(-4.915+9.984*dk/(dk+12)+0.981*log(h)-0.180*di+(0.0014+0.00534)/2)          #Model 3

         #Kuori
ko.kuo.m1 = function(dk, h) exp(-5.401+10.061*dk/(dk+12)+2.657*h/(h+20)+(0.01043+0.04443)/2)#Model 1
ko.kuo.m2 = function(dk, h) exp(-5.433+10.121*dk/(dk+12)+2.647*h/(h+20)+(0.01059+0.04419)/2)#Model 2
ko.kuo.m3 = function(dk, h, kuorr13) exp(-5.304+8.498*dk/(dk+8)+3.380*h/(h+22)+0.382*log(kuorr13)+(0.01135+0.03508)/2) #Model 3

                 #Elävät oksat
ko.ran.m1 = function(dk, h) exp(-4.152+15.874*dk/(dk+16)-4.407*h/(h+10)+(0.02733+0.07662)/2)              #Model 1
ko.ran.m2= function(dk, h, cl) exp(-5.067+14.614*dk/(dk+12)-5.074*h/(h+12)+0.092*cl+(0.01508+0.05663)/2)      #Model 2
ko.ran.m3 = function(dk, h, id5, cl, ika) exp(-5.918+12.867*dk/(dk+10)-3.573*h/(h+10)+0.238*log(id5)+0.095*cl+0.007*ika+(0.01171+0.043)/2)#Model 3

                 #Lehdet
ko.lehdet.m1= function(dk) exp(-29.556+33.372*dk/(dk+2)+(0.077)/2) #Model 1
ko.lehdet.m2= function(dk, cr) exp(-20.856+22.320*dk/(dk+2)+2.819*cr+(0.01082+0.04355)/2)#Model 2
   
                 #Kuolleet oksat
ko.ran5.m1 = function(dk) 2.073*exp(-8.335+12.402*dk/(dk+16))       #Model 1
ko.ran5.m2 = function(dk) 2.149*exp(-7.996+11.824*dk/(dk+16))       #Model 2
ko.ran5.m3 = function(dk, h, ika, id5) 1.788*exp(-16.113+37.902*dk/(dk+6)-17.342*h/(h+10)-0.063*ika-0.166*id5)  #Model 3

                 #Koko maanpäällinen biomassa
ko.tot.m1 = function(dk, h) exp(-3.654+10.582*dk/(dk+12)+3.018*h/(h+22)+(0.00068+0.00727)/2) #Model 1
ko.tot.m2 = function(dk, h, ika) exp(-3.659+10.588*dk/(dk+12)+2.996*h/(h+22)+0.0006*ika+(0.00049+0.00711)/2)     #Model 2
ko.tot.m3 = function(dk, h, id5, di) exp(-3.713+10.616*dk/(dk+12)+3.235*h/(h+22)+0.007*id5-0.214*di+(0.00673)/2)        #Model 3

                 #Kanto- ja juuret
ko.kanto.m1 = function(dk) exp(-3.574+11.304*dk/(dk+26)+(0.02154+0.04542)/2)               #Model 1
ko.juuret.m1 = function(dk, h) exp(-3.223+6.497*dk/(dk+22)+1.033*log(h)+(0.048+0.02677)/2)    #Model 1
ko.tot.m1 = function(dk, h) exp(-2.726+7.652*dk/(dk+24)+0.799*log(h)+(0.02623+0.02152)/2)     #Model 1(kanto+juuret)

     # MÄNNYN MALLIT (multivariate)
                 #Rungon puuaine
ma.ru.m1 = function(dk, h) exp(-3.721+8.103*dk/(dk+14)+5.066*h/(h+12)+(0.002+0.009)/2)  #Model 1
ma.ru.m2 = function(dk, h, ika) exp(-4.018+8.358*dk/(dk+14)+4.646*h/(h+10)+0.041*log(ika)+(0.001+0.008)/2)     #Model 2
ma.ru.m3 = function(dk, h, ika, ig5) exp(-4.590+8.520*dk/(dk+9)+5.013*h/(h+16)+0.002*ika+0.002*ig5+(0.001+0.008)/2)  #Model 3
 
                 #Rungon kuori
ma.kuo.m1 = function(dk, h) exp(-4.548+7.997*dk/(dk+12)+0.357*log(h)+(0.015+0.061)/2)             #Model 1
ma.kuo.m2 = function(dk, h) exp(-4.695+8.727*dk/(dk+12)+0.228*log(h)+(0.014+0.057)/2)             #Model 2
ma.kuo.m3 = function(dk, h, di, kuorr13) exp(-5.565+9.691*dk/(dk+8)-0.444*di+0.068*kuorr13+(0.008+0.058)/2)    #Model 3

                 #Neulaset
ma.neul.m1 = function(dk, h) exp(-6.303+14.472*dk/(dk+6)-3.976*h/(h+1)+(0.109+0.118)/2)             #Model 1
ma.neul.m2 = function(dk, h, cl) exp(-1.748+14.824*dk/(dk+4)-12.684*h/(h+1)+1.209*log(cl)+(0.032+0.093)/2)                #Model 2
ma.neul.m3 = function(dk, h, ig5, cl) exp(-2.209+9.347*dk/(dk+6)-6.364* h/(h+1)+0.309*log(ig5)+0.611*log(cl)+(0.027+0.082)/2)  #Model 3

                 #Elävät oksat
ma.ran.m1 = function(dk, h) exp(-6.162+15.075*dk/(dk+12)-2.618*h/(h+12)+(0.041+0.089)/2)             #Model 1
ma.ran.m2 = function(dk, h, cl) exp(-5.166+13.085*dk/(dk+12)-5.189*h/(h+8)+1.110*log(cl)+(0.020+0.063)/2)#Model 2
ma.ran.m3 = function(dk, h, ig5, cl) exp(-4.833+13.126* dk/(dk+10)-4.808* h/(h+4)+0.098*log(ig5)+0.727*log(cl)+(0.018+0.059)/2) #Model 3
         
         #Kuolleet oksat
ma.ran5.m1 = function(dk) 0.911*exp(-5.201+10.574*dk/(dk+16))      #Model 1
ma.ran5.m2 = function(dk) 0.913*exp(-5.318+10.771*dk/(dk+16))      #Model 2
ma.ran5.m3 = function(dk, cl, ig5, ika) 0.918*exp(-5.798+17.82* dk/(dk+16)-0.738*log(cl)-0.461*log(ig5)-0.017*ika)   #Model 3

         #Koko maanpäällinen biomassa
ma.tot.m1 = function(dk, h) exp(-3.198+9.547*dk/(dk+12)+3.241*h/(h+20)+(0.009+0.010)/2)            #Model 1
ma.tot.m2 = function(dk, h, cr) exp(-3.416+9.555*dk/(dk+12)+3.592*h/(h+24)+0.395*cr+(0.008+0.009)/2)   #Model 2
ma.tot.m3 = function(dk, h, id5, ika, kuorr13) exp(-3.529+9.337*dk/(dk+12)+3.265*h/(h+18)+0.124*id5+0.001*ika-0.006*kuorr13+(0.003+0.009)/2)   #Model 3

             #Kanto ja juuret
ma.kanto.m1 = function(dk) exp(-6.753+12.681*dk/(dk+12)+(0.010+0.044)/2)              #Model 1
ma.juuret.m1 = function(dk) exp(-5.550+13.408*dk/(dk+15)+0.079/2)   #Model 1


          # KUUSEN MALLIT (multivariate)
                 #Rungon puuaine
ku.ru.m1 = function(dk, h) exp(-3.555+8.042*dk/(dk+14)+0.869*log(h)+0.015*h+(0.009+0.009)/2)   #Model 1
ku.ru.m2 = function(dk, h, di) exp(-4.000+8.881*dk/(dk+12)+0.728*log(h)+0.022*h-0.273*di+(0.003+0.008)/2)             #Model 2
ku.ru.m3 = function(dk, h, ika, id5) exp(-3.950+8.534*dk/(dk+12)+0.743*log(h)+0.022*h+0.001*ika-0.071*id5+(0.003+0.008)/2) #Model 3

                   #Rungon kuoori
ku.kuo.m1 = function(dk, h) exp(-4.548+9.448*dk/(dk+18)+0.436*log(h)+(0.023+0.041)/2) #Model 1
ku.kuo.m2 = function(dk, h) exp(-4.437+10.071*dk/(dk+18)+0.261*log(h)+(0.019+0.039)/2)#Model 2
ku.kuo.m3 = function(dk, h, kuorr13) exp(-4.626+9.638*dk/(dk+16)+0.266*log(h)+0.084*kuorr13+(0.013+0.042)/2)     #Model 3

                 #Neulaset
ku.neul.m1 = function(dk, h) exp(-2.994+12.251*dk/(dk+10)-3.415*h/(h+1)+(0.107+0.089/2))                #Model 1
ku.neul.m2 = function(dk, h, cl) exp(-0.085+15.222*dk/(dk+4)-14.446*h/(h+1)+1.273*log(cl)+(0.028+0.087)/2)    #Model 2
ku.neul.m3 = function(dk, cr, id5) exp(-4.258+9.200*dk/(dk+12)+0.967*cr+0.287*log(id5)+(0.022+0.068)/2)      #Model 3
 
                 #Elävät oksat
ku.ran.m1 = function(dk, h) exp(-4.214+14.508*dk/(dk+13)-3.277*h/(h+5)+(0.039+0.081)/2)                #Model 1
ku.ran.m2 = function(dk, h, cl) exp(-3.023+12.017*dk/(dk+14)-5.722*h/(h+5)+1.033*log(cl)+(0.017+0.068)/2)  #Model 2
ku.ran.m3 = function(dk, h, cr, di, id5) exp(-3.950+12.014*dk/(dk+18)-1.296*h/(h+2)+1.528*cr-0.461*di+0.112*id5+(0.011+0.067)/2)         #Model 3        
 
         #Kuolleet oksat
ku.ran5.m1 = function(dk, h) 1.343*exp(-4.850+7.702*dk/(dk+18)+0.513*log(h))          #Model 1
ku.ran5.m2 = function(dk, h) 1.208*exp(-5.317+6.384*dk/(dk+18)+0.982*log(h))          #Model 2 
ku.ran5.m3 = function(dk, cr, id5) 1.091*exp(-0.140+11.293*dk/(dk+14)+3.058*log(cr)-7.014*cr-0.189*log(id5))  #Model 3

         #Koko maanpäällinen biomassa
ku.tot.m1 = function(dk, h) exp(-1.808+9.482*dk/(dk+20)+0.469*log(h)+(0.006+0.013)/2)           #Model 1
ku.tot.m2 = function(dk, h, cr) exp(-2.141+9.074*dk/(dk+20)+0.570*log(h)+0.403*cr+(0.006+0.013)/2)  #Model 2
ku.tot.m3 = function(dk, h, cr) exp(-2.037+9.146*dk/(dk+20)+0.543*log(h)+0.296*cr+(0.007+0.013)/2)  #Model 3
 
                 #Kanto- ja juuret
ku.kanto.m1 = function(dk) exp(-3.964+11.730*dk/(dk+26)+(0.065+0.058)/2)        #Model 1
ku.juuret.m1 = function(dk) exp(-2.294+10.646*dk/(dk+24)+(0.105+0.114)/2)       #Model 1

####################
