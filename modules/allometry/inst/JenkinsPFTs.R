#Jenkins PFT groupings (tables 1 and 3)
#actual query is performed based on the spcd number
#querying whole aboveground (2), whole aboveground (above stump) (3) and whole stem (6) biomass eqns.
######### Table 1
aspen.alder.willow <- list(AAW = data.frame(spcd = c(350,351,353,740,741,742,743,746),
                           acronym=c("ALNUS","ALRU2","ALVIS","POPUL","POBA2","PODE3","POGR4","POTR5")))

allom.stats.aspen = AllomAve(aspen.alder.willow,components=c(2,3,6),ngibbs=500)

###ACMA3 isn't being queried
###there isn't a component 2,3 or 6 for bigleaf maple (ACMA3)
maple.birch <- list(MB = data.frame(spcd = c(312,315,316,319,371,372,375,379),
                    acronym=c("ACMA3","ACPE","ACRU","ACSP2","BEAL2","BELE","BEPA","BEPO")))

allom.stats.maple.birch = AllomAve(maple.birch,components=c(2,3,6),ngibbs=500)

#aesculus octandra synonym: A. flava
#Castanopsis chrysophylla synonym: Chrysolepis chrysophylla : golden chinkapin
###CACH6 isn't being queried - no 2,3, or 6 component eqns. in table 3
###TIAMH 952 is white basswood Tilia heterophylla
mixed.hardwood <- list(MH = data.frame(spcd = c(332,431,491,541,543,544,611,621,691,
                                                693,711,731,761,762,763,931,951,952,
                                                972,970),
                                       acronym=c("AEOC2","CACH6","COFL2","FRAM2",
                                                 "FRNI","FRPE","LIST2","LITU","NYAQ2",
                                                 "NYSY","OXAR","PLOC","PRPE2","PRSE2",
                                                 "PRVI","SAAL5","TIAM","TIAMH","ULAM","ULMUS")))

allom.stats.mixed.hardwood = AllomAve(mixed.hardwood,components=c(2,3,6),ngibbs=500)

#QUPR2 is chestnut oak (quercus prinus - now quercus montana) - code also QUMO4
#will cause issues with own data - code is QUMO4
maple.oak.hickory.beech <- list(MOHB = data.frame(spcd = c(318,400,531,802,806,809,
                                                           812,820,827,832,833,835,
                                                           837),
                                                  acronym=c("ACSA3","CARYA","FAGR",
                                                            "QUAL","QUCO2","QUEL",
                                                            "QUFA","QULA3","QUNI",
                                                            "QUPR2","QURU","QUST",
                                                            "QUVE")))

allom.stats.mohb = AllomAve(maple.oak.hickory.beech,components=c(2,3,6),ngibbs=500)

####### Table 3
#Chamaecyparis/Thuja spp. haven't found yet - should be 3rd entry
#entry 41 and 43 don't exist in table - these are only other Chamaecyparis spp. not
#accounted for. Both Thuja spp are accounted for
cedar.larch <- list(CL = data.frame(spcd = c(81,42,68,71,73,70,212,241,242),
                                    acronym = c("CADE27","CANO9","JUVI","LALA","LAOC",
                                                "LARIX","SEGI2","THOC2","THPL")))

allom.stats.cedar.larch = AllomAve(cedar.larch,components=c(2,3,6),ngibbs=500)

#fir/hemlock
#ABIES - fir spp.
#equation for pacific silver fir dropped (entry 14), citation 91, eqn. form: biomass = a + b*ln(X)
#entry 1085 also dropped, but it's mountain hemlock and original paper doesn't appear to have an eqn for this spp.
#citation number for 1085 is incorrect
fir.hemlock <- list(FH = data.frame(spcd = c(202,11,12,15,17,19,20,22,10,261,263,264),
                                    acronym = c("PSME","ABAM","ABBA","ABCO","ABGR",
                                                "ABLA","ABMA","ABPR","ABIES","TSCA",
                                                "TSHE","TSME")))

allom.stats.fir.hemlock = AllomAve(fir.hemlock,components=c(2,3,6),ngibbs=500)

#Pine
#pinyon pine also called Twoneedle pinyon (106)
pine <- list(P = data.frame(spcd = c(101,105,108,136,106,116,117,133,119,122,125,126,
                                     129,131),
                            acronym = c("PIAL","PIBA2","PICO","PIDI3","PIED","PIJE",
                                        "PILA","PIMO","PIMO3","PIPO","PIRE","PIRI",
                                        "PIST","PITA")))

allom.stats.pine = AllomAve(pine,components=c(2,3,6),ngibbs=500)

#Spruce
#eqn 337 for Black Spruce (95) dropped by default code. Source: citation 82
spruce <- list(S = data.frame(spcd = c(91,93,94,95,97,98,90),
                              acronym = c("PIAB","PIEN","PIGL","PIMA","PIRU","PISI","PICEA")))

allom.stats.spruce = AllomAve(spruce,components=c(2,3,6),ngibbs=500)

#Woodland Spp.
#spp ID 300 = Acacia Spp. - not present in table 3 but prickly, earleaf & willow acacia are
woodland <- list(W = data.frame(spcd = c(475,69,65,986,814,843),
                                acronym = c("CELE3","JUMO","JUOS","PROSO","QUGA","QUHY")))

allom.stats.woodland = AllomAve(woodland,components=c(2,3,6),ngibbs=500) 



