
##### First run Clean Million Base procedure on the World Champ PGN file, 
# This should make a file of the same structure as the variable 'R' in the Clean Million Base script, 
# which is now called Z2

# Then make sure there is no misspelled names, and then our list of world champs includes:
Champs<-c("KarpovA",
"KasparovG",
"KramnikV",
"KhalifmanA",
"AnandV",
"PonomariovR",
"KasimdzhanovR",
"TopalovV",
"CarlsenM",
"KorchnoiV",
"ShortN" ,
"LekoP" ,
"TimmanJ" ,
"KamskyG",
"AkopianV",
"ShirovA" ,
"IvanchukV",
"AdamsM",
"GelfandB")

########################################################################## Drop Games where White was not World Champ
Drop<- ifelse( Z2$NameWhite==Champs[1]  |   Z2$NameWhite==Champs[2]  |
Z2$NameWhite==Champs[3]  |
Z2$NameWhite==Champs[4]  |
Z2$NameWhite==Champs[5]  |
Z2$NameWhite==Champs[6]  |
Z2$NameWhite==Champs[7]  |
Z2$NameWhite==Champs[8]  |
Z2$NameWhite==Champs[9]  |
Z2$NameWhite==Champs[10]  |
Z2$NameWhite==Champs[11]  |
Z2$NameWhite==Champs[12]  |
Z2$NameWhite==Champs[13]  |
Z2$NameWhite==Champs[14]  |
Z2$NameWhite==Champs[15]  |
Z2$NameWhite==Champs[16]  |
Z2$NameWhite==Champs[17]  |
Z2$NameWhite==Champs[18]  |
Z2$NameWhite==Champs[19]  
,1,0)
Z3<-Z2[which(Drop==1),]

############################### Now, make sure we only get game from the reigning world champ in the correct year
 Z3$ID<-paste0(Z3$Date,Z3$NameWhite)
 
ChampYear<-c(
"1975KarpovA",
"1976KarpovA",
"1977KarpovA",
"1978KarpovA",
"1979KarpovA",
"1980KarpovA",
"1981KarpovA",
"1982KarpovA",
"1983KarpovA",
"1984KarpovA",
"1985KasparovG",
"1986KasparovG",
"1987KasparovG",
"1988KasparovG",
"1989KasparovG",
"1990KasparovG",
"1991KasparovG",
"1992KasparovG",
"1993KarpovA",
"1994KarpovA",
"1995KarpovA",
"1996KarpovA",
"1997KarpovA",
"1998KarpovA",
"1999KhalifmanA",
"2000AnandV",
"2001AnandV",
"2002PonomariovR",
"2003PonomariovR",
"2004KasimdzhanovR",
"2005TopalovV",
"2006KramnikV",
"2007AnandV",
"2008AnandV",
"2009AnandV",
"2010AnandV",
"2011AnandV",
"2012AnandV")

Drop<- ifelse( Z3$ID==ChampYear[1]  |   
Z3$ID==ChampYear[2]  |
Z3$ID==ChampYear[3]  |
Z3$ID==ChampYear[4]  |
Z3$ID==ChampYear[5]  |
Z3$ID==ChampYear[6]  |
Z3$ID==ChampYear[7]  |
Z3$ID==ChampYear[8]  |
Z3$ID==ChampYear[9]  |
Z3$ID==ChampYear[10]  |
Z3$ID==ChampYear[11]  |
Z3$ID==ChampYear[12]  |
Z3$ID==ChampYear[13]  |
Z3$ID==ChampYear[14]  |
Z3$ID==ChampYear[15]  |
Z3$ID==ChampYear[16]  |
Z3$ID==ChampYear[17]  |
Z3$ID==ChampYear[18]  |
Z3$ID==ChampYear[19]  |
Z3$ID==ChampYear[20]  |
Z3$ID==ChampYear[21]  |
Z3$ID==ChampYear[22]  |
Z3$ID==ChampYear[23]  |
Z3$ID==ChampYear[24]  |
Z3$ID==ChampYear[25]  |
Z3$ID==ChampYear[26]  |
Z3$ID==ChampYear[27]  |
Z3$ID==ChampYear[28]  |
Z3$ID==ChampYear[29]  |
Z3$ID==ChampYear[30]  |
Z3$ID==ChampYear[31]  |
Z3$ID==ChampYear[32]  |
Z3$ID==ChampYear[33]  |
Z3$ID==ChampYear[34]  |
Z3$ID==ChampYear[35]  |
Z3$ID==ChampYear[36]  |
Z3$ID==ChampYear[37]  |
Z3$ID==ChampYear[38]  
,1,0)

Z4<-Z3[which(Drop==1),]

############################ Error Check and Clean
table(Z4$ID)

Z4<-Z4[which( Z4$X1 != "Ng3"),]
Z4<-Z4[which( Z4$X1 != ""),]
             
Z4$X1<-factor(Z4$X1)
 
levels(Z4$X1) <- c(levels(Z4$X1), "a3" ,  "b4" ,  "c3","e3",  "g4" ,  "f4",  "h3",    "h4",  "Nc3","d3",  "a4" , "Nh3",  "Na3" ,   "f3" )

FF<-table(Z4$ID,Z4$X1)
Z4$ID<-factor(Z4$ID)

##################### Calc Quantities for the Bayesian Model
Z5a<-Z4[which(Z4$Win=="1"),]
Z5b<-Z4[which(Z4$Win=="2"),]
Z5c<-Z4[which(Z4$Win=="3"),]
     
WW<- table(Z5a$ID,Z5a$X1)
WD<- table(Z5b$ID,Z5b$X1)
WL<- table(Z5c$ID,Z5c$X1)

Pwr<-(WW+(WD*0.5))/(WW+WL+WD)
      
Pfreq<-(WW+WL+WD)
Pfr <- Pfreq
      
for(i in 1: dim(Pfreq)[1]){
  Pfr[i,] <- Pfreq[i,]/sum( Pfreq[i,])
      }
      
 FF2<-FF[,order(colnames(FF))]  
 
   Pwr<-t(Pwr)
   Pfr<-t(Pfr)
             
   Pwr<-Pwr[order(rownames(Pwr)),]
   Pfr<-Pfr[order(rownames(Pfr)),]
             
   Pfr[is.na(Pfr)]<-0

