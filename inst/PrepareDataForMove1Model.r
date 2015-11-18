 ###############################################################################    Start With All Data
     Rwin<-R[which(R$Win=="1"),]
     Rdraw<-R[which(R$Win=="2"),]
     Rloss<-R[which(R$Win=="3"),]
    
### Split by Win, Loss, Or Draw
     Swin <- table(Rwin$X1,Rwin$Date)
     Sdraw <- table(Rdraw$X1,Rdraw$Date)
     Sloss <- table(Rloss$X1,Rloss$Date)

### Calc Social Win Rate by Year (Note: Better practice is to estimate win rate with multinomial, but model is too big already)
     Swr<-(Swin+(Sdraw*0.5))/(Sloss+Sdraw+Swin)  

### Calc Social Play Rate by Year (Note: Better practice is to estimate freq with multinomial, but model is too big already)
     Sfreq<-(Sloss+Sdraw+Swin) 
     Sfr <- Sfreq 

    for(i in 1: dim(Sfreq)[2]){
     Sfr[,i] <- Sfreq[,i]/sum( Sfreq[,i])
      }
      

Sfr2<-Sfr[order(rownames(Sfr)),]    
Swr2<-Swr[order(rownames(Swr)),]                         

# Thin to years of interest
Swr<-Swr2[,5:42]
Sfr<-Sfr2[,5:42]

##################################   Subset usable Individual-level data for regression, players with >200 games
      P<-table(R$NameWhite)[which(table(R$NameWhite)>=200)]
      rP<-rownames(P)
      Drop<-rep(NA,length(R$NameWhite))
      for(i in 1:length(R$NameWhite)){
      Drop[i]<- ifelse( sum((R$NameWhite[i]==rP))>0,1,0) }

      R100<-R[which(Drop==1),]
      
      R100$NameWhite<-factor(R100$NameWhite)
      
####################################################################################
      Name<-unique(R100$NameWhite)
      
      Move <- dim( table(R100$NameWhite,R100$X1))[2]
      Person <-length(Name) 
      Year<- dim(table(R100$NameWhite,R100$Date))[2]
                            
      I1<-array(NA,dim=c(Person,Move,38))   # Count of use of each opening by year
      Ifr<-array(NA,dim=c(Person,Move,38))  # Freq of use of each opening by year
      Iwr<-array(NA,dim=c(Person,Move,38))  # Win ratio of each opening by year
Elo<-c()
##################################################### Loop over individuals to get quantities of interest
  for( i in 1:length(Name)){
    F1 <-  R100[which(R100$NameWhite==Name[i]),]
    
     Fwin <-F1[which(F1$Win=="1"),]
     Fdraw<-F1[which(F1$Win=="2"),]
     Floss<-F1[which(F1$Win=="3"),]
     
     Felo<-mean(R100[which(R100$NameWhite==Name[i]),3])
     Elo[i]<-Felo
    
     F2win <- table(Fwin$X1,Fwin$Date)
     F2draw <- table(Fdraw$X1,Fdraw$Date)
     F2loss <- table(Floss$X1,Floss$Date)
    
     Fwr1<-(F2win+(F2draw*0.5))/(F2win+F2draw+F2loss)  
     Fwr2<- Fwr1[order(rownames(Fwr1)),]
     Fwr3 <-  Fwr2[,5:42]
    
     Iwr[i,,]<-Fwr3 
     
     Ffreq1<-(F2win+F2draw+F2loss)
     Ffr1 <- Ffreq1 
          
     for(j in 1: 43){
     Ffr1[,j] <- Ffreq1[,j]/sum( Ffreq1[,j])
          }
          
     Ffr2<- Ffr1[order(rownames(Ffr1)),]
     Ffr3 <-  Ffr2[,5:42] 
     
     Ffreq2<- Ffreq1[order(rownames(Ffreq1)),]
     Ffreq3 <-  Ffreq2[,5:42] 
          
     Ifr[i,,]<-Ffr3
     I1[i,,]<- Ffreq3  
      }

############################################### Find missing years by individual, to be used for Bayesian Model
   MissingYears <- array(NA,dim=c(Person,38)) 
   
  for( i in 1:1009){
      for(j in 1: 38){   
         MissingYears[i,j] <- ifelse(sum( I1[i,,j])==0,1,0)
         }} 


#Add in Prestige Data
Pfr<-
structure(c(0, 0, 0, 0, 0, 0.233333333333333, 0, 0.133333333333333, 
0, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0.0333333333333333, 0, 0, 0, 
0, 0, 0, 0.25, 0, 0.2, 0, 0.525, 0, 0, 0, 0, 0, 0, 0, 0, 0.025, 
0, 0, 0, 0, 0, 0, 0.256410256410256, 0, 0.0512820512820513, 0, 
0.666666666666667, 0, 0, 0, 0, 0, 0, 0, 0, 0.0256410256410256, 
0, 0, 0, 0, 0, 0, 0.258064516129032, 0, 0.032258064516129, 0, 
0.709677419354839, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0.32258064516129, 0, 0.0967741935483871, 0, 0.580645161290323, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.08, 0, 0.2, 0, 
0.68, 0, 0, 0, 0, 0, 0, 0, 0, 0.04, 0, 0, 0, 0, 0, 0, 0.194444444444444, 
0, 0.277777777777778, 0, 0.444444444444444, 0, 0, 0, 0, 0, 0, 
0, 0, 0.0833333333333333, 0, 0, 0, 0, 0, 0, 0, 0, 0.104166666666667, 
0, 0.895833333333333, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0.0416666666666667, 0, 0.125, 0, 0.75, 0, 0, 0, 0, 0, 0, 
0, 0, 0.0833333333333333, 0, 0, 0, 0, 0, 0, 0.0185185185185185, 
0, 0.185185185185185, 0, 0.37037037037037, 0, 0, 0, 0, 0, 0, 
0, 0, 0.425925925925926, 0, 0, 0, 0, 0, 0, 0.111111111111111, 
0, 0.603174603174603, 0, 0.26984126984127, 0, 0, 0, 0, 0, 0, 
0, 0, 0.0158730158730159, 0, 0, 0, 0, 0, 0, 0.0196078431372549, 
0, 0.686274509803922, 0, 0.274509803921569, 0, 0, 0, 0, 0, 0, 
0, 0, 0.0196078431372549, 0, 0, 0, 0, 0, 0, 0.204545454545455, 
0, 0.545454545454545, 0, 0.0454545454545455, 0, 0, 0, 0, 0, 0, 
0, 0, 0.204545454545455, 0, 0, 0, 0, 0, 0, 0.3125, 0, 0.541666666666667, 
0, 0.114583333333333, 0, 0, 0, 0, 0, 0, 0, 0, 0.03125, 0, 0, 
0, 0, 0, 0, 0.222222222222222, 0, 0.583333333333333, 0, 0.0833333333333333, 
0, 0, 0, 0, 0, 0, 0, 0, 0.111111111111111, 0, 0, 0, 0, 0, 0, 
0.172413793103448, 0, 0.310344827586207, 0, 0.448275862068966, 
0, 0, 0, 0, 0, 0, 0, 0, 0.0689655172413793, 0, 0, 0, 0, 0, 0, 
0.217391304347826, 0, 0.405797101449275, 0, 0.289855072463768, 
0, 0, 0, 0, 0, 0, 0, 0, 0.0869565217391304, 0, 0, 0, 0.0135135135135135, 
0, 0, 0.0810810810810811, 0, 0.310810810810811, 0, 0.459459459459459, 
0, 0, 0.0135135135135135, 0, 0, 0, 0, 0, 0.121621621621622, 0, 
0, 0, 0, 0, 0, 0.04, 0, 0.853333333333333, 0, 0.0266666666666667, 
0, 0, 0, 0, 0, 0, 0, 0, 0.08, 0, 0, 0, 0, 0, 0, 0.08, 0, 0.64, 
0, 0.266666666666667, 0, 0, 0, 0, 0, 0, 0, 0, 0.0133333333333333, 
0, 0, 0, 0, 0, 0, 0.163934426229508, 0, 0.80327868852459, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0327868852459016, 0, 0, 0, 0, 0, 
0, 0, 0, 0.854545454545454, 0, 0.0181818181818182, 0, 0, 0, 0, 
0, 0, 0, 0, 0.127272727272727, 0, 0, 0, 0, 0, 0, 0.175675675675676, 
0, 0.581081081081081, 0, 0.135135135135135, 0, 0, 0, 0, 0, 0, 
0, 0, 0.108108108108108, 0, 0, 0, 0, 0, 0, 0.0697674418604651, 
0, 0.697674418604651, 0, 0.0232558139534884, 0, 0, 0, 0, 0, 0, 
0, 0, 0.209302325581395, 0, 0, 0, 0, 0, 0, 0, 0, 0.515151515151515, 
0, 0.303030303030303, 0, 0, 0, 0, 0, 0, 0, 0, 0.181818181818182, 
0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.742857142857143, 0, 0, 0, 0, 
0, 0, 0, 0, 0.0571428571428571, 0, 0, 0, 0, 0, 0, 0, 0, 0.129032258064516, 
0, 0.82258064516129, 0, 0, 0, 0, 0, 0, 0, 0, 0.0483870967741935, 
0, 0, 0, 0, 0, 0, 0, 0, 0.0540540540540541, 0, 0.864864864864865, 
0, 0, 0, 0, 0, 0, 0, 0, 0.0810810810810811, 0, 0, 0, 0, 0, 0, 
0.0333333333333333, 0, 0.266666666666667, 0, 0.566666666666667, 
0, 0, 0, 0, 0, 0, 0, 0, 0.133333333333333, 0, 0, 0, 0, 0, 0, 
0.0434782608695652, 0, 0.239130434782609, 0, 0.673913043478261, 
0, 0, 0.0217391304347826, 0, 0, 0, 0, 0, 0.0217391304347826, 
0, 0, 0, 0, 0, 0, 0.024390243902439, 0, 0.439024390243902, 0, 
0.390243902439024, 0, 0, 0, 0, 0, 0, 0, 0, 0.146341463414634, 
0, 0, 0, 0, 0, 0, 0, 0, 0.722222222222222, 0, 0.111111111111111, 
0, 0, 0, 0, 0, 0, 0, 0, 0.166666666666667, 0, 0, 0, 0, 0, 0, 
0.0610687022900763, 0, 0.213740458015267, 0, 0.610687022900763, 
0, 0, 0, 0, 0, 0, 0, 0, 0.114503816793893, 0, 0, 0, 0, 0, 0, 
0.0222222222222222, 0, 0.133333333333333, 0, 0.822222222222222, 
0, 0, 0, 0, 0, 0, 0, 0, 0.0222222222222222, 0, 0, 0, 0, 0, 0, 
0.0454545454545455, 0, 0.378787878787879, 0, 0.545454545454545, 
0, 0, 0, 0, 0, 0, 0, 0, 0.0303030303030303, 0, 0, 0, 0, 0, 0, 
0.0344827586206897, 0, 0.413793103448276, 0, 0.551724137931034, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.037037037037037, 
0, 0.407407407407407, 0, 0.555555555555556, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.375, 0, 0.625, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0), .Dim = c(20L, 38L), .Dimnames = structure(list(
    c("a3", "a4", "b3", "b4", "c3", "c4", "d3", "d4", "e3", "e4", 
    "f3", "f4", "g3", "g4", "h3", "h4", "Na3", "Nc3", "Nf3", 
    "Nh3"), c("1975KarpovA", "1976KarpovA", "1977KarpovA", "1978KarpovA", 
    "1979KarpovA", "1980KarpovA", "1981KarpovA", "1982KarpovA", 
    "1983KarpovA", "1984KarpovA", "1985KasparovG", "1986KasparovG", 
    "1987KasparovG", "1988KasparovG", "1989KasparovG", "1990KasparovG", 
    "1991KasparovG", "1992KasparovG", "1993KarpovA", "1994KarpovA", 
    "1995KarpovA", "1996KarpovA", "1997KarpovA", "1998KarpovA", 
    "1999KhalifmanA", "2000AnandV", "2001AnandV", "2002PonomariovR", 
    "2003PonomariovR", "2004KasimdzhanovR", "2005TopalovV", "2006KramnikV", 
    "2007AnandV", "2008AnandV", "2009AnandV", "2010AnandV", "2011AnandV", 
    "2012AnandV")), .Names = c("", "")))
    
    Pwr<-structure(c(NaN, NaN, NaN, NaN, NaN, 0.714285714285714, NaN, 
1, NaN, 0.666666666666667, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, 0.5, NaN, NaN, NaN, NaN, NaN, NaN, 0.85, NaN, 0.625, NaN, 
0.714285714285714, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.5, 
NaN, NaN, NaN, NaN, NaN, NaN, 0.95, NaN, 0.5, NaN, 0.807692307692308, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.625, NaN, 0.5, NaN, 0.659090909090909, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
0.7, NaN, 0.833333333333333, NaN, 0.805555555555556, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
0.875, NaN, 0.7, NaN, 0.661764705882353, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, 0.5, NaN, NaN, NaN, NaN, NaN, NaN, 0.714285714285714, 
NaN, 0.75, NaN, 0.6875, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
0.833333333333333, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.7, 
NaN, 0.709302325581395, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, 0.583333333333333, 
NaN, 0.694444444444444, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
0.75, NaN, NaN, NaN, NaN, NaN, NaN, 0.5, NaN, 0.8, NaN, 0.675, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.565217391304348, NaN, 
NaN, NaN, NaN, NaN, NaN, 1, NaN, 0.802631578947368, NaN, 0.823529411764706, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.5, NaN, 0.728571428571429, NaN, 0.75, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, 0.5, NaN, NaN, NaN, NaN, NaN, NaN, 0.555555555555556, 
NaN, 0.875, NaN, 0.5, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
0.777777777777778, NaN, NaN, NaN, NaN, NaN, NaN, 0.783333333333333, 
NaN, 0.836538461538462, NaN, 0.863636363636364, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, 0.666666666666667, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.8125, NaN, 0.761904761904762, NaN, 1, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, 0.75, NaN, NaN, NaN, NaN, NaN, NaN, 
0.8, NaN, 0.888888888888889, NaN, 0.730769230769231, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, 0.5, NaN, NaN, NaN, NaN, NaN, NaN, 
0.833333333333333, NaN, 0.892857142857143, NaN, 0.825, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, 0.833333333333333, NaN, NaN, NaN, 
0, NaN, NaN, 0.916666666666667, NaN, 0.891304347826087, NaN, 
0.794117647058823, NaN, NaN, 1, NaN, NaN, NaN, NaN, NaN, 0.944444444444444, 
NaN, NaN, NaN, NaN, NaN, NaN, 0.666666666666667, NaN, 0.734375, 
NaN, 0.75, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.583333333333333, 
NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, 0.71875, NaN, 0.625, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, NaN, NaN, 
NaN, 0.45, NaN, 0.683673469387755, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, 0.75, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, 0.670212765957447, NaN, 0.5, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.785714285714286, NaN, NaN, NaN, NaN, NaN, NaN, 0.769230769230769, 
NaN, 0.709302325581395, NaN, 0.9, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.6875, NaN, NaN, NaN, NaN, NaN, NaN, 0.666666666666667, 
NaN, 0.733333333333333, NaN, 1, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.611111111111111, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, 0.823529411764706, NaN, 0.6, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.916666666666667, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, 0.785714285714286, NaN, 0.711538461538462, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, 0.75, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.75, NaN, 0.607843137254902, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, 0.666666666666667, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.75, NaN, 0.625, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, 0.666666666666667, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, 
0.6875, NaN, 0.647058823529412, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.375, NaN, NaN, NaN, NaN, NaN, NaN, 0.75, NaN, 0.545454545454545, 
NaN, 0.774193548387097, NaN, NaN, 1, NaN, NaN, NaN, NaN, NaN, 
0.5, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, 0.583333333333333, 
NaN, 0.65625, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.833333333333333, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.653846153846154, NaN, 
0.75, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, 
NaN, NaN, NaN, 0.875, NaN, 0.875, NaN, 0.725, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, 0.966666666666667, NaN, NaN, NaN, NaN, 
NaN, NaN, 0, NaN, 0.583333333333333, NaN, 0.554054054054054, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.5, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.666666666666667, NaN, 0.58, NaN, 0.708333333333333, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, NaN, 
NaN, NaN, 0.5, NaN, 0.625, NaN, 0.6875, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0, NaN, 0.636363636363636, 
NaN, 0.633333333333333, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.666666666666667, 
NaN, 0.6, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN), .Dim = c(20L, 
38L), .Dimnames = structure(list(c("a3", "a4", "b3", "b4", "c3", 
"c4", "d3", "d4", "e3", "e4", "f3", "f4", "g3", "g4", "h3", "h4", 
"Na3", "Nc3", "Nf3", "Nh3"), c("1975KarpovA", "1976KarpovA", 
"1977KarpovA", "1978KarpovA", "1979KarpovA", "1980KarpovA", "1981KarpovA", 
"1982KarpovA", "1983KarpovA", "1984KarpovA", "1985KasparovG", 
"1986KasparovG", "1987KasparovG", "1988KasparovG", "1989KasparovG", 
"1990KasparovG", "1991KasparovG", "1992KasparovG", "1993KarpovA", 
"1994KarpovA", "1995KarpovA", "1996KarpovA", "1997KarpovA", "1998KarpovA", 
"1999KhalifmanA", "2000AnandV", "2001AnandV", "2002PonomariovR", 
"2003PonomariovR", "2004KasimdzhanovR", "2005TopalovV", "2006KramnikV", 
"2007AnandV", "2008AnandV", "2009AnandV", "2010AnandV", "2011AnandV", 
"2012AnandV")), .Names = c("", "")))
