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
