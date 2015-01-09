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
