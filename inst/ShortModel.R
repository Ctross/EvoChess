#################################################
# EvoChess Model of Learning Biases in Chess Data
# Code by: Cody Ross
#

################################################# Load Packages
library(rethinking)

################################################ Load R Workspace
# load('R workspace')

################################################# Parse Data
P<-1009          # Define number of people in full data set

########################### Now count years with games per individual
testz1<-c()
for( i in 1:P){
testz1[i]<-sum(colSums(Ifr[i,,],na.rm=T),na.rm=T)
}

########################### Now lets individuals with less than 10 games
Ifr.cut<-Ifr[which(testz1>9),,]
Iwr.cut<-Iwr[which(testz1>9),,]
I1.cut<-I1[which(testz1>9),,]

Elo.cut <-Elo[which(testz1>9)]
Name.cut <-Name[which(testz1>9)]

########################### Now lets drop all low frequency moves, and keep e4, d4, c4, Nf3
Sfr.cut<-Sfr[c(6,8,10,19),]   # Cut low freq moves for social play rate
Sfr.cut2<-Sfr.cut             # Then rescale
Y<-38                         #
for( i in 1:Y){               #
Sfr.cut2[,i]<-Sfr.cut[,i]/colSums(Sfr.cut)[i] }

Pfr.cut<-Pfr[c(6,8,10,19),]  # Cut low freq moves for prestige play rate
Pfr.cut2<-Pfr.cut            # Then rescale
Y<-38                        #
for( i in 1:Y) {             #
Pfr.cut2[,i]<-Pfr.cut[,i]/colSums(Pfr.cut)[i] }

                                 ################## CHECK HERE FOR ERROR
Ifr.cut<-Ifr.cut[,c(6,8,10,19),] # Cut low freq moves for individual play rate
Ifr.cut2<-Ifr.cut                # Then rescale
Y<-38                            #
P<-958                           # New count of individuals
for(p in 1:P){                   #
for( i in 1:Y){                  #
Ifr.cut2[p,,i]<-ifelse(colSums(Ifr.cut[p,,],na.rm=TRUE)[i]==0,Ifr.cut[p,,i],Ifr.cut[p,,i]/colSums(Ifr.cut[p,,],na.rm=TRUE)[i])
}}

################################# Cut low freq moves for win rateand outcome count
Swr.cut<-Swr[c(6,8,10,19),]
Pwr.cut<-Pwr[c(6,8,10,19),]
Iwr.cut<-Iwr.cut[,c(6,8,10,19),]
I1.cut<-I1.cut[,c(6,8,10,19),]

################################# Indexes
P<-958   # Players in final data set
M<-4     # Moves in final data set
Y<-38    # Years in final data set

################################ Redefine index of missing outcome data, for reduced set of individuals
MissingYears.cut<-MissingYears[which(testz1>9),]

Years<-MissingYears.cut
Years[,1]<-rep(0,P)
for(p in 1:P){
for(y in 2:Y){
   Years[p,y]<-ifelse(MissingYears.cut[p,y]==0 & MissingYears.cut[p,(y-1)]==0,1,0)
   }}

############################### Overwrite old data with new data
I1<-I1.cut
Sfr<-Sfr.cut2
Swr<-Swr.cut
Pfr<-Pfr.cut2
Pwr<-Pwr.cut
Ifr<-Ifr.cut2
Iwr<-Iwr.cut

############################### Classify missing data for Stan
NonMissSwr<-ifelse(is.na(Swr),0,1)
MSwr<-sum(is.na(Swr))
MissCumSumSwr<-matrix(cumsum(is.na(Swr)), nrow=M, ncol=Y)
MissCumSumSwr<-ifelse(MissCumSumSwr==0,1,MissCumSumSwr)
Swr_na <-Swr
Swr_na[is.na(Swr)]<-999999

NonMissPwr<-ifelse(is.na(Pwr),0,1)
MPwr<-sum(is.na(Pwr))
MissCumSumPwr<-matrix(cumsum(is.na(Pwr)), nrow=M, ncol=Y)
MissCumSumPwr<-ifelse(MissCumSumPwr==0,1,MissCumSumPwr)
Pwr_na <-Pwr
Pwr_na[is.na(Pwr)]<-999999

Ifr[is.na(Ifr)]<-999999
Iwr[is.na(Iwr)]<-999999
I1[is.na(I1)]<-999999

NonMissIwr<-ifelse(is.na(Iwr),0,1)
MIwr<-sum(is.na(Iwr))

############################# Define data list for Stan
model_dat<-list(
 P=P,
 Y=Y,
 M=M,
 I1=I1,
 Iwr=Iwr,
 Ifr=Ifr,
 Sfr=Sfr,
 Pfr=Pfr,
 Years=Years,
Swr=Swr,
Pwr_na=Pwr_na,MPwr=MPwr,NonMissPwr=NonMissPwr,MissCumSumPwr=MissCumSumPwr
)

#################### For CmdStan dump data, for Rstan ignore below
stan_rdump(c('P',
 'Y',
 'M',
'I1',
'Iwr',
'Ifr',
'Sfr',
'Pfr',
'Years',
'Swr',
'Pwr_na','MPwr','NonMissPwr','MissCumSumPwr'),"chess.data.R")
#rm(list=setdiff(ls(), "model_dat"))

################################################################################
############################################################ Begin Stan Code
################################################################################

model_code<-'
data{
##################################### In this block we declare our data
######## Indexes
 int P;          # Players
 int Y;          # Years
 int M;          # Moves

 int I1[P,M,Y];   # Outcome count of opening moves, M, by player, P, and year, Y

 real Iwr[P,M,Y]; # Individual win rate, by player, P, move, M, and year, Y
 real Ifr[P,M,Y]; # Individual playing rate, by player, P, move, M, and year, Y

 real Sfr[M,Y];   # Social playing rate, by move, M, and year, Y
 real Pfr[M,Y];   # Prestige playing rate, by move, M, and year, Y

 real Swr[M,Y];    # Social win rate, by move, M, and year, Y
 real Pwr_na[M,Y]; # Prestige win rate, by move, M, and year, Y

 int Years[P,Y];   # Indicate which years for which individuals are missing data

 int<lower=0> MPwr;               # Number of missing data points in PWr
 int NonMissPwr[M,Y];             # Binary to declare Non-missing of Pwr
 int<lower=1> MissCumSumPwr[M,Y]; # Cumulative Sum of Missings for Pwr
}

parameters{
##################################### In this block we declare our Parameters
  vector[7*M] Psi_z[P];           # Unscaled individual-level regression parameters, see Matt Trick
  vector[7*M] MuPsi;              # Population-level Mean regression parameters
  vector<lower=0>[7*M] SdPsi;     # Dispersion of individual-level parameters from MuPsi
  corr_matrix[7*M] RhoPsi;        # Correlation of individual-level parameters

  real<lower=0,upper=1> iPwr[MPwr];    # Parameterize Missings, prestige win rate
  vector<lower=0,upper=1>[4] iIwr[P];  # Parameterize Missings, individual win rate
}

transformed parameters{
##################################### In this block we transform some Parameters for speed in MCMC
  vector[7*M] Psi[P];           # Properly scaled individual-level regression parameters, see Matt Trick
  
  { matrix[(7*M),(7*M)] L;      # Local parameter to store the choleksy decomposed covariance matrix
     L <- diag_matrix(SdPsi)*cholesky_decompose(RhoPsi); #  Choleksy decompose of covariance matrix 

# For each individual, their Psi parameters are the mean vector, plus their deviaton from the mean vector Psi_z,
# as scaled by the covariance matrix.  
   for ( p in 1:P){
      Psi[p] <- MuPsi + (L * Psi_z[p]);  
                  }
     }
}

model{
##################################### In this block we define the statistical model
############################## Declare Local Variables
 vector[M] Alpha;      # Used to store results of link function

 int Years_local[Y];   # Used to store years with data for a given individual

 int I1_local[M,Y];    # Used to store outcome moves for a given individual
 int I2_local[M];      # Used to store outcome moves for a given individual in a given year

 real Iwr_local[M,Y];  # Used to store win rate of moves for a given individual
 real Ifr_local[M,Y];  # Used to store play rate of moves for a given individual

 real Theta[P,M,7];    # Used to reshape Psi from vector to matrix

 matrix[M,Y] Pwr;      # Prestige win rate, mix of data and missing data parameters

 real Iwr2_local[M,Y]; # Individual win rate, mix of data and missing data parameters


################################################### Priors
 MuPsi ~ normal(0,1);    # Regularize mean vector 

 SdPsi ~ cauchy(0,2.5);  # Regularize dispersion vector 
  
 for ( p in 1:P){
   Psi_z[p] ~ normal(0,1);  # see Matt Trick
   iIwr[p] ~ beta(2,2);     # Weak information for missing win rate for each move in individual p
                }

  iPwr ~ beta(2,2);         # Weak information for missing win rate for each move in prestige data
  
################# Parameterize Missing Values and Build Data/Missing-Data-Parameter Matrix, Prestige win rate
  for(m in 1:M){
  for(y in 1:Y){
          Pwr[m,y] <- if_else(NonMissPwr[m,y], Pwr_na[m,y], iPwr[MissCumSumPwr[m,y]]);
                }}
                            
############################## Reshape Main Regression Parameters
 for ( p in 1:P){
 for ( m in 0:(M-1)){
 for ( d in 1:7){
      Theta[p,(m+1),d] <- Psi[p,(m*7)+d];
                }}}

#############################################  Probability function begins here
#### We iterate the following code for each individual
 for ( p in 1:P){
############################### Grab individual p
   I1_local <- I1[p];       # Store params for individual p locally
   Iwr_local <-Iwr[p];      #
   Ifr_local <-Ifr[p];      #
   Years_local <- Years[p]; #

# Check for missing values in individual-level data and replace with missing data parameters
 for(y in 1:Y){
 for(m in 1:M){  
   if(Iwr_local[m,y]==999999){
      Iwr2_local[m,y] <- iIwr[p,m];
       }else{
      Iwr2_local[m,y] <- Iwr_local[m,y]; 
       } 
               }} 

############################### Loop over years check where y and y-1 have observations 
######################## so that an AR-1 auto-regression is well defined
 for (y in 1:Y){
 
################# Outcome moves are stored in a year specific vector
 for(m in 1:M){
    I2_local[m] <- I1_local[m,y];
              }
###### Check where y and y-1 have observations so that an AR-1 auto-regression is well defined
 if(Years_local[y]==1){
 
############################################################# Learning Equations
# for each move, m, model outcome as a function of predictors unique to that move
 for(m in 1:(M-1)){
   Alpha[m] <- Theta[p,m,1] +  Theta[p,m,2]*Ifr_local[m,(y-1)] + Theta[p,m,3]*Iwr2_local[m,(y-1)] + Theta[p,m,4]*Sfr[m,(y-1)] + Theta[p,m,5]*Swr[m,(y-1)] + Theta[p,m,6]*Pfr[m,(y-1)] + Theta[p,m,7]*Pwr[m,(y-1)];
              }

# Fix the intercept on the last move to 0 for identification              
Alpha[M] <- 0 +  Theta[p,M,2]*Ifr_local[M,(y-1)] + Theta[p,M,3]*Iwr2_local[M,(y-1)] + Theta[p,M,4]*Sfr[M,(y-1)] + Theta[p,M,5]*Swr[M,(y-1)] + Theta[p,M,6]*Pfr[M,(y-1)] + Theta[p,M,7]*Pwr[M,(y-1)];

# transform Alpha with softmax and then model the outcome vector from a multinomial distribution
   I2_local ~ multinomial(softmax(Alpha));
      }
      
      }
      }
}'

############# Fit With Optimization
#olsChess <- optimizing(stan_model(model_code=model_code), data = model_dat, init=0 ,iter=2000)

############# Fit with MCMC
fitChess <- stan(model_code=model_code, data = model_dat, init=0, pars=c("MuPsi","Psi"), thin=1, iter = 2000, warmup=1000, chains=2, refresh=1)

   
################################################################################# Post Process Samples


  MuPsi <-extract(fitChess, pars="MuPsi")$MuPsi

  SAMP<-2000

  MuTheta <- array(NA,dim=c(M,7))
   SdTheta <- array(NA,dim=c(M,7))
  RawTheta <- array(NA,dim=c(SAMP,M,7))
       for ( m in 0:(M-1)){
       for ( d in 1:7){
        RawTheta[,(m+1),d] <-( MuPsi[,(m*7)+d] )
        MuTheta[(m+1),d] <-mean( MuPsi[,(m*7)+d] )
               SdTheta[(m+1),d]   <-sd( MuPsi[,(m*7)+d] )
                }}
   
   rownames(MuTheta)<-rownames(Swr)
   colnames(MuTheta)<-c("Intercept", "Ifreq","Iwin","Sfreq","Swin","Pfreq","Pwin")
         round(MuTheta,2)             

   rownames(SdTheta)<-rownames(Swr)
   colnames(SdTheta)<-c("Intercept", "Ifreq","Iwin","Sfreq","Swin","Pfreq","Pwin")
         round(SdTheta,2)

   XX<- pmin(pnorm(0,mean=MuTheta,sd=SdTheta, lower.tail = TRUE, log.p = FALSE),
               1-pnorm(0,mean=MuTheta,sd=SdTheta, lower.tail = TRUE, log.p = FALSE))
     round(XX,2)



########################## Everything Below Is SCRAP It worked once on other data
# It wont run, but it has clues as to how to do things. We will clean this up together.









     Softmax<-function(DD) {
 FFF<-exp(DD)/sum(exp(DD))
 return( FFF)}




 Ifr_local<-c(0.08,0.4,0.42,0.1)
 Iwr_local<-c(0.55,0.55,0.55,0.55)
 Swr_local<-c(0.55,0.55,0.55,0.55)
 Pfr_local<-c(0.12,0.38,0.42,0.08)
 Pwr_local<-c(0.55,0.55,0.55,0.55)
  for(i in 1:SAMP){
 for(k in 1:80){
   Sfr_local<-c(0.1,(0.8-(k/100)),(k/100),0.1)

  Alpha<-c()
  for(m in 1:(M-1)){
   Alpha[m] <- RawTheta[i,m,1] +  RawTheta[i,m,2]*Ifr_local[m] + RawTheta[i,m,3]*Iwr_local[m] + RawTheta[i,m,4]*Sfr_local[m] + RawTheta[i,m,5]*Swr_local[m] + RawTheta[i,m,6]*Pfr_local[m] + RawTheta[i,m,7]*Pwr_local[m]
              }
   Alpha[M] <- 0

   G<-Softmax(Alpha)

   }  }









 
  Name<-unique(R100$NameWhite)     
    Elo<-c()
  for( i in 1:length(Name)){
    F1 <-  R100[which(R100$NameWhite==Name[i]),]
    Elo[i]<-mean(F1$EloW)
    }
 
             matrix( olsChess$par[ 182188:length(olsChess$par)],nrow=1009,ncol=(7

 IND<-extract(fitChess, pars="Psi")$Psi
   N<-P
 mJw<-matrix(ncol=3,nrow=N)
 mJf<-matrix(ncol=3,nrow=N)
 for( i in 1:N){
   IndTheta <- array(NA,dim=c(M,7)) 
   
   
       for ( m in 0:(M-1)){
       for ( d in 1:7){
         IndTheta[(m+1),d] <- IND[i,(m*7)+d];
                }}
   
 #  rownames(IndTheta)<-rownames(Swr)
 #  colnames(IndTheta)<-c("Intercept", "Ifreq","Iwin","Sfreq","Swin","Pfreq","Pwin")
 #  round(IndTheta,2)          
         
      mJw[i,] <-  c(IndTheta[PP,3],IndTheta[PP,7],IndTheta[PP,5])       #win
      mJf[i,] <-  c(IndTheta[PP,2],IndTheta[PP,6],IndTheta[PP,4])   }   #freq
         
 ####################### Tertiary plot
 library(vcd)
 library(plotrix)
 library(RColorBrewer)
 
 add.alpha <- function(col, alpha=1){
if(missing(col))
stop("Please provide a vector of colours.")
apply(sapply(col, col2rgb)/255, 2,
function(x)
rgb(x[1], x[2], x[3], alpha=alpha))
}

setwd("C:/Users/Empyrean/Desktop/Dropbox/Open Papers/The Evolution of Chess Strategy/Latex Paper/Figures")

CairoPDF("Freq-e4.pdf", width=8,height=8)
 colnames(mJf)<-c("I-PR", "P-PR", "S-PR")
       palette(add.alpha(brewer.pal(9, "OrRd"),0.8)) 
ternaryplot((mJf+(min(mJf)*-1)),pch=20, cex=0.7,col=plotrix:::rescale(Elo,c(1,10)),main="Frequency-Biased Learning (e4)")
dev.off()

CairoPDF("Win-e4.pdf", width=8,height=8)
 colnames(mJw)<-c("I-WR", "P-WR", "S-WR")
       palette(add.alpha(brewer.pal(9, "OrRd"),0.8)) 
ternaryplot((mJw+(min(mJw)*-1)),pch=20, cex=0.7,col=plotrix:::rescale(Elo,c(1,10)),main="Sucess-Biased Learning (e4)")
dev.off()

 CairoPDF("abs-Freq-e4.pdf", width=8,height=8)
 colnames(mJf)<-c("I-PR", "P-PR", "S-PR")
       palette(add.alpha(brewer.pal(9, "OrRd"),0.8)) 
ternaryplot(abs(mJf),pch=20, cex=0.7,col=plotrix:::rescale(Elo,c(1,10)),main="Frequency-Biased Learning (e4)")
 dev.off()

CairoPDF("abs-Win-e4.pdf", width=8,height=8)
 colnames(mJw)<-c("I-WR", "P-WR", "S-WR")
       palette(add.alpha(brewer.pal(9, "OrRd"),0.8)) 
ternaryplot(abs(mJw),pch=20, cex=0.7,col=plotrix:::rescale(Elo,c(1,10)),main="Sucess-Biased Learning (e4)")
 dev.off()


 PP <- 19
CairoPDF("Rates-Nf3.pdf", width=12,height=6)
plot(0:16,0:16,type="n", xlim=c(-0.2,1.2),main="Relative Weight of Learning (Nf3)",yaxt="n",ylab="",xlab="Relative Absolute Parameter Size",xaxt="n")
segments(0,c(1:15),1,c(1:15))
axis(1,at=c(0,.5,1),label=c(1,.5,1))

text(-0.1,c(15:1),c("I-pr", "I-pr", "I-pr", "I-pr", "I-pr", "I-wr",  "I-wr", "I-wr", "I-wr", "S-pr", "S-pr", "S-pr", "S-wr", "S-wr","P-pr"))
text(1.1,c(15:1),c("I-wr", "S-pr", "S-wr", "P-pr", "P-wr", "S-pr",  "S-wr", "P-pr", "P-wr", "S-wr", "P-pr", "P-wr", "P-pr", "P-wr","P-wr"))

 points(1- abs(MuTheta[PP,2])/(abs(MuTheta[PP,2])+abs(MuTheta[PP,3])),15,col="indianred",pch=18,cex=2)
  points(1- abs(MuTheta[PP,2])/(abs(MuTheta[PP,2])+abs(MuTheta[PP,4])),14,col="indianred",pch=18,cex=2)
   points(1- abs(MuTheta[PP,2])/(abs(MuTheta[PP,2])+abs(MuTheta[PP,5])),13,col="indianred",pch=18,cex=2)
    points(1- abs(MuTheta[PP,2])/(abs(MuTheta[PP,2])+abs(MuTheta[PP,6])),12,col="indianred",pch=18,cex=2)
     points(1- abs(MuTheta[PP,2])/(abs(MuTheta[PP,2])+abs(MuTheta[PP,7])),11,col="indianred",pch=18,cex=2)
      points(1- abs(MuTheta[PP,3])/(abs(MuTheta[PP,3])+abs(MuTheta[PP,4])),10,col="indianred",pch=18,cex=2)
       points(1- abs(MuTheta[PP,3])/(abs(MuTheta[PP,3])+abs(MuTheta[PP,5])),9,col="indianred",pch=18,cex=2)
        points(1- abs(MuTheta[PP,3])/(abs(MuTheta[PP,3])+abs(MuTheta[PP,6])),8,col="indianred",pch=18,cex=2)
         points(1- abs(MuTheta[PP,3])/(abs(MuTheta[PP,3])+abs(MuTheta[PP,7])),7,col="indianred",pch=18,cex=2)
          points(1- abs(MuTheta[PP,4])/(abs(MuTheta[PP,4])+abs(MuTheta[PP,5])),6,col="indianred",pch=18,cex=2)
           points(1- abs(MuTheta[PP,4])/(abs(MuTheta[PP,4])+abs(MuTheta[PP,6])),5,col="indianred",pch=18,cex=2)
            points(1- abs(MuTheta[PP,4])/(abs(MuTheta[PP,4])+abs(MuTheta[PP,7])),4,col="indianred",pch=18,cex=2)
             points(1- abs(MuTheta[PP,5])/(abs(MuTheta[PP,5])+abs(MuTheta[PP,6])),3,col="indianred",pch=18,cex=2)
              points(1- abs(MuTheta[PP,5])/(abs(MuTheta[PP,5])+abs(MuTheta[PP,7])),2,col="indianred",pch=18,cex=2)
               points(1- abs(MuTheta[PP,6])/(abs(MuTheta[PP,6])+abs(MuTheta[PP,7])),1,col="indianred",pch=18,cex=2)
             dev.off()  



  
 
  
################################################################################################################### Plot All
########################### Cross Tab Opening by Date
library(Cairo)
G<-Sfr
G2<-G
for(i in 1:dim(G)[2])
G2[,i]<-G[,i]/sum(G[,i])

G4<-G2

for(y in 1:38){
G4[,y]<-rev(cumsum(rev(G2[,y])))
}
 CairoPDF("History-S.pdf", width=8,height=6)
 plot(G4[2,]~c(1975:2012),type="l", ylim=c(0,1),xlab="Date", ylab="Frequency of Opening Move")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[1,]) , rev((G4[2,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[2,]) , rev((G4[3,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[3,]) , rev((G4[4,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[4,]) , rev((G4[5,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[5,]) , rev((G4[6,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[6,]) , rev((G4[7,])))) , col = '#bebada'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[7,]) , rev((G4[8,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[8,]) , rev((G4[9,])))) , col = '#8dd3c7'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[9,]) , rev((G4[10,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[10,]) , rev((G4[11,])))) , col = '#b3de69'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[11,]) , rev((G4[12,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[12,]) , rev((G4[13,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[13,]) , rev((G4[14,])))) , col = '#fdb462'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[14,]) , rev((G4[15,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[15,]) , rev((G4[16,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[16,]) , rev((G4[17,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[17,]) , rev((G4[18,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[18,]) , rev((G4[19,])))) , col = '#ffffb3'  , border = "black")
polygon(c(c(1975:2012), rev(c(1975:2012))), c(c((G4[19,]) , rev((G4[20,])))) , col = 'indianred'  , border = "black")

text(c(1977,1977,1977,1977),c(.92,.7,.38,.09),c("c4","d4","e4","Nf3"))

#text(c(1989,1988,2002,1998),c(.9,.4,.42,.08),c("c4","d4","e4","Nf3"))
dev.off()


  library(Cairo)
setwd("C:/Users/Empyrean/Desktop/Dropbox/Open Papers/The Evolution of Chess Strategy/Latex Paper/Figures")
 G4<-Pwr
 CairoPDF("Win-P.pdf", width=8,height=6)
 plot(G4[6,]~c(1975:2012),type="n", ylim=c(0.1,1),xlab="Date", ylab="Winning Ratio of Opening Move (WC)",bg="gray")
 abline(h=0.5)
lines(G4[13,]~c(1975:2012), lwd=6)
lines(G4[13,]~c(1975:2012), lwd=4,  col = '#fdb462' )
lines(G4[6,]~c(1975:2012),  lwd=6)      
lines(G4[6,]~c(1975:2012),  lwd=4,col = '#bebada') 
lines(G4[19,]~c(1975:2012), lwd=6)
lines(G4[19,]~c(1975:2012), lwd=4, col = 'indianred')  
 lines(G4[8,]~c(1975:2012), lwd=6)
 lines(G4[8,]~c(1975:2012),  lwd=4,  col = '#8dd3c7')
lines(G4[10,]~c(1975:2012), lwd=6)
lines(G4[10,]~c(1975:2012), lwd=4,  col = '#b3de69')


legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend
c("c4","Nf3","d4","e4"),
lty=c(1,1), # gives the legend appropriate symbols (lines)
lwd=c(4,4,4,4),col=c('#bebada','indianred','#8dd3c7','#b3de69')) # gives the legend lines the correct color and width
  dev.off()
