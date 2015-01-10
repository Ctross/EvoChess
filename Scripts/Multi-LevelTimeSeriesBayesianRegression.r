library(rstan)
################################################# Parse Data
P<-1009
M<-20
Y<-38
Years<-MissingYears
Years[,1]<-rep(0,P)
for(p in 1:P){
for(y in 2:Y){
Years[p,y]<-ifelse(MissingYears[p,y]==0 & MissingYears[p,(y-1)]==0,1,0)
}}
NonMissSwr<-ifelse(is.na(Swr),0,1)
MSwr<-sum(is.na(Swr))
MissCumSumSwr<-matrix(cumsum(is.na(Swr)), nrow=M, ncol=Y)
MissCumSumSwr<-ifelse(MissCumSumSwr==0,1,MissCumSumSwr)
Swr_na <-Swr
Swr_na[is.na(Swr)]<-9999999999999
NonMissPwr<-ifelse(is.na(Pwr),0,1)
MPwr<-sum(is.na(Pwr))
MissCumSumPwr<-matrix(cumsum(is.na(Pwr)), nrow=M, ncol=Y)
MissCumSumPwr<-ifelse(MissCumSumPwr==0,1,MissCumSumPwr)
Pwr_na <-Pwr
Pwr_na[is.na(Pwr)]<-9999999999999
Ifr[is.na(Ifr)]<-9999999999999
Iwr[is.na(Iwr)]<-9999999999999
I1[is.na(I1)]<-9999999999999
NonMissIwr<-ifelse(is.na(Iwr),0,1)
MIwr<-sum(is.na(Iwr))
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
Swr_na=Swr_na,MSwr=MSwr,NonMissSwr=NonMissSwr,MissCumSumSwr=MissCumSumSwr,
Pwr_na=Pwr_na,MPwr=MPwr,NonMissPwr=NonMissPwr,MissCumSumPwr=MissCumSumPwr
)
############################################################ Begin Stan Code
model_code<-'
data{
int P;
int Y;
int M;
int I1[P,M,Y];
real Iwr[P,M,Y];
real Ifr[P,M,Y];
real Sfr[M,Y];
real Pfr[M,Y];
int Years[P,Y];
real Swr_na[M,Y];
int<lower=0> MSwr;
int NonMissSwr[M,Y]; // Binary to declare Non-missing
int<lower=1> MissCumSumSwr[M,Y]; // Cumulative Sum of Missings
real Pwr_na[M,Y];
int<lower=0> MPwr;
int NonMissPwr[M,Y]; // Binary to declare Non-missing
int<lower=1> MissCumSumPwr[M,Y]; // Cumulative Sum of Missings
}
parameters{
vector[7*M] Psi_z[P];
vector[7*M] MuPsi;
vector<lower=0>[7*M] SdPsi;
corr_matrix[7*M] RhoPsi;
real<lower=0,upper=1> iSwr[MSwr]; // Parameterize Missings
real<lower=0,upper=1> iPwr[MPwr]; // Parameterize Missings
vector<lower=0,upper=1>[20] iIwr[P]; // Parameterize Missings
}
transformed parameters{
vector[7*M] Psi[P];
{ matrix[(7*M),(7*M)] L;
L <- diag_matrix(SdPsi)*cholesky_decompose(RhoPsi);
for ( p in 1:P){
Psi[p] <- MuPsi + (L * Psi_z[p]);
}
}
}
model{
############################## Declare Local Variables
vector[M] Alpha;
int Years_local[Y];
int I1_local[M,Y];
int I2_local[M];
real Iwr_local[M,Y];
real Ifr_local[M,Y];
real Theta[P,M,7];
matrix[M,Y] Swr;
matrix[M,Y] Pwr;
real Iwr2_local[M,Y];
################## Priors
MuPsi ~ normal(0,1);
SdPsi ~ cauchy(0,2.5);
for ( p in 1:P){
Psi_z[p] ~ normal(0,1);
}
################# Parameterize Missing Values and Build Data/Missing-Data-Parameter Matrix
for(m in 1:M){
for(y in 1:Y){
Swr[m,y] <- if_else(NonMissSwr[m,y], Swr_na[m,y], iSwr[MissCumSumSwr[m,y]]);
Pwr[m,y] <- if_else(NonMissPwr[m,y], Pwr_na[m,y], iPwr[MissCumSumPwr[m,y]]);
}}
############################## Reshape Parameters
for ( p in 1:P){
for ( m in 0:(M-1)){
for ( d in 1:7){
Theta[p,(m+1),d] <- Psi[p,(m*7)+d];
}}}
for ( p in 1:P){
############################### Grab individual p
I1_local <- I1[p];
Iwr_local <-Iwr[p];
Ifr_local <-Ifr[p];
Years_local <- Years[p];
for (y in 1:Y){
for(m in 1:M){
if(Iwr_local[m,y]==9999999999999){
Iwr2_local[m,y] <- iIwr[p,m];
}else{
Iwr2_local[m,y] <- Iwr_local[m,y];
} }}
############################### Loop over years where y and y-1 have observations
for (y in 1:Y){
for(m in 1:M){
I2_local[m] <- I1_local[m,y];
}
if(Years_local[y]==1){
############################### Learning Equation
for(m in 1:(M-1)){
Alpha[m] <- Theta[p,m,1] + Theta[p,m,2]*Ifr_local[m,(y-1)] + Theta[p,m,3]*Iwr2_local[m,(y-1)] + Theta[p,m,4]*Sfr[m,(y-1)] + Theta[p,m,5]*Swr[m,(y-1)] + Theta[p,m,6]*Pfr[m,(y-1)] + Theta[p,m,7]*Pwr[m,(y-1)];
}
Alpha[M] <- 0;
I2_local ~ multinomial(softmax(Alpha));
}}}
}'
############# Fit With Optimization
olsChess <- optimizing(stan_model(model_code=model_code), data = model_dat, init=0 ,iter=200000)
############# Fit with MCMC
fitChess <- stan(model_code=model_code, data = model_dat, pars=c("MuPsi","Psi"), init=0, thin=1, iter = 1000, warmup=400, chains=1, refresh=1)
