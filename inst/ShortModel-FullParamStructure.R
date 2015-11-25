#################################################
# EvoChess Model of Learning Biases in Chess Data
# Code by: Cody Ross

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
  vector[25*M] Psi_z[P];           # Unscaled individual-level regression parameters, see Matt Trick
  vector[25*M] MuPsi;              # Population-level Mean regression parameters
  vector<lower=0>[25*M] SdPsi;     # Dispersion of individual-level parameters from MuPsi
  corr_matrix[25*M] RhoPsi;        # Correlation of individual-level parameters

  real<lower=0,upper=1> iPwr[MPwr];    # Parameterize Missings, prestige win rate
  vector<lower=0,upper=1>[4] iIwr[P];  # Parameterize Missings, individual win rate
}

transformed parameters{
##################################### In this block we transform some Parameters for speed in MCMC
  vector[25*M] Psi[P];           # Properly scaled individual-level regression parameters, see Matt Trick
  
  { matrix[(25*M),(25*M)] L;      # Local parameter to store the choleksy decomposed covariance matrix
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

 real Theta[M,25];      # Used to reshape Psi from vector to matrix

 matrix[M,Y] Pwr;      # Prestige win rate, mix of data and missing data parameters

 real Iwr2_local[M,Y]; # Individual win rate, mix of data and missing data parameters
 
 vector[25] Predictors; # Vector of predictors, local


################################################### Priors
 MuPsi ~ normal(0,1);    # Regularize mean vector 

 SdPsi ~ cauchy(0,1);    # Regularize dispersion vector 
  
 for ( p in 1:P){
   Psi_z[p] ~ normal(0,1);  # see Matt Trick
   iIwr[p] ~ beta(1,1);     # Weak information for missing win rate for each move in individual p
                }

  iPwr ~ beta(1,1);         # Weak information for missing win rate for each move in prestige data
  
################# Parameterize Missing Values and Build Data/Missing-Data-Parameter Matrix, Prestige win rate
  for(m in 1:M){
  for(y in 1:Y){
          Pwr[m,y] <- if_else(NonMissPwr[m,y], Pwr_na[m,y], iPwr[MissCumSumPwr[m,y]]);
                }}
                            
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
               
############################## Reshape Main Regression Parameters
 for ( m in 0:(M-1)){
 for ( d in 1:25){
      Theta[(m+1),d] <- Psi[p,(m*25)+d];
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
Predictors[1]<-1;
Predictors[2]<-Ifr_local[1,(y-1)];
Predictors[3]<-Ifr_local[2,(y-1)];
Predictors[4]<-Ifr_local[3,(y-1)];
Predictors[5]<-Ifr_local[4,(y-1)];
Predictors[6]<-Iwr2_local[1,(y-1)];
Predictors[7]<-Iwr2_local[2,(y-1)];
Predictors[8]<-Iwr2_local[3,(y-1)];
Predictors[9]<-Iwr2_local[4,(y-1)];
Predictors[10]<-Sfr[1,(y-1)];
Predictors[11]<-Sfr[2,(y-1)];
Predictors[12]<-Sfr[3,(y-1)];
Predictors[13]<-Sfr[4,(y-1)];
Predictors[14]<-Swr[1,(y-1)];
Predictors[15]<-Swr[2,(y-1)];
Predictors[16]<-Swr[3,(y-1)];
Predictors[17]<-Swr[4,(y-1)];
Predictors[18]<-Pfr[1,(y-1)];
Predictors[19]<-Pfr[2,(y-1)];
Predictors[20]<-Pfr[3,(y-1)];
Predictors[21]<-Pfr[4,(y-1)];
Predictors[22]<-Pwr[1,(y-1)];
Predictors[23]<-Pwr[2,(y-1)];
Predictors[24]<-Pwr[3,(y-1)];
Predictors[25]<-Pwr[4,(y-1)];

# transform Alpha with softmax and then model the outcome vector from a multinomial distribution
   I2_local ~ multinomial(softmax(Theta* Predictors));
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

  MuTheta <- array(NA,dim=c(M,25))
   SdTheta <- array(NA,dim=c(M,25))
  RawTheta <- array(NA,dim=c(SAMP,M,25))
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


########################## Elo and Names

Elo<-c(2428.4, 2483.7, 2475.1, 2604, 2563.2, 2530.4, 2343.8, 2570.5,
2559.1, 2472.6, 2611, 2517.6, 2523.3, 2605.9, 2640.8, 2441.9,
2582.2, 2726.5, 2556.1, 2592.6, 2421.8, 2385.1, 2493, 2523.2,
2495.3, 2442.1, 2523.6, 2458.8, 2440.6, 2695.1, 2431.6, 2433.2,
2517.7, 2591.6, 2566.5, 2602.2, 2626.1, 2568.5, 2549.1, 2669.2,
2493, 2472.1, 2495.3, 2611.5, 2452.1, 2531.3, 2551.5, 2661.4,
2399.9, 2474.4, 2514.1, 2497.6, 2547.5, 2629.8, 2382.2, 2290.5,
2549.1, 2535.7, 2410.1, 2465.7, 2438.6, 2523.7, 2484.3, 2514.8,
2523.3, 2489.7, 2407.4, 2643.2, 2461.2, 2415.8, 2428.8, 2337.8,
2540.9, 2464.6, 2528.8, 2263.6, 2597.2, 2478, 2569.7, 2479.8,
2523.4, 2500.2, 2533.1, 2537.3, 2345.9, 2577.7, 2539, 2495, 2559.4,
2552.7, 2499.1, 2456.5, 2428.5, 2482.4, 2508.4, 2357.6, 2503.7,
2385.6, 2481.9, 2531.6, 2525.7, 2446, 2420.3, 2565, 2446.9, 2403.3,
2544.7, 2600.3, 2467.8, 2550.3, 2429.6, 2483.9, 2593.1, 2392.2,
2537.2, 2531.1, 2524.2, 2501.6, 2458.4, 2478.6, 2573, 2586.5,
2576.1, 2500, 2493.3, 2653, 2552.9, 2368.6, 2533.1, 2505.7, 2538.6,
2507.6, 2485.1, 2604, 2561.9, 2493.3, 2317.3, 2595.8, 2475.7,
2499.2, 2465.5, 2344.8, 2257.8, 2494.5, 2300.5, 2596.9, 2509.3,
2472.9, 2337.4, 2503.5, 2481.3, 2448.1, 2380.2, 2421, 2536.2,
2482, 2623.7, 2584, 2567.4, 2469.5, 2568.3, 2488.4, 2418.7, 2564.6,
2431.1, 2325.4, 2472.6, 2653.4, 2501, 2528, 2494.8, 2575.2, 2698.3,
2559.4, 2565.6, 2521.9, 2355.1, 2426.8, 2557.5, 2548.2, 2536.8,
2511.9, 2566.7, 2564, 2540.6, 2512.6, 2489.7, 2457, 2604.3, 2547.3,
2704, 2481.6, 2371.5, 2501, 2581.1, 2401.3, 2511.7, 2627.6, 2575.4,
2471.1, 2569.8, 2509.3, 2557.6, 2453.9, 2348.6, 2555.1, 2602.2,
2346.7, 2480.7, 2458.5, 2478.7, 2515.5, 2521.4, 2402.7, 2488.2,
2530.5, 2535.2, 2518.9, 2571.4, 2534.3, 2369.6, 2466.9, 2410.2,
2571.1, 2502.3, 2596.5, 2608.7, 2525.7, 2406.8, 2401.5, 2573.1,
2568.3, 2509.5, 2525.8, 2580.8, 2620.4, 2473.2, 2517, 2587.7,
2412.8, 2482.5, 2591.6, 2440.7, 2513.5, 2720.9, 2577.6, 2481.4,
2492.1, 2374.4, 2387.9, 2663.4, 2513.2, 2463.8, 2479.3, 2512.3,
2483.3, 2466.1, 2421.2, 2348.8, 2575.5, 2317.1, 2576.1, 2471.8,
2686.3, 2482.4, 2721.8, 2657.8, 2778.3, 2482.6, 2564.2, 2593.5,
2573.3, 2554.8, 2634.2, 2574.7, 2602, 2470.7, 2431.5, 2515.9,
2494.7, 2356.8, 2452.3, 2347.3, 2482.7, 2556.9, 2467.8, 2396.4,
2449.4, 2591.1, 2553.8, 2634.3, 2500.9, 2509, 2467.8, 2574.6,
2554.8, 2549.1, 2453, 2453.2, 2588.4, 2465.9, 2746.4, 2625.1,
2271.6, 2483.2, 2534.9, 2436.5, 2482.2, 2534.4, 2536.6, 2515.3,
2479.7, 2540.9, 2531, 2524.4, 2520.1, 2602.5, 2413.2, 2403, 2418.1,
2496.7, 2576.2, 2595, 2485.3, 2621, 2478.6, 2473.7, 2572.1, 2697.2,
2309.2, 2535, 2507.6, 2435.9, 2520.6, 2435, 2463.3, 2405.8, 2590.9,
2531.8, 2399.3, 2415.4, 2468.9, 2302.3, 2577.8, 2514, 2472.8,
2537.8, 2572.5, 2220.5, 2594.3, 2389.6, 2520.9, 2477.5, 2515.4,
2263, 2269.4, 2563.5, 2664.2, 2538.8, 2480.5, 2386.5, 2447, 2532.9,
2562.7, 2440.7, 2488.3, 2493.5, 2381.2, 2457.7, 2434.4, 2507.4,
2419.8, 2573.8, 2428.7, 2557.4, 2491, 2473.8, 2471.8, 2391.6,
2528, 2515.2, 2549.1, 2556.2, 2542.6, 2581.1, 2624.5, 2602.1,
2524.2, 2515.3, 2568.3, 2424.6, 2311.4, 2478.8, 2715.2, 2451.8,
2529.9, 2481.3, 2636.1, 2663.5, 2504.8, 2395.6, 2455.8, 2632.6,
2612.7, 2461.6, 2515.4, 2548.2, 2493.5, 2461.8, 2460.3, 2426,
2552.4, 2539.1, 2501.6, 2609.4, 2543.6, 2517, 2613.7, 2542.8,
2553, 2572.1, 2450.1, 2482.5, 2300.5, 2570.4, 2525.9, 2516.9,
2366.5, 2525.8, 2425.1, 2569.1, 2383.8, 2505.4, 2496.2, 2377.5,
2510.6, 2542.4, 2550.1, 2418.9, 2416, 2473.6, 2479.3, 2305.4,
2529.6, 2355.8, 2559, 2566.2, 2510.5, 2416.2, 2546.3, 2421.9,
2472.5, 2442.8, 2496.7, 2613.3, 2514.4, 2618.3, 2703.7, 2455.4,
2605.9, 2566.1, 2413.8, 2467.8, 2570.6, 2443.9, 2509.6, 2436.9,
2460.2, 2501.1, 2506.1, 2389.8, 2532.5, 2463.9, 2455.1, 2518.3,
2442.6, 2494.8, 2225.9, 2590.1, 2589.3, 2516.1, 2489.5, 2437.9,
2547.1, 2492, 2510.6, 2420, 2560.3, 2486.7, 2511.4, 2522.8, 2411.2,
2581.6, 2653.7, 2525.8, 2291.3, 2420.6, 2573.6, 2532.1, 2578.1,
2561.6, 2617.8, 2642.9, 2534.7, 2617.3, 2378.5, 2410.1, 2538.5,
2549.6, 2504.3, 2469.4, 2531.7, 2461.7, 2462.9, 2382.5, 2417.5,
2595.6, 2441.8, 2444.3, 2542.3, 2526.6, 2579.1, 2530.8, 2438.3,
2542.8, 2517.8, 2531.9, 2576.4, 2421.4, 2699.1, 2634, 2571.8,
2429.4, 2454.9, 2455.2, 2450, 2368.9, 2477.3, 2526.3, 2526.6,
2550.3, 2631.1, 2577.6, 2563.6, 2591.7, 2561.5, 2633.8, 2548.8,
2439.8, 2508.9, 2440.6, 2539, 2535.6, 2342.1, 2561.8, 2601.5,
2569.6, 2561.6, 2433.2, 2519.2, 2475.8, 2476.9, 2457.9, 2499.8,
2552.4, 2547.2, 2551.3, 2543.1, 2518.5, 2539.8, 2550.1, 2507,
2504.5, 2542, 2443, 2493, 2632, 2533.6, 2547, 2699.9, 2320.6,
2342.4, 2478.4, 2618.6, 2412.4, 2404.8, 2401, 2511.4, 2461.6,
2384.6, 2444.8, 2603.3, 2550.4, 2489.5, 2407.5, 2631.5, 2636.4,
2711.5, 2529.8, 2446.4, 2461.3, 2601.8, 2545.9, 2568.4, 2464.1,
2567.4, 2516.2, 2489.7, 2538.9, 2466.1, 2359.8, 2606.6, 2547.8,
2504, 2362.3, 2636.9, 2372, 2546.1, 2500.2, 2412.9, 2444.9, 2523.6,
2397.2, 2626.3, 2399.2, 2520.3, 2393.2, 2458.7, 2512.8, 2518.8,
2566.4, 2423.3, 2476.9, 2327.8, 2474.5, 2486.2, 2448.8, 2517.2,
2558.2, 2507.2, 2478.3, 2540.5, 2527.9, 2470.5, 2491.7, 2369.2,
2488.6, 2395.9, 2491.3, 2332.6, 2412, 2551.9, 2529.8, 2562.4,
2547.3, 2476, 2539.5, 2559.8, 2537.2, 2565.2, 2607.1, 2494.4,
2464.2, 2504.6, 2509.6, 2454.6, 2536.9, 2571.6, 2599, 2500.1,
2445.9, 2369.3, 2537.7, 2639.6, 2624.6, 2653.9, 2666.7, 2614.9,
2486.9, 2658.7, 2529.3, 2389.1, 2404.6, 2598, 2562.2, 2695.9,
2648.2, 2424.6, 2410.3, 2542.3, 2517.9, 2539.1, 2638.4, 2415.8,
2527.6, 2522, 2471.9, 2461.1, 2465.3, 2487.6, 2555.7, 2433.2,
2677.4, 2530.8, 2452.8, 2584.2, 2618.8, 2480.5, 2523.3, 2567.4,
2650.7, 2386.2, 2483, 2511.1, 2497.2, 2528.5, 2561.2, 2576, 2651.5,
2480.2, 2537.1, 2551.8, 2565.7, 2546.3, 2566, 2535.5, 2450.7,
2599.2, 2493.1, 2440.1, 2491.5, 2463.4, 2561.2, 2426.4, 2462.1,
2415.4, 2345, 2330.6, 2423, 2377, 2328.4, 2387.5, 2436.3, 2400.2,
2355.7, 2498.5, 2438.1, 2617.7, 2619, 2582.8, 2576.4, 2474.3,
2504.1, 2656.8, 2608.7, 2554.4, 2555.6, 2631.9, 2640.2, 2471.5,
2528.5, 2506.6, 2566.2, 2644.9, 2574, 2446, 2639.8, 2578, 2562.4,
2603.5, 2675.8, 2560.6, 2564.2, 2562.5, 2566, 2638.3, 2546, 2593.7,
2515.5, 2472.6, 2511.7, 2378.8, 2445.5, 2527.6, 2538.6, 2505.7,
2473.1, 2497, 2483.5, 2479.1, 2450.5, 2555.7, 2448.8, 2546.2,
2528.6, 2490, 2423.3, 2466, 2553.7, 2421.6, 2458.8, 2442.5, 2453.6,
2579.9, 2483.9, 2569.7, 2424.7, 2440.2, 2488.6, 2409, 2406.5,
2270.3, 2298.8, 2459.2, 2452.8, 2558.8, 2507, 2511.3, 2453.1,
2472.8, 2561.6, 2536.1, 2604, 2424.4, 2576.8, 2470.3, 2555.9,
2380.6, 2577.5, 2518.8, 2419.7, 2330.2, 2512, 2373, 2389.2, 2445.5,
2515.2, 2561.5, 2482.5, 2530.2, 2435.8, 2428.2, 2622.3, 2309.7,
2576.9, 2453.8, 2300.6, 2660.8, 2607.1, 2509.7, 2574.2, 2454.1,
2468.8, 2475.6, 2489.8, 2725.3, 2478.8, 2505.3, 2475.1, 2427.7,
2505.1, 2481, 2327.1, 2348.4, 2456.7, 2502.9, 2549.4, 2370.3,
2470.9, 2367, 2468.1, 2512.4, 2455.5, 2383.1, 2600.6, 2603.8,
2604.9, 2536.7, 2577.8, 2541.7, 2227.6, 2424.3, 2390.3, 2244.4,
2371.8, 2498.6, 2423.5, 2514.5, 2447.6, 2409, 2456.6, 2494.6,
2536.9, 2563, 2579.2, 2523, 2499.2, 2633.3, 2467.6, 2511.9, 2490.4,
2348.3, 2418.4, 2529.1, 2377.4, 2524.4, 2478.1, 2596.1, 2380.4,
2501.5, 2605.7, 2551.5, 2504.7, 2529.8, 2455.6, 2417, 2489.2,
2338.1, 2409.3, 2435.2, 2482.9, 2448.9, 2512.3, 2337.7, 2394.2,
2349.5, 2529.2, 2475.9, 2535.8, 2692.2, 2402.1, 2417.6, 2446.4,
2512.2, 2458.8, 2558.6, 2451.4, 2481.2, 2600, 2192, 2467.3, 2462.4,
2462.6, 2562.9, 2488.6, 2474.2, 2570.7, 2463.7, 2548.4, 2392.7,
2461.5, 2579.1, 2530, 2545.6, 2224.3, 2483.8, 2311.3, 2447.6)

Names<-structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 15L,
12L, 13L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L,
26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L,
39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L,
52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L,
65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L,
78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L,
91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L,
103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 115L, 112L, 111L,
113L, 114L, 116L, 118L, 117L, 119L, 120L, 121L, 122L, 123L, 124L,
125L, 126L, 127L, 128L, 129L, 130L, 131L, 132L, 133L, 134L, 135L,
136L, 137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L, 145L, 146L,
147L, 148L, 149L, 150L, 151L, 152L, 153L, 154L, 155L, 156L, 157L,
158L, 159L, 160L, 161L, 162L, 163L, 164L, 165L, 166L, 167L, 168L,
169L, 170L, 171L, 172L, 173L, 174L, 175L, 176L, 177L, 178L, 179L,
180L, 181L, 182L, 183L, 184L, 185L, 186L, 187L, 188L, 189L, 190L,
191L, 192L, 193L, 194L, 195L, 196L, 197L, 198L, 199L, 200L, 201L,
202L, 203L, 204L, 205L, 206L, 207L, 208L, 209L, 210L, 211L, 212L,
213L, 214L, 215L, 216L, 217L, 218L, 219L, 220L, 221L, 222L, 223L,
224L, 225L, 226L, 227L, 228L, 229L, 230L, 231L, 232L, 233L, 234L,
235L, 236L, 237L, 238L, 239L, 240L, 241L, 242L, 243L, 244L, 245L,
246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L, 254L, 255L, 256L,
257L, 258L, 259L, 260L, 261L, 262L, 263L, 264L, 265L, 266L, 267L,
268L, 269L, 270L, 271L, 272L, 273L, 274L, 275L, 276L, 277L, 278L,
279L, 280L, 281L, 282L, 283L, 284L, 285L, 286L, 287L, 288L, 289L,
290L, 291L, 292L, 293L, 294L, 295L, 296L, 297L, 298L, 299L, 300L,
301L, 302L, 303L, 304L, 305L, 306L, 307L, 308L, 309L, 310L, 311L,
312L, 313L, 314L, 315L, 316L, 317L, 318L, 319L, 320L, 321L, 322L,
323L, 324L, 325L, 326L, 327L, 328L, 329L, 330L, 331L, 332L, 333L,
336L, 334L, 335L, 337L, 338L, 339L, 340L, 341L, 342L, 343L, 344L,
345L, 346L, 347L, 348L, 349L, 350L, 351L, 352L, 353L, 354L, 355L,
356L, 357L, 358L, 359L, 360L, 361L, 362L, 363L, 364L, 365L, 366L,
367L, 368L, 369L, 370L, 371L, 372L, 373L, 374L, 375L, 376L, 377L,
378L, 379L, 380L, 381L, 382L, 383L, 384L, 385L, 386L, 387L, 388L,
389L, 390L, 391L, 392L, 393L, 395L, 396L, 397L, 398L, 399L, 400L,
401L, 402L, 403L, 404L, 405L, 406L, 407L, 408L, 409L, 410L, 411L,
412L, 413L, 414L, 415L, 416L, 417L, 418L, 419L, 420L, 421L, 422L,
423L, 424L, 425L, 426L, 427L, 428L, 429L, 430L, 431L, 432L, 433L,
434L, 435L, 436L, 437L, 438L, 439L, 440L, 441L, 442L, 443L, 444L,
446L, 447L, 448L, 449L, 450L, 451L, 452L, 453L, 454L, 455L, 456L,
457L, 458L, 459L, 460L, 461L, 462L, 463L, 464L, 465L, 466L, 467L,
468L, 470L, 472L, 473L, 474L, 475L, 476L, 478L, 477L, 479L, 480L,
481L, 482L, 483L, 484L, 485L, 486L, 487L, 488L, 489L, 490L, 491L,
492L, 493L, 494L, 495L, 496L, 497L, 498L, 499L, 500L, 501L, 502L,
503L, 504L, 505L, 506L, 507L, 508L, 509L, 510L, 511L, 512L, 513L,
514L, 515L, 516L, 517L, 518L, 519L, 520L, 521L, 522L, 523L, 524L,
525L, 526L, 527L, 528L, 529L, 530L, 531L, 532L, 533L, 534L, 535L,
536L, 537L, 538L, 539L, 540L, 541L, 542L, 543L, 544L, 545L, 546L,
547L, 548L, 549L, 550L, 551L, 552L, 553L, 554L, 555L, 556L, 557L,
558L, 559L, 560L, 561L, 562L, 563L, 564L, 565L, 566L, 567L, 568L,
569L, 570L, 571L, 572L, 573L, 574L, 575L, 576L, 577L, 578L, 579L,
580L, 581L, 582L, 583L, 584L, 585L, 586L, 587L, 589L, 588L, 590L,
591L, 592L, 593L, 594L, 595L, 596L, 597L, 598L, 599L, 600L, 601L,
602L, 603L, 604L, 605L, 606L, 607L, 608L, 609L, 610L, 611L, 612L,
613L, 614L, 615L, 616L, 617L, 618L, 619L, 620L, 621L, 622L, 623L,
624L, 625L, 626L, 627L, 628L, 629L, 630L, 631L, 632L, 633L, 634L,
635L, 636L, 637L, 638L, 639L, 640L, 642L, 643L, 644L, 645L, 646L,
647L, 648L, 649L, 650L, 651L, 652L, 653L, 654L, 947L, 948L, 950L,
951L, 952L, 953L, 954L, 955L, 956L, 957L, 958L, 959L, 960L, 961L,
963L, 964L, 965L, 968L, 969L, 970L, 975L, 976L, 977L, 979L, 983L,
779L, 939L, 789L, 693L, 971L, 861L, 720L, 767L, 908L, 887L, 862L,
659L, 695L, 849L, 677L, 719L, 822L, 727L, 853L, 726L, 893L, 729L,
813L, 655L, 702L, 739L, 715L, 946L, 829L, 854L, 926L, 679L, 930L,
745L, 919L, 792L, 942L, 694L, 847L, 818L, 863L, 973L, 817L, 810L,
943L, 759L, 888L, 974L, 672L, 812L, 879L, 859L, 904L, 949L, 894L,
721L, 775L, 782L, 768L, 924L, 714L, 873L, 967L, 966L, 931L, 891L,
788L, 840L, 757L, 889L, 731L, 657L, 911L, 663L, 747L, 915L, 878L,
688L, 671L, 858L, 716L, 756L, 838L, 856L, 857L, 944L, 807L, 669L,
706L, 897L, 773L, 699L, 717L, 844L, 683L, 722L, 881L, 811L, 852L,
743L, 877L, 796L, 724L, 700L, 737L, 766L, 781L, 872L, 772L, 835L,
754L, 933L, 733L, 876L, 842L, 890L, 864L, 691L, 920L, 824L, 902L,
830L, 749L, 678L, 866L, 685L, 761L, 744L, 670L, 697L, 819L, 658L,
712L, 901L, 689L, 834L, 723L, 836L, 709L, 895L, 839L, 708L, 734L,
882L, 687L, 740L, 825L, 827L, 673L, 703L, 676L, 760L, 735L, 764L,
675L, 899L, 913L, 787L, 736L, 698L, 786L, 680L, 725L, 916L, 780L,
886L, 696L, 674L, 937L, 795L, 820L, 921L, 922L, 741L, 799L, 845L,
668L, 774L, 738L, 905L, 896L, 656L, 814L, 797L, 690L, 776L, 907L,
682L, 843L, 980L, 823L, 869L, 742L, 982L, 686L, 778L, 785L, 664L,
661L, 832L, 794L, 746L, 855L, 910L, 898L, 748L, 918L, 684L, 718L,
711L, 798L, 793L, 936L, 883L, 934L, 884L, 763L, 850L, 867L, 662L,
871L, 892L, 803L, 932L, 801L, 809L, 704L, 667L, 900L, 860L, 848L,
805L, 802L, 808L, 851L, 728L, 753L, 713L, 837L, 752L, 707L, 701L,
833L, 804L, 751L, 816L, 770L, 914L, 705L, 925L, 758L, 874L, 941L,
710L, 870L, 875L, 841L, 791L, 962L, 928L, 681L, 828L, 821L, 929L,
769L, 826L, 730L, 762L, 865L, 868L, 755L, 777L, 800L, 692L, 912L,
903L, 989L, 984L, 992L, 993L, 998L, 997L, 991L, 1001L), .Label = c("AagaardJ(wh)",
"AbramovicB(wh)", "AcsP(wh)", "AdamsM(wh)", "AdiantoU(wh)", "AdorjanA(wh)",
"AfekY(wh)", "AgdesteinS(wh)", "AgrestE(wh)", "AkessonR(wh)",
"AkopianV(wh)", "AlburtL(wh)", "AleksandrovA(wh)", "AlmasiZ(wh)",
"AlModiahkiM(wh)", "AlonsoS(wh)", "AmonatovF(wh)", "AnandV(wh)",
"AnastasianA(wh)", "AnderssonU(wh)", "AnkaE(wh)", "AnsellS(wh)",
"AntoniewskiR(wh)", "AntonioR(wh)", "ApicellaM(wh)", "ArakhamiaK(wh)",
"ArencibiaW(wh)", "ArkellK(wh)", "ArlandiE(wh)", "AronianL(wh)",
"ArsovicG(wh)", "ArsovicZ(wh)", "AseevK(wh)", "AsrianK(wh)",
"AtalikS(wh)", "AvrukhB(wh)", "AzmaiparashviliZ(wh)", "BabulaV(wh)",
"BaburinA(wh)", "BacrotE(wh)", "BadeaB(wh)", "BagaturovG(wh)",
"BagirovV(wh)", "BaklanV(wh)", "BakreT(wh)", "BalashovY(wh)",
"BanikasH(wh)", "BareevE(wh)", "BarleJ(wh)", "BarlovD(wh)", "BarsovA(wh)",
"BaruaD(wh)", "BecerraRiveroJ(wh)", "BeliavskyA(wh)", "BellinR(wh)",
"BenderacA(wh)", "BenjaminJ(wh)", "BergE(wh)", "BergK(wh)", "BeshukovS(wh)",
"BhatV(wh)", "BischoffK(wh)", "BlatnyP(wh)", "BobrasP(wh)", "BoenschU(wh)",
"BojkovD(wh)", "BojkovicN(wh)", "BologanV(wh)", "BorgesMateosJ(wh)",
"BorgoG(wh)", "BosboomM(wh)", "BrinckClaussenB(wh)", "BrodskyM(wh)",
"BrombergerS(wh)", "BrowneW(wh)", "BrustkernJ(wh)", "BruzonL(wh)",
"BrynellS(wh)", "BurmakinV(wh)", "CabriloG(wh)", "CamporaD(wh)",
"CebaloM(wh)", "ChandlerM(wh)", "ChatalbashevB(wh)", "ChelushkinaI(wh)",
"CherninA(wh)", "ChernyshovK(wh)", "ChiburdanidzeM(wh)", "ChristiansenL(wh)",
"ChuchelovV(wh)", "CifuentesR(wh)", "CmilyteV(wh)", "ColovicA(wh)",
"ComasFabregoL(wh)", "ConquestS(wh)", "ContinD(wh)", "CramlingP(wh)",
"CrouchC(wh)", "CsomI(wh)", "CvitanO(wh)", "CyborowskiL(wh)",
"CzebeA(wh)", "DamasoR(wh)", "DamljanovicB(wh)", "DanielianE(wh)",
"DannerG(wh)", "DaoThienHai(wh)", "DautovR(wh)", "DaviesN(wh)",
"DeFirmianN(wh)", "DelchevA(wh)", "DelRioAngelisS(wh)", "DemboY(wh)",
"DeviatkinA(wh)", "DeVreugtD(wh)", "DgebuadzeA(wh)", "DizdarevicE(wh)",
"DizdarG(wh)", "DjurhuusR(wh)", "DjuricS(wh)", "DolmatovS(wh)",
"DominguezL(wh)", "DorfmanJ(wh)", "DraskoM(wh)", "DrazicS(wh)",
"DreevA(wh)", "DvoirysS(wh)", "DworakowskaJ(wh)", "DydyshkoV(wh)",
"DzhumaevM(wh)", "DzindzichashviliR(wh)", "DziubaM(wh)", "EfimovI(wh)",
"EhlvestJ(wh)", "EingornV(wh)", "EmmsJ(wh)", "EperjesiL(wh)",
"EpishinV(wh)", "ErmenkovE(wh)", "ErnstS(wh)", "ErnstT(wh)",
"EstradaNietoJ(wh)", "FakhiridouE(wh)", "FaragoI(wh)", "FaragoS(wh)",
"FedorchukS(wh)", "FedorowiczJ(wh)", "FercecN(wh)", "FierroBaqueroM(wh)",
"FinegoldB(wh)", "FlearG(wh)", "FogarasiT(wh)", "FoisorC(wh)",
"FoisorO(wh)", "FominyhA(wh)", "FontaineR(wh)", "FressinetL(wh)",
"FridmanD(wh)", "FtacnikL(wh)", "GalegoL(wh)", "GalkinA(wh)",
"GalliamovaA(wh)", "GalyasM(wh)", "GangulyS(wh)", "GaponenkoI(wh)",
"GaraA(wh)", "GarciaPalermoC(wh)", "GashimovV(wh)", "GauselE(wh)",
"GavrikovV(wh)", "GdanskiJ(wh)", "GelashviliT(wh)", "GelfandB(wh)",
"GellerE(wh)", "GeorgievK(wh)", "GheorghiuF(wh)", "GiffardN(wh)",
"GladyszevO(wh)", "GleizerovE(wh)", "GlekI(wh)", "GligoricS(wh)",
"GodenaM(wh)", "GoldinA(wh)", "GolodV(wh)", "GoloshchapovA(wh)",
"GolubevM(wh)", "GormallyD(wh)", "GrabarczykM(wh)", "GrandaZunigaJ(wh)",
"GreenfeldA(wh)", "GrischukA(wh)", "GrivasE(wh)", "GrootenH(wh)",
"GroszpeterA(wh)", "GulkoB(wh)", "GunnarssonJ(wh)", "GurevichD(wh)",
"GurevichM(wh)", "GustafssonJ(wh)", "GutmanL(wh)", "GyimesiZ(wh)",
"HabaP(wh)", "HamdouchiH(wh)", "HandkeF(wh)", "HanleyC(wh)",
"HansenC(wh)", "HarikrishnaP(wh)", "HartochR(wh)", "HasangatinR(wh)",
"HaslingerS(wh)", "HauchardA(wh)", "HebdenM(wh)", "HectorJ(wh)",
"HendriksW(wh)", "HeraI(wh)", "HertneckG(wh)", "HicklJ(wh)",
"HillarpPerssonT(wh)", "HjartarsonJ(wh)", "HodgsonJ(wh)", "HoelzlF(wh)",
"HoffmanA(wh)", "HoiC(wh)", "HortV(wh)", "HorvathJ(wh)", "HracekZ(wh)",
"HubnerR(wh)", "HulakK(wh)", "HuntA(wh)", "HuntH(wh)", "HuzmanA(wh)",
"IbragimovI(wh)", "IlincicZ(wh)", "IljushinA(wh)", "IllescasM(wh)",
"InarkievE(wh)", "InkiovV(wh)", "IonovS(wh)", "IordachescuV(wh)",
"IppolitoD(wh)", "IskusnyhS(wh)", "IstratescuA(wh)", "ItkisB(wh)",
"IuldachevS(wh)", "IvanchukV(wh)", "IvanisevicI(wh)", "IvanovicB(wh)",
"IvkovB(wh)", "JakabA(wh)", "JakobsenO(wh)", "JakovenkoD(wh)",
"JakubiecA(wh)", "JanevE(wh)", "JansaV(wh)", "JaraczP(wh)", "JenniF(wh)",
"JohansenD(wh)", "JonkmanH(wh)", "JurekJ(wh)", "KacheishviliG(wh)",
"KahnE(wh)", "KaidanovG(wh)", "KalodR(wh)", "KamskyG(wh)", "KarlssonL(wh)",
"KarpovA(wh)", "KasimdzhanovR(wh)", "KasparovG(wh)", "KasparovS(wh)",
"KavalekL(wh)", "KazhgaleyevM(wh)", "KempinskiR(wh)", "KengisE(wh)",
"KhalifmanA(wh)", "KharlovA(wh)", "KhenkinI(wh)", "KholmovR(wh)",
"KhurtsidzeN(wh)", "KindermannS(wh)", "KingD(wh)", "KlinovaM(wh)",
"KlovansJ(wh)", "KnottS(wh)", "KoganA(wh)", "KomarovD(wh)", "KomljenovicD(wh)",
"KonguvelP(wh)", "KonopkaM(wh)", "KorneevO(wh)", "KorotylevA(wh)",
"KortchnoiV(wh)", "KosicD(wh)", "KostenA(wh)", "KosteniukA(wh)",
"KotroniasV(wh)", "KotsurP(wh)", "KovacevicA(wh)", "KovacevicB(wh)",
"KovalevskayaE(wh)", "KozulZ(wh)", "KraaiJ(wh)", "KramnikV(wh)",
"KrasenkowM(wh)", "KrivecJ(wh)", "KrivosheyS(wh)", "KruppaY(wh)",
"KrushI(wh)", "KuczynskiR(wh)", "KudrinS(wh)", "KulaotsK(wh)",
"KunteA(wh)", "KupreichikV(wh)", "KurajicaB(wh)", "KuzminG(wh)",
"KveinysA(wh)", "LalicB(wh)", "LandaK(wh)", "LandenbergueC(wh)",
"LaneG(wh)", "LangewegK(wh)", "LankaZ(wh)", "LarsenB(wh)", "LastinA(wh)",
"LauR(wh)", "LautierJ(wh)", "LazarevV(wh)", "LeinA(wh)", "LeitaoR(wh)",
"LekoP(wh)", "LengyelB(wh)", "LernerK(wh)", "LevinF(wh)", "LevittJ(wh)",
"LigterinkG(wh)", "LikavskyT(wh)", "LiShilong(wh)", "LjubicicF(wh)",
"LjubojevicL(wh)", "LobronE(wh)", "LomineishviliM(wh)", "LoncarR(wh)",
"LopezMartinezJ(wh)", "LorscheidG(wh)", "LputianS(wh)", "LugovoiA(wh)",
"LukacsP(wh)", "LutherT(wh)", "LutzC(wh)", "LyellM(wh)", "MaciejaB(wh)",
"MadlI(wh)", "MagemBadalsJ(wh)", "MainkaR(wh)", "MakarovM(wh)",
"MakropoulouM(wh)", "MaksimovicS(wh)", "MalakhatkoV(wh)", "MalakhovV(wh)",
"MalaniukV(wh)", "MalisauskasV(wh)", "MancaF(wh)", "ManikM(wh)",
"MarinM(wh)", "MarkowskiT(wh)", "MarzoloC(wh)", "MastrovasilisA(wh)",
"MatamorosFrancoC(wh)", "MatnadzeA(wh)", "MatsuuraE(wh)", "MatveevaS(wh)",
"MazeS(wh)", "McDonaldN(wh)", "MchedlishviliM(wh)", "McNabC(wh)",
"McShaneL(wh)", "MedvegyZ(wh)", "MeijersV(wh)", "MestelJ(wh)",
"MestrovicZ(wh)", "MiezisN(wh)", "MikhalchishinA(wh)", "MikhalevskiV(wh)",
"MiladinovicI(wh)", "MilesA(wh)", "MilosG(wh)", "MilovV(wh)",
"MiroshnichenkoE(wh)", "MirzoevA(wh)", "MitkovN(wh)", "MitonK(wh)",
"MkrtchianL(wh)", "MohotaN(wh)", "MohrG(wh)", "MoiseenkoA(wh)",
"MorozevichA(wh)", "MortensenE(wh)", "MoskalenkoV(wh)", "MotwaniP(wh)",
"MotylevA(wh)", "MovsesianS(wh)", "MovsziszianK(wh)", "MrdjaM(wh)",
"MurshedN(wh)", "NaiditschA(wh)", "NajerE(wh)", "NanuC(wh)",
"NarcisoDublanM(wh)", "NatafIA(wh)", "NaumannA(wh)", "NaumkinI(wh)",
"NeelotpalD(wh)", "NeubauerM(wh)", "NevednichyV(wh)", "NeverovV(wh)",
"NguyenAnhDung(wh)", "NiHua(wh)", "NijboerF(wh)", "NikolaidisI(wh)",
"NikolicP(wh)", "NogueirasJ(wh)", "NovikovI(wh)", "NunnJ(wh)",
"OkhotnikV(wh)", "OlafssonH(wh)", "OlarasuG(wh)", "OllL(wh)",
"OralT(wh)", "OvetchkinR(wh)", "OvodE(wh)", "OvsejevitschS(wh)",
"PaehtzE(wh)", "PalacM(wh)", "PalliserR(wh)", "PannoO(wh)", "ParaguaM(wh)",
"PaschallW(wh)", "PaunovicD(wh)", "PavasovicD(wh)", "PelletierY(wh)",
"PengZhaoqin(wh)", "PeptanC(wh)", "PertN(wh)", "PetkovV(wh)",
"PetrenkoS(wh)", "PetrosianT(wh)", "PeturssonM(wh)", "PhilippeC(wh)",
"PigusovE(wh)", "PiketJ(wh)", "PikulaD(wh)", "PilgaardK(wh)",
"PinterJ(wh)", "PlachetkaJ(wh)", "PlaskettJ(wh)", "PogorelovR(wh)",
"PolakT(wh)", "PolgarJ(wh)", "PolgarZ(wh)", "PolugaevskyL(wh)",
"PonomariovR(wh)", "PopchevM(wh)", "PortischL(wh)", "PotkinV(wh)",
"PrasadD(wh)", "PrieE(wh)", "PsakhisL(wh)", "RadulovI(wh)", "RadulskiJ(wh)",
"RadziewiczI(wh)", "RaetskyA(wh)", "RajlichV(wh)", "RameshR(wh)",
"RashkovskyN(wh)", "RausisI(wh)", "RaviL(wh)", "RazuvaevY(wh)",
"ReefatS(wh)", "ReeH(wh)", "ReindermanD(wh)", "RelangeE(wh)",
"RenetO(wh)", "ResikaN(wh)", "RiazantsevA(wh)", "RibliZ(wh)",
"RicardiP(wh)", "RodriguezA(wh)", "RoederM(wh)", "RogersI(wh)",
"RogicD(wh)", "RogozenkoD(wh)", "RoguljB(wh)", "RomanishinO(wh)",
"RomeroHolmesA(wh)", "RotsteinA(wh)", "RowsonJ(wh)", "RoyChowdhuryS(wh)",
"RozentalisE(wh)", "RublevskyS(wh)", "RuckR(wh)", "RuddJ(wh)",
"RukavinaJ(wh)", "RustemovA(wh)", "RychagovA(wh)", "SadlerM(wh)",
"SadvakasovD(wh)", "SakaevK(wh)", "SalovV(wh)", "SandipanC(wh)",
"SargissianG(wh)", "SarkarJ(wh)", "SatyapragyanS(wh)", "SavchenkoS(wh)",
"SaxG(wh)", "SchandorffL(wh)", "ScheblerG(wh)", "SchlosserP(wh)",
"SchmidtW(wh)", "SchmittdielE(wh)", "SedinaE(wh)", "SeemanT(wh)",
"SeirawanY(wh)", "SeppO(wh)", "SeresL(wh)", "SermekD(wh)", "SerperG(wh)",
"ShabalovA(wh)", "ShaposhnikovE(wh)", "ShawJ(wh)", "ShchekachevA(wh)",
"ShengeliaD(wh)", "SherbakovR(wh)", "ShipovS(wh)", "ShiraziK(wh)",
"ShirovA(wh)", "ShortN(wh)", "ShulmanY(wh)", "SiebrechtS(wh)",
"SimacekP(wh)", "SkembrisS(wh)", "SkripchenkoA(wh)", "SkytteR(wh)",
"SlipakS(wh)", "SlobodjanR(wh)", "SmeetsJ(wh)", "SmejkalJ(wh)",
"SmirinI(wh)", "SmirnovP(wh)", "SmyslovV(wh)", "SockoB(wh)",
"SokolovA(wh)", "SokolovI(wh)", "SolakD(wh)", "SolovjovS(wh)",
"SolozhenkinE(wh)", "SoppeG(wh)", "SorokinM(wh)", "SosonkoG(wh)",
"SowrayP(wh)", "SpasovV(wh)", "SpasskyB(wh)", "SpeelmanJ(wh)",
"SpraggettK(wh)", "SriramJ(wh)", "StanecN(wh)", "StanglM(wh)",
"StanojoskiZ(wh)", "StarostitsI(wh)", "StefanovaA(wh)", "StefanssonH(wh)",
"StevicH(wh)", "StocekJ(wh)", "StohlI(wh)", "StrikovicA(wh)",
"StripunskyA(wh)", "SturuaZ(wh)", "SubaM(wh)", "SulavaN(wh)",
"SulskisS(wh)", "SummerscaleA(wh)", "SunyeNetoJ(wh)", "SutovskyE(wh)",
"SveshnikovE(wh)", "SvetushkinD(wh)", "SvidlerP(wh)", "SzalanczyE(wh)",
"SzeberenyiA(wh)", "TaimanovM(wh)", "TallaV(wh)", "TalM(wh)",
"TataiS(wh)", "TeranAlvarezI(wh)", "TeskeH(wh)", "ThipsayP(wh)",
"ThorfinnssonBr(wh)", "ThorhallssonT(wh)", "TimmanJ(wh)", "TimoshenkoG(wh)",
"TischbierekR(wh)", "TissirM(wh)", "TiviakovS(wh)", "TkachievV(wh)",
"TopalovV(wh)", "TorreE(wh)", "TosicM(wh)", "TratarM(wh)", "TregubovP(wh)",
"TseshkovskyV(wh)", "TukmakovV(wh)", "TunikG(wh)", "TurovM(wh)",
"UbilavaE(wh)", "UhlmannW(wh)", "UlibinM(wh)", "UrbanK(wh)",
"VadaszL(wh)", "VaganianR(wh)", "VaisserA(wh)", "VajdaL(wh)",
"VajdaS(wh)", "VallejoPonsF(wh)", "VanDelftM(wh)", "VandenDoelE(wh)",
"VanderSterrenP(wh)", "VanderStrichtG(wh)", "VanderWeideK(wh)",
"VanderWielJ(wh)", "VanRiemsdijkH(wh)", "VanWelyL(wh)", "VasilevichT(wh)",
"VaulinA(wh)", "VehiBachV(wh)", "VelickaP(wh)", "VelimirovicD(wh)",
"VeraR(wh)", "VescoviG(wh)", "VisserY(wh)", "VogtL(wh)", "VoiskaM(wh)",
"VokacM(wh)", "VokarevS(wh)", "VolkovS(wh)", "VoloshinL(wh)",
"VolzhinA(wh)", "VorobiovE(wh)", "VotavaJ(wh)", "VukicM(wh)",
"VysochinS(wh)", "WahlsM(wh)", "WardC(wh)", "WedbergT(wh)", "WellingG(wh)",
"WellsP(wh)", "WesterinenH(wh)", "WinantsL(wh)", "AbergelT(wh)",
"AdamsMi(wh)", "AdlyA(wh)", "AkobianV(wh)", "AkopianVl(wh)",
"AlekseevEvgeny(wh)", "AlSayedMohamad(wh)", "AndriasianZ(wh)",
"AreshchenkoA(wh)", "ArizmendiMartinezJ(wh)", "ArunPrasadS(wh)",
"ArutinianD(wh)", "AtakisiU(wh)", "AzarovS(wh)", "BaloghC(wh)",
"BanuszT(wh)", "BaramidzeD(wh)", "BartelMat(wh)", "BauerCh(wh)",
"BellonLopezJuM(wh)", "BelovVl(wh)", "BenjaminJoe(wh)", "BerczesD(wh)",
"BerescuA(wh)", "BerkesF(wh)", "BernasekJ(wh)", "BindrichF(wh)",
"BluvshteinM(wh)", "BocharovD(wh)", "BorisekJ(wh)", "BorosDe(wh)",
"BosiocicMari(wh)", "BrandenburgD(wh)", "BraunA(wh)", "BrkicA(wh)",
"BrunelloS(wh)", "BuhmannR(wh)", "BuiVinh(wh)", "BuXiangzhi(wh)",
"CanE(wh)", "CarlsenM(wh)", "CarlssonP(wh)", "CaruanaF(wh)",
"CernousekL(wh)", "CharbonneauP(wh)", "CheparinovI(wh)", "CioaraA(wh)",
"CornetteM(wh)", "CubasJo(wh)", "CvekR(wh)", "DanielsenHen(wh)",
"DavidAlb(wh)", "DeepanC(wh)", "DegraeveJM(wh)", "DjingarovaE(wh)",
"DjukicNi(wh)", "DoluhanovaE(wh)", "DoricD(wh)", "DrozdovskijY(wh)",
"DzagnidzeN(wh)", "EdouardR(wh)", "EfimenkoZ(wh)", "EljanovP(wh)",
"ErdogduM(wh)", "ErdosV(wh)", "ErenburgS(wh)", "EsenB(wh)", "FedorovAlex(wh)",
"FeletarD(wh)", "FelgaerR(wh)", "FellerS(wh)", "FernandezRomeroErn(wh)",
"FierA(wh)", "FirmanN(wh)", "FloresDi(wh)", "FlumbortA(wh)",
"FodorTjr(wh)", "FridmanD2(wh)", "FriedelJ(wh)", "FrolyanovD(wh)",
"GagunashviliM(wh)", "GajewskiG(wh)", "GallagherJo(wh)", "GasanovE(wh)",
"GellerJ(wh)", "GenovPeta(wh)", "GeorgievKi(wh)", "GeorgievVl(wh)",
"GhaemMaghamiE(wh)", "GondaL(wh)", "GopalG(wh)", "GrachevB(wh)",
"GrafA(wh)", "GrigoriantsSRUS(wh)", "GrunbergM(wh)", "GuliyevN(wh)",
"GuninaV(wh)", "GuptaAb(wh)", "GuseinovG(wh)", "HalkiasS(wh)",
"HammerJ(wh)", "HansenSuB(wh)", "HarikaD(wh)", "HaznedarogluK(wh)",
"HeberlaB(wh)", "HessRo(wh)", "HoangThanhTrang(wh)", "HorvathP1(wh)",
"HossainEnam(wh)", "HouskaJo(wh)", "HouYifan(wh)", "HowellD(wh)",
"HuangQian(wh)", "IkonnikovVy(wh)", "IllescasCordobaM(wh)", "IotovV(wh)",
"IturrizagaE(wh)", "IvanovMM(wh)", "IzoriaZ(wh)", "JakubowskiK(wh)",
"JankovicA(wh)", "JavakhishviliL(wh)", "JianuV(wh)", "JirkaJ(wh)",
"JobavaBa(wh)", "JohannessenLE(wh)", "JonesG(wh)", "JovanicO(wh)",
"JovanovicZoCRO(wh)", "JuWenjun(wh)", "KanepM(wh)", "KanovskyD(wh)",
"KantorikM(wh)", "KaravadeE(wh)", "KarjakinSergey(wh)", "KarpovAna(wh)",
"KarttunenM(wh)", "KhairullinI(wh)", "KhamrakulovI(wh)", "KhismatullinD(wh)",
"KjartanssonG(wh)", "KobaliaM(wh)", "KoneruH(wh)", "KononenkoT(wh)",
"KorchnoiV(wh)", "KorobovA(wh)", "KosintsevaN(wh)", "KosintsevaT(wh)",
"KotanjianT(wh)", "KovanovaB(wh)", "KovchanA(wh)", "KravtsivM(wh)",
"KritzL(wh)", "KryvoruchkoY(wh)", "KuninV(wh)", "KurnosovI(wh)",
"KuzubovY(wh)", "L'AmiE(wh)", "LafuenteP(wh)", "LahnoKateri(wh)",
"LalithB(wh)", "LaxmanR(wh)", "LaznickaV(wh)", "LenicL(wh)",
"LeonHoyosM(wh)", "LeQuangLiem(wh)", "LieK(wh)", "LimaDa(wh)",
"LintchevskiD(wh)", "LupulescuC(wh)", "LysyjI(wh)", "MakkaI(wh)",
"MaletinP(wh)", "MamedovRau(wh)", "MamedyarovS(wh)", "ManolacheM(wh)",
"MarecoS(wh)", "MarholevD(wh)", "MarkosJ(wh)", "MartinovicSa(wh)",
"MastrovasilisD(wh)", "MedicMir(wh)", "MegarantoS(wh)", "MeierGeo(wh)",
"MekhitarianK(wh)", "MeliaS(wh)", "MichielsB(wh)", "MilanovicDa(wh)",
"MillietS(wh)", "MinasianArt(wh)", "MistaA(wh)", "MoiseenkoA1(wh)",
"MoradiabadiE(wh)", "MoserE(wh)", "MurariuA(wh)", "MuzychukA(wh)",
"MuzychukM(wh)", "NakamuraH(wh)", "NavaraD(wh)", "NegiP(wh)",
"NepomniachtchiI(wh)", "NielsenPH(wh)", "NikolicPr(wh)", "NisipeanuLD(wh)",
"NybackT(wh)", "OleksienkoM(wh)", "OnischukAl(wh)", "PantsulaiaL(wh)",
"PapinV(wh)", "PapM(wh)", "PappGa(wh)", "ParligrasM(wh)", "PashikianA(wh)",
"PeraltaFe(wh)", "PerelshteynE(wh)", "PerunovicMil(wh)", "PetrosianTL(wh)",
"PiscopoP(wh)", "PogoninaN(wh)", "PokornaReg(wh)", "PolgarJu(wh)",
"PopovicDu(wh)", "PopovVal(wh)", "PostnyE(wh)", "PredojevicB(wh)",
"ProhaszkaP2(wh)", "RadjabovT(wh)", "RaggerM(wh)", "RahmanZia(wh)",
"RathnakaranK(wh)", "RodriguezAndURU(wh)", "RodshteinM(wh)",
"RoizM(wh)", "RomanovE(wh)", "RombaldoniD(wh)", "RusevK(wh)",
"SachdevT(wh)", "SafarliE(wh)", "SalgadoLopezI(wh)", "SanikidzeT(wh)",
"SaricA(wh)", "SaricIv(wh)", "SasikiranK(wh)", "SavchenkoB(wh)",
"SavicMiod1(wh)", "SebagM(wh)", "SebenikM(wh)", "SedlakN(wh)",
"SemcesenD(wh)", "SenguptaD(wh)", "SergeevVl(wh)", "SethuramanS(wh)",
"ShanavaK(wh)", "ShenYang(wh)", "ShishkinVaUKR(wh)", "ShomoevA(wh)",
"SimutoweA(wh)", "SjugirovS(wh)", "SmerdonD(wh)", "SockoM(wh)",
"SokolovAnd1(wh)", "SolodovnichenkoY(wh)", "SoW(wh)", "SrebrnicA(wh)",
"StellwagenD(wh)", "StojanovicDa(wh)", "SundararajanK(wh)", "SwathiG(wh)",
"SwierczD(wh)", "SwinkelsR(wh)", "ThorfinnssonBj(wh)", "TikkanenH(wh)",
"TimofeevArty(wh)", "TimoscenkoG(wh)", "TodorovicGM(wh)", "TomashevskyE(wh)",
"UsheninaA(wh)", "VachierLagraveM(wh)", "VargaZo(wh)", "VenkateshM(wh)",
"VidenovaI(wh)", "VijayalakshmiS(wh)", "VitiugovN(wh)", "VocaturoD(wh)",
"VolokitinAnd(wh)", "VovkY(wh)", "VuckovicBo(wh)", "WangHao(wh)",
"WangYue(wh)", "WerleJ(wh)", "WernerDi(wh)", "WirigA(wh)", "WittmannW(wh)",
"WohlA(wh)", "WojtaszekR(wh)", "WojtkiewiczA(wh)", "XieJun(wh)",
"XuJun(wh)", "YakovichY(wh)", "YandemirovV(wh)", "YemelinV(wh)",
"YermolinskyA(wh)", "YevseevD(wh)", "YudasinL(wh)", "YusupovA(wh)",
"ZagrebelnyS(wh)", "ZajaI(wh)", "ZambranaO(wh)", "ZapataA(wh)",
"ZarnickiP(wh)", "ZatonskihA(wh)", "ZawadzkaJ(wh)", "ZdebskajaN(wh)",
"ZelcicR(wh)", "ZhangPengxiang(wh)", "ZhangZhong(wh)", "ZhaoXue(wh)",
"ZherebukhY(wh)", "ZhigalkoA(wh)", "ZhigalkoS(wh)", "ZhuChen(wh)",
"ZhukovaN(wh)", "ZimmermanY(wh)", "ZinchenkoYa(wh)", "ZontakhA(wh)",
"ZozuliaA(wh)", "ZubarevAl1(wh)", "ZuficM(wh)", "ZvjaginsevV(wh)",
"AndreikinD(wh)", "BodnarukA(wh)", "GiriA(wh)", "GiryaO(wh)",
"GrandeliusN(wh)", "KuljasevicD(wh)", "LiChao2(wh)", "MamedjarovaZ(wh)",
"MatlakovM(wh)", "MelkumyanH(wh)", "RajlichI(wh)", "RobsonR(wh)",
"ShimanovA(wh)", "SzaboGROM(wh)", "YildizB(wh)", "ZhouJianchao(wh)",
"AlmeidaQuintanaO(wh)", "AntalGe(wh)", "AshwinJ(wh)", "DominguezPerezL(wh)",
"RodriguezVilaA(wh)", "VolkovSergey1(wh)", "KislikE(wh)", "KallioH(wh)",
"PiketJe(wh)", "NakamuraHi(wh)"), class = "factor")


########################## Look at post means
Psi<-extract(fitChess,pars="Psi")$Psi
MuTheta <- array(NA, dim=c(P,M,7))
## Reshape Psi
 for ( p in 1:P){
 for ( m in 0:(M-1)){
 for ( d in 1:7){
      MuTheta[p,(m+1),d] <- mean(Psi[,p,(m*7)+d]);
                }}}

 plot(MuTheta[,2,1]~Elo)
 
 
####################### Tertiary plot
 library(vcd)
 library(plotrix)
 library(RColorBrewer)
 library(ggtern)

 add.alpha <- function(col, alpha=1){
if(missing(col))
stop("Please provide a vector of colours.")
apply(sapply(col, col2rgb)/255, 2,
function(x)
rgb(x[1], x[2], x[3], alpha=alpha))
}
 Samps<-2000
IndTheta <- array(NA, dim=c(Samps,P,M,7))

## Reshape Psi
 for ( p in 1:P){
 for ( m in 0:(M-1)){
 for ( d in 1:7){
      IndTheta[,p,(m+1),d] <- Psi[,p,(m*7)+d];
                }}}


# Set move, and then bind parameters
   MoveM <-1
   FreqDat  <-    cbind( as.vector(IndTheta[1:100,,MoveM,2]),as.vector(IndTheta[1:100,,MoveM,6]) ,as.vector(IndTheta[1:100,,MoveM,4]))

suppressMessages(library(ggtern))
set.seed(1)
plot <- ggtern(data = data.frame(Ipr = FreqDat[,1],
Ppr = FreqDat[,2],
Spr = FreqDat[,3]),
aes(x=Ipr, y=Ppr, z=Spr))
plot + geom_density_tern(n = 200,
aes(fill  = ..level..,
alpha = ..level..)) +
theme_rgbw() +
labs(title = "Example Density/Contour Plot")    +
scale_fill_gradient(low = "blue",high = "red")  +
guides(color = "none", fill = "none", alpha = "none")



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
