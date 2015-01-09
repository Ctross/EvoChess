
#############################################################################################
# Load Data
# Importing Data in Modern format
#
#  I started using the MillionBase 2.2 games. http://www.top-5000.nl/pgn.htm
#  I then split the resulting text file into files that could be read into R in manageble peices, 
#  using cygwin's split command, leaving 34 files to read, clean, and compress into a workable R object. 
#  The script below can be used in replication of the cleaning process.
#

#############################################################################################
# Load Data
# Importing Data in Modern format. This should read any PGN game, but make sure there are no spaces between periods.
# ie. The first move should read as: 1.e4 e5, not 1. e4 e5

###########################################################
# Begin R Code:
###########################################################

temp_read <- as.vector(read.table(file.choose(), sep = '\n', stringsAsFactors = FALSE))

to_exclude <- c('[Site kaggle.com]', '[Date ??]', '[White ??]', '[Black ??]', '[Round ??]')

for(removed in to_exclude)
{temp_read <- temp_read[temp_read != removed]}

temp_read1<-temp_read

Moves <- vector(mode = "character", length = 2*length(temp_read1))
count = 1
for (i in c(1:length(temp_read1)))
{
  if (grepl("Event",temp_read1[i]))
  {Moves[count] = paste(temp_read1[i])
   count= count+1}
  else
  {Moves[count] = paste(Moves[count],temp_read1[i])}
  if (grepl("Event",temp_read1[i+1]))
  {count = count+1}
}

##################### Awkward part
# Find how many moves are in list by hand for now: replace 38014 below by hand searching Moves for the last filled cell,
# And then cut off extra space
Moves<-Moves[1:38014]

# Transform vector to matrix
train_raw <- matrix(Moves, ncol = 2, byrow = TRUE)

##########################################################################
#### Clean Data
#### Set N, could be done by hand
 N<-dim(train_raw)[1]
 
## Get Players Elo Scores
   EloW<-EloB<-c()
 for(i in 1:N){
EloW[i] <-   as.numeric(as.character(sub(".*?[[]WhiteElo (.*?)[]].*", "\\1", train_raw[i,2])))
EloB[i] <-   as.numeric(as.character(sub(".*?[[]BlackElo (.*?)[]].*", "\\1", train_raw[i,2])))
}



####################### Get Dates of Games
Date<-c()
 for(i in 1:N)
 Date[i]<-as.numeric((substr( sub(".*?Date (.*?). .*", "\\1", train_raw[i,2]),1,4)))
for(i in 1:N)
Date[i]<-as.numeric(as.character(Date[i]))

###################### Get Win/Draw/Loss Informations
Win<-c()
 NameBlack<-c()
  NameWhite<-c()
 for(i in 1:N){
if((( sub(".*?Result (.*?). .*", "\\1", train_raw[i,2])))=="1-0"){
  Win[i]<-1} else{
  if((( sub(".*?Result (.*?). .*", "\\1", train_raw[i,2])))=="1/2-1/2"){
  Win[i]<-2} else{
    if((( sub(".*?Result (.*?). .*", "\\1", train_raw[i,2])))=="0-1"){
  Win[i]<-3} else{
  Win[i]<-NA
  }}}

 NameBlack[i]<- sub("\\.", "",sub("\\,", "",gsub("\\s","",sub(".*?[[]Black (.*?)[]].*", "\\1", train_raw[i,2]))))
  NameWhite[i]<- sub("\\.", "",sub("\\,", "",gsub("\\s","",sub(".*?[[]White (.*?)[]].*", "\\1", train_raw[i,2]))))

  }

########################################## Get Matrix of First 5 moves
M<-matrix(NA, ncol=10,nrow=length(Win))
 for(i in 1:N){
M[i,1]<-gsub( " .*$", "", sub(".*?1[.] (.*?) 2[.].*", "\\1", train_raw[i,2]))
M[i,2]<-gsub(".+\\s+", "", sub(".*?1[.] (.*?) 2[.].*", "\\1", train_raw[i,2]))

M[i,3]<-gsub( " .*$", "", sub(".*?2[.] (.*?) 3[.].*", "\\1", train_raw[i,2]))
M[i,4]<-gsub(".+\\s+", "", sub(".*?2[.] (.*?) 3[.].*", "\\1", train_raw[i,2]))

M[i,5]<-gsub( " .*$", "", sub(".*?3[.] (.*?) 4[.].*", "\\1", train_raw[i,2]))
M[i,6]<-gsub(".+\\s+", "", sub(".*?3[.] (.*?) 4[.].*", "\\1", train_raw[i,2]))

M[i,7]<-gsub( " .*$", "", sub(".*?4[.] (.*?) 5[.].*", "\\1", train_raw[i,2]))
M[i,8]<-gsub(".+\\s+", "", sub(".*?4[.] (.*?) 5[.].*", "\\1", train_raw[i,2]))

M[i,9]<-gsub( " .*$", "", sub(".*?5[.] (.*?) 6[.].*", "\\1", train_raw[i,2]))
M[i,10]<-gsub(".+\\s+", "", sub(".*?5[.] (.*?) 6[.].*", "\\1", train_raw[i,2]))
              }
              
Z<-data.frame(Date,EloW,EloB,NameWhite,NameBlack,Win, M)
Z2<-Z[complete.cases(Z),]
Z2[1:100,]

setwd("C:/Users/Empyrean/Desktop/Dropbox/Open Papers/The Evolution of Chess Strategy/CleanedData")
write.csv(Z2,"Part1.csv")


#####################################################################################
### Clean second round
setwd("C:/Users/Empyrean/Desktop/Dropbox/Open Papers/The Evolution of Chess Strategy/CleanedData")
R1<-read.csv( "Part1.csv")
R2<-read.csv( "Part2.csv")
R3<-read.csv( "Part3.csv")
R4<-read.csv( "Part4.csv")
R5<-read.csv( "Part5.csv")
R6<-read.csv( "Part6.csv")
R7<-read.csv( "Part7.csv")
R8<-read.csv( "Part8.csv")
R9<-read.csv( "Part9.csv")
R10<-read.csv( "Part10.csv")
R11<-read.csv( "Part11.csv")
R12<-read.csv( "Part12.csv")
R13<-read.csv( "Part13.csv")
R14<-read.csv( "Part14.csv")
R15<-read.csv( "Part15.csv")
R16<-read.csv( "Part16.csv")
R17<-read.csv( "Part17.csv")
R18<-read.csv( "Part18.csv")
R19<-read.csv( "Part19.csv")
R20<-read.csv( "Part20.csv")
R21<-read.csv( "Part21.csv")
R22<-read.csv( "Part22.csv")
R23<-read.csv( "Part23.csv")
R24<-read.csv( "Part24.csv")
R25<-read.csv( "Part25.csv")
R26<-read.csv( "Part26.csv")
R27<-read.csv( "Part27.csv")
R28<-read.csv( "Part28.csv")
R29<-read.csv( "Part29.csv")
R30<-read.csv( "Part30.csv")
R31<-read.csv( "Part31.csv")
R32<-read.csv( "Part32.csv")
R33<-read.csv( "Part33.csv")
R34<-read.csv( "Part34.csv")

R<-rbind(R1                              ,
         R2                              ,
         R3                              ,
         R4                             ,
         R5                             ,
         R6                           ,
         R7                           ,
         R8                          ,
         R9                         ,
         R10                        ,
         R11                       ,
         R12                      ,
         R13                     ,
         R14                    ,
         R15                   ,
         R16                  ,
         R17                 ,
         R18                ,
         R19               ,
         R20              ,
         R21             ,
         R22            ,
         R23           ,
         R24          ,
         R25         ,
         R26        ,
         R27       ,
         R28      ,
         R29     ,
         R30    ,
         R31   ,
         R32  ,
         R33 ,
         R34)
         
         rm(R1                              ,
         R2                              ,
         R3                              ,
         R4                             ,
         R5                             ,
         R6                           ,
         R7                           ,
         R8                          ,
         R9                         ,
         R10                        ,
         R11                       ,
         R12                      ,
         R13                     ,
         R14                    ,
         R15                   ,
         R16                  ,
         R17                 ,
         R18                ,
         R19               ,
         R20              ,
         R21             ,
         R22            ,
         R23           ,
         R24          ,
         R25         ,
         R26        ,
         R27       ,
         R28      ,
         R29     ,
         R30    ,
         R31   ,
         R32  ,
         R33 ,
         R34)
         
         
         
############################ Drop Broken Records
Drop<- ifelse( R$X1=="" | R$X1==" " |R$X1=="..." | R$X1=="Bh3" | R$X1=="Bh3" | R$X1=="f5" | R$X1=="Nf5" ,1,0)
R<-R[which(Drop==0),]

Drop<- ifelse( R$Date==1969 ,1,0)
R<-R[which(Drop==0),]

Drop<- ifelse( R$Date==1970 ,1,0)
R<-R[which(Drop==0),]

R$Date<-factor(R$Date)

Drop<- ifelse( R$X2=="" | R$X1==" " |R$X1=="0-1" | R$X1=="1-0" | R$X1=="1/2-1/2" | R$X1=="Bf8" | R$X1=="Qf5" ,1,0)
R<-R[which(Drop==0),]


R$X2<-factor(R$X2)
R$X1<-factor(R$X1)
