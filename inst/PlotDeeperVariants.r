
################################################## Paste Moves Together to Get Deep Variants
 Opening<-c()
 for(i in 1:length(R$X1)){
  Opening[i]<-paste0(R$X1[i],R$X2[i],R$X3[i],R$X4[i],R$X5[i],R$X6[i],R$X7[i],R$X8[i],R$X9[i],R$X10[i])
  }
  
  R$Opening<-Opening
  
  
  ###################### Get Deep Variants, Two Examples
                
########################################### Sicilian Defense
 R4<-R[    which( R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4e5Nb5a6" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4e5Nb5d6" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4e6Nb5d6" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4e6Nc3a6" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4e6Nc3d6" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4e6Nc3Qc7" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4g6Be2Bg7" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4g6Be3Bg7" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4g6c4Bg7"  |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4g6c4Nf6"  |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4g6Nc3Bg7" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Nf6Nc3d6" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Nf6Nc3e5"  |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Nf6Nc3e6"  |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Nf6Nc3g6"  |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Qb6Nb3Nf6"  |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Qc7Nb5Qb8" |
 R$Opening=="e4c5Nf3Nc6d4cxd4Nxd4Qc7Nc3e6" ),]

########################################## Kings Indian
 R4<-R[    which( R$Opening=="d4Nf6c4g6Nc3Bg7e4d6Bd3O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6Be2O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6Bg5O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6f3c6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6f3O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6f4c5" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6f4O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6h3O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6Nf3O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4d6Nge2O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4O-OBe2d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4O-OBe3d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4O-Of3d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4O-ONf3c6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7e4O-ONf3d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7g3O-OBg2d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7Nf3d6g3O-O" |
 R$Opening=="d4Nf6c4g6Nc3Bg7Nf3O-OBf4d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7Nf3O-OBg5c5" |
 R$Opening=="d4Nf6c4g6Nc3Bg7Nf3O-OBg5d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7Nf3O-Oe4d6" |
 R$Opening=="d4Nf6c4g6Nc3Bg7Nf3O-Og3d6" ),]
 
 ###############################################################
 ######################################### Plot Sicilian Defense
   R5<-R4#[which(R4$EloW>2500),] # If desired, drop weaker players, or stronger players

  G<-as.matrix(table(R5$Opening,R5$Date))
   G2<-G
   for(i in 1:dim(G)[2])
   G2[,i]<-G[,i]/sum(G[,i])

   mG<-c()
   for(i in 1:22){
   mG[i]<-mean(G2[i,])
   }

   G3<-G2[order(mG, decreasing=T),]

   G4<-G3

   for(y in 1:43){
   G4[,y]<-rev(cumsum(rev(G3[,y])))
   }

    plot(G4[2,]~c(1971:2013),type="l", ylim=c(0,1),xlim=c(1975,2013),xlab="Date", ylab="Frequency of Opening Move",xaxt="n")
    axis(1, at=1975:2013)
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[1,]) , rev((G4[2,])))) , col = '#a6cee3'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[2,]) , rev((G4[3,])))) , col = '#1f78b4'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[3,]) , rev((G4[4,])))) , col = '#b2df8a'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[4,]) , rev((G4[5,])))) , col = '#33a02c'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[5,]) , rev((G4[6,])))) , col = '#fb9a99'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[6,]) , rev((G4[7,])))) , col = '#e31a1c'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[7,]) , rev((G4[8,])))) , col = '#fdbf6f'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[8,]) , rev((G4[9,])))) , col = '#ff7f00'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[9,]) , rev((G4[10,])))) , col = '#cab2d6'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[10,]) , rev((G4[11,])))) , col = '#6a3d9a'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[11,]) , rev((G4[12,])))) , col = '#ffff99'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[12,]) , rev((G4[13,])))) , col = '#b15928'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[13,]) , rev((G4[14,])))) , col = '#1b9e77'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[14,]) , rev((G4[15,])))) , col = '#d95f02'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[15,]) , rev((G4[16,])))) , col = '#7570b3'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[16,]) , rev((G4[17,])))) , col = 'indianred'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c((G4[17,]) , rev((G4[18,])))) , col = 'slateblue'  , border = "black")
   polygon(c(c(1971:2013), rev(c(1971:2013))), c(c(((G4[18,])) , rev(rep(0,length(G4[8,]))))) , col = '#e7298a'  , border = "black")
   

# All variants begin with the moves:
# e4c5Nf3Nc6d4cxd4Nxd  

#And continuw with:
text(2006,.92,"Nf6Nc3d6" )
text(2006,.65,"Nf6Nc3e5" )
text(2006,.405,"g6Nc3Bg7" )
text(2006,.32,"e5Nb5d6"  ) 
text(1987.5,.32,"g6c4Bg7"  )
text(2006,,"Qc7Nc3e6" )
 
