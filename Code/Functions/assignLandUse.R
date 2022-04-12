#function to assign land use. Takes as arguments the DF that needs land use assigned (DF1)
#and a DF with land use codes with columns Value, LandcoverNA, NLCD_US and LandcoverUS (DF2)
#DF1 requires a column called Value

assignLandUse<-function(DF1, DF2){
     #assign new empty columns
     DF1$LandcoverNA<-"fill"
     DF1$NLCD_US<-100
     DF1$LandcoverUS<-"fill"
     runs<-length(unique(DF1$Value))
     for (i in 1:runs){
          fix<-which(DF1$Value == DF2$Value[i])
          DF1$LandcoverNA[fix]<-DF2$LandcoverNA[i]
          DF1$NLCD_US[fix]<-DF2$NLCD_US[i]
          DF1$LandcoverUS[fix]<-DF2$LandcoverUS[i]
     }
     return(DF1)
}