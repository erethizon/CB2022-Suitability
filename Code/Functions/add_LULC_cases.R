#function add_LULC_cases. This function goes through a dataframe and, for each homerange,
#adds rows for any Landcover classes that are missing and assigns a value of 0 for the area.


add_LULC_cases<-function(df){
     #determine number of objects to step through
     HRs<-unique(df$OBJECTID_1)
     num_HRs<-length(unique(df$OBJECTID_1))
     all_vals<-unique(df$Value)
     for (i in 1:num_HRs){
          myGroup<-filter(df, OBJECTID_1 == HRs[i])
          num_missing<-length(all_vals)-length(myGroup$OBJECTID_1)#determines #lulc classes missing from that object id
          if(num_missing > 0){
               #now find out which ones are missing using setdiff()
               missing_values<-setdiff(all_vals, myGroup$Value)
               #now add those classes back 
               newdf<-data.frame(matrix(nrow = num_missing, ncol = ncol(df)))
               colnames(newdf)<-colnames(df)
               newdf$OBJECTID_1<-i
               newdf$Value<-missing_values
               newdf$Count<-0
               newdf$Area<-0
               df<-rbind(df, newdf)
          }
     }
     return(df)
}