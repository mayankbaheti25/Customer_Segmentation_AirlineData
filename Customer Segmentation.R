
rm(list = ls(all=T))

library(plotly)
library(data.table)
#reading training dataset
path = "C:/Users/HP/Downloads"
setwd(path)

rm(path)

data = fread(input = "cohort_data.csv")
data = as.data.frame(data)
data$booking_date=as.Date(data$booking_date)

#Standardize the Data
normalize <- function(newdataf){
  normalizeddataf <- newdataf 
  for (n in names(newdataf)){
    normalizeddataf[,n] <-  
      (newdataf[,n] - min(newdataf[,n])) /  (max(newdataf[,n]) -  min(newdataf[,n]))
  } 
  return(normalizeddataf)
}


##Customer RFM Analysis

startDate <- as.Date("20140101","%Y%m%d")
endDate <- Sys.Date()

#Function for RFM analysis
RFM_Fun = function(variables) {
  #Recency Calculation
  df1 <- data[order(data$booking_date,decreasing = TRUE),]
  df2 <- df1[df1$booking_date >= startDate & df1$booking_date <= endDate,]
  df3 <- df2[!duplicated(df2[[variables]]),]
  Recency <- as.numeric(difftime(endDate,df3$booking_date,units="days"))
  df3$Recency <- NULL
  df3$Recency <- Recency
  df3 <- df3[order(df3[[variables]]), ]
  
  
  #Frequency Calculation
  df2$ID <- as.factor(df2[[variables]]) 
  fre <- as.data.frame(table(df2[[variables]]))
  df3$Frequency <- NULL
  df3$Frequency <- fre[,2]
  
  
  #Monetary Calculation
  M_df <- aggregate(df2$booking_value, list(df2[[variables]]), sum)
  df3$Monetary <- NULL
  df3$Monetary <- M_df$x
  return(df3)
}
RFM_DF_customer= RFM_Fun("customer_id")[,c("customer_id", "Recency","Frequency","Monetary")]

#25 records removed as monetaryy values were NA
RFM_DF_customer = RFM_DF_customer[complete.cases(RFM_DF_customer[ , 4]),]

normalized_data_customer <- normalize(RFM_DF_customer[,-1]) 

#Clustering for customer

# Identifying right number of clusters
tot.wss <- 0
set.seed(123)
for (i in 1:15) {
  tot.wss[i] <- kmeans(normalized_data_customer,centers=i)$tot.withinss
}

plot(1:15, tot.wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Total within groups sum of squares") 
rm(i, tot.wss)

clus_customer <- kmeans(normalized_data_customer, 5) # 4 cluster solution

# append cluster numbers
clustered_customer <- data.frame(RFM_DF_customer, "Customer_Cluster" = clus_customer$cluster) 

clustered_customer$Customer_Cluster=as.factor(clustered_customer$Customer_Cluster)
rownames(clustered_customer)=clustered_customer[,1]

plot_ly(clustered_customer[,-1], x = ~Recency, y = ~Frequency, z = ~Monetary, color = ~Customer_Cluster) %>%
  add_markers() %>%
layout(title = 'Clustered Customer',scene = list(xaxis = list(title = 'Recency'),
                          yaxis = list(title = 'Frequency '),
                          zaxis = list(title = 'Monetary')))
 