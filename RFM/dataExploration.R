# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    DATA EXPLORATION
# __________________________________________________________
# //////////////////////////////////////////////////////////

# Explore customer data with negative amounts
negative_data$V3 = as.Date(negative_data$V3, "%Y-%m-%d")
hist(negative_data$V2)
summary(negative_data$V2)
plot(negative_data$V2, negative_data$V3)

# Cluster Analysis
cluster_data = negative_data
cluster_data$V3 = as.numeric(difftime(time1 = "2014-01-01",time2 = cluster_data$V3, units = "days"))
clusters = kmeans(cluster_data, 5, 10)
table(clusters$size)
hist(clusters$size)
plot(clusters$cluster, clusters$fitted.values)

#thorsten: my pie chart function
myPieChart <- function(dataInput, chartName){
  # Plot a pie chart
  mytable <- table(dataInput$segment)
  slices <- c(mytable)
  pct <- round(slices/sum(slices)*100, digits = 2)
  lbls <- paste(names(mytable), "\n", mytable, sep="")
  lbls <- paste(lbls, ",", pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(mytable, labels = lbls, col = rainbow(length(lbls)), main = chartName)
}
