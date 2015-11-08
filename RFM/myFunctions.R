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

myBarplot <- function(inputData, chartName ="My Boxplot", xLbl = "Customer Segments", yLbl = "Average revenue"){
  barplot(inputData$x, names.arg = inputData$Group.1, legend.text = round(inputData$x, 2), xlab = xLbl, ylab = yLbl, col = c("darkgreen","green","orange","yellow","darkblue","blue","red"), main = chartName)
}

#thorsten: my rfm segmentation function
rfmSegmentation <- function(dataInput, orderValueThreshold = 100, endDate = 0) {
  
  if(endDate == 0) {
    # Compute recency, frequency, and average purchase amount per customer 
    print(endDate)
    dataOutput = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM dataInput GROUP BY 1")
  } else if(endDate==365) {
    print(endDate)
    # must be solved v <- endDate
    dataOutput = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM dataInput 
                       WHERE days_since > 360
                       GROUP BY 1")
    
  } else if(endDate==730){
    print(endDate)
    # must be solved v <- endDate
    dataOutput = sqldf("SELECT customer_id,
                       MIN(days_since) - 730 AS 'recency',
                       MAX(days_since) - 730 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM dataInput 
                       WHERE days_since > 730
                       GROUP BY 1")
    
  } else if(endDate==1095){
    print(endDate)
    # must be solved v <- endDate
    dataOutput = sqldf("SELECT customer_id,
                       MIN(days_since) - 1095 AS 'recency',
                       MAX(days_since) - 1095 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM dataInput 
                       WHERE days_since > 1095
                       GROUP BY 1")
  }
  
  dataOutput$segment = "long time inactive"
  dataOutput$segment[which(dataOutput$recency > 730)] = "inactive"
  dataOutput$segment[which(dataOutput$recency <= 730 & dataOutput$recency > 365)] = "cold"
  dataOutput$segment[which(dataOutput$recency <= 365 & dataOutput$recency > 182.5)] = "warm"
  dataOutput$segment[which(dataOutput$recency <= 182.5)] = "active"
  dataOutput$segment[which(dataOutput$segment == "warm" & dataOutput$first_purchase <= 365)] = "new warm"
  dataOutput$segment[which(dataOutput$segment == "warm" & dataOutput$amount < orderValueThreshold)] = "warm low value"
  dataOutput$segment[which(dataOutput$segment == "warm" & dataOutput$amount >= orderValueThreshold)] = "warm high value"
  dataOutput$segment[which(dataOutput$segment == "active" & dataOutput$first_purchase <= 182.5)] = "new active"
  dataOutput$segment[which(dataOutput$segment == "active" & dataOutput$amount < orderValueThreshold)] = "active low value"
  dataOutput$segment[which(dataOutput$segment == "active" & dataOutput$amount >= orderValueThreshold)] = "active high value"
  dataOutput$segment = factor(x = dataOutput$segment, levels = c("inactive", "cold", "warm high value", "warm low value", "new warm", "active high value", "active low value", "new active"))
  
  # return data
  dataOutput
  
}
# --- COMPUTE TRANSITION MATRIX ------------------------------------------------------------------------------------

transitionMatrix <- function(dataPrevYear, dataActualYear){
  
  # Compute transition matrix
  new_data = merge(x = dataPrevYear, y = dataActualYear, by = "customer_id", all.x = TRUE)
  head(new_data)
  transition = table(new_data$segment.x, new_data$segment.y)
  print(transition)
  
  # Divide each row by its sum
  transition = transition / rowSums(transition)
  print(transition) 
  
  # Return transition probabilities per customer segement
  transition
}
