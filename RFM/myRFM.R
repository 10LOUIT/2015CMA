# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 2 - MANAGERIAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////

rm(list=ls(all=TRUE))
setwd("~/Documents/github/2015CMA/RFM")

# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------

# Load necessary packages
library(RSQLite)
library(DBI)
library(gsubfn)
library(proto)
library(tcltk)

# Compute key marketing indicators using SQL language
library(sqldf)

# Load text file into local variable called 'data'
setwd("~/OneDrive/B_Data/Berner_PT")
raw_data = read.delim(file = 'Customer_purchases_2011_2013_with_empty_amounts.csv', header = FALSE, sep = ',', dec = '.')
#summary(raw_data)

# Manipulate data of old customers without purchases in this file
new_data = sqldf("SELECT * FROM raw_data WHERE V3 > 0")
old_data = sqldf("SELECT * FROM raw_data WHERE V3 = 0")
negative_data = sqldf("SELECT * FROM raw_data WHERE V2 < 0")
old_data$V3 = "2010-01-01" 
old_data$V2 = "0.00"
data = merge(new_data, old_data, all.x = TRUE, all.y = TRUE)
data = sqldf("SELECT * FROM raw_data WHERE V2 > -0.01")
#data = sqldf("SELECT * FROM raw_data WHERE V2 > -0.01 AND V3 NOT LIKE 0")

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

# Filter customers purchases smaller than a distinct threshold 
#data = sqldf("SELECT * FROM customers_with_purchases WHERE V2 > 10")
#cleand_data = na.omit(raw_data)

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2014-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

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
                      WHERE days_since > 365
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
 }
  
  dataOutput$segment = "long time inactive"
  dataOutput$segment[which(dataOutput$recency > 365*3)] = "inactive"
  dataOutput$segment[which(dataOutput$recency <= 365*3 & dataOutput$recency > 365*2)] = "cold"
  dataOutput$segment[which(dataOutput$recency <= 365*2 & dataOutput$recency > 365*1)] = "warm"
  dataOutput$segment[which(dataOutput$recency <= 365)] = "active"
  dataOutput$segment[which(dataOutput$segment == "warm" & dataOutput$first_purchase <= 365*2)] = "new warm"
  dataOutput$segment[which(dataOutput$segment == "warm" & dataOutput$amount < orderValueThreshold)] = "warm low value"
  dataOutput$segment[which(dataOutput$segment == "warm" & dataOutput$amount >= orderValueThreshold)] = "warm high value"
  dataOutput$segment[which(dataOutput$segment == "active" & dataOutput$first_purchase <= 365)] = "new active"
  dataOutput$segment[which(dataOutput$segment == "active" & dataOutput$amount < orderValueThreshold)] = "active low value"
  dataOutput$segment[which(dataOutput$segment == "active" & dataOutput$amount >= orderValueThreshold)] = "active high value"
 
  
  dataOutput
  
}

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

# Call the rfm segmentation function and return the processed df
# All Customers
customers <- rfmSegmentation(data, 150)
# Customers with purchase in 2013
customers_2013 <- rfmSegmentation(data, 150, 0)
# Customers with purchase in 2012
customers_2012 <- rfmSegmentation(data, 150, 365)
# Customers with purchase in 2011
customers_2011 <- rfmSegmentation(data, 150, 730)
# Explore the processed data
table(customers$segment)
table(customers_2013$segment)
table(customers_2012$segment)
table(customers_2011$segment)

# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new2_data = sqldf("SELECT * FROM customers WHERE amount > 0")
summary(new2_data)

# Remove customer id as a variable, store it as row names
head(new2_data)
row.names(new2_data) = new_data$customer_id
new2_data$customer_id = NULL
head(new2_data)

# Take the log-transform of the amount, and plot
new_data2$amount = log(new_data$amount)
hist(new2_data$amount)

# Standardize variables
new_data2 = scale(new_data)
head(new2_data)

# Call the pie chart functions
myPieChart(customers, "Pie Chart of Customer Segments\n (Portugal)")

aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# Display the data after transformation
#head(data)
#summary(data)


# Compute key marketing indicators using SQL language
# library(sqldf)

# Compute recency, frequency, and average purchase amount
# customers_2013 = sqldf("SELECT customer_id,
#                                MIN(days_since) AS 'recency',
#                                MAX(days_since) AS 'first_purchase',
#                                COUNT(*) AS 'frequency',
#                                AVG(purchase_amount) AS 'amount'
#                         FROM data GROUP BY 1")

# Explore the data
head(customers_2013)
summary(customers_2013)
hist(customers_2013$recency)
hist(customers_2013$frequency)
hist(customers_2013$amount)
hist(customers_2013$amount, breaks = 100)

# --- CODING A MANAGERIAL SEGMENTATION ---------------------

# Simple 2-segment solution based on recency alone
# customers_2013$segment = ifelse(test = customers_2013$recency > 365*3, yes = "inactive", no = "NA")
# table(customers_2013$segment)
# aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# A more complex 3-segment solution based on recency alone
# customers_2013$segment = ifelse(test = customers_2013$recency > 365*3,
#                            yes = "inactive",
#                            no = ifelse(test = customers_2013$recency > 365*2,
#                                        yes = "cold",
#                                        no = "NA"))
# table(customers_2013$segment)
# aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# Simple 2-segment solution using the which statement
# customers_2013$segment = "NA"
# customers_2013$segment[which(customers_2013$recency > 365*3)] = "inactive"
# table(customers_2013$segment)
# aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# More complex 4-segment solution using which
# customers_2013$segment = "NA"
# customers_2013$segment[which(customers_2013$recency > 365*3)] = "inactive"
# customers_2013$segment[which(customers_2013$recency <= 365*3 & customers_2013$recency > 365*2)] = "cold"
# customers_2013$segment[which(customers_2013$recency <= 365*2 & customers_2013$recency > 365*1)] = "warm"
# customers_2013$segment[which(customers_2013$recency <= 365)] = "active"
# table(customers_2013$segment)
# aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# Complete segment solution using which, and exploiting previous test as input
# customers_2013$segment = "NA"
# customers_2013$segment[which(customers_2013$recency > 365*3)] = "inactive"
# customers_2013$segment[which(customers_2013$recency <= 365*3 & customers_2013$recency > 365*2)] = "cold"
# customers_2013$segment[which(customers_2013$recency <= 365*2 & customers_2013$recency > 365*1)] = "warm"
# customers_2013$segment[which(customers_2013$recency <= 365)] = "active"
# customers_2013$segment[which(customers_2013$segment == "warm" & customers_2013$first_purchase <= 365*2)] = "new warm"
# customers_2013$segment[which(customers_2013$segment == "warm" & customers_2013$amount < 100)] = "warm low value"
# customers_2013$segment[which(customers_2013$segment == "warm" & customers_2013$amount >= 100)] = "warm high value"
# customers_2013$segment[which(customers_2013$segment == "active" & customers_2013$first_purchase <= 365)] = "new active"
# customers_2013$segment[which(customers_2013$segment == "active" & customers_2013$amount < 100)] = "active low value"
# customers_2013$segment[which(customers_2013$segment == "active" & customers_2013$amount >= 100)] = "active high value"
# table(customers_2013$segment)
# aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# Re-order factor in a way that makes sense
# customers_2013$segment = factor(x = customers_2013$segment, levels = c("inactive", "cold",
#                                                              "warm high value", "warm low value", "new warm",
#                                                              "active high value", "active low value", "new active"))
# table(customers_2013$segment)
# aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)


# --- SEGMENTING A DATABASE RETROSPECTIVELY ----------------

customers_2012 <- rfmSegmentation(data, 150, 365)
# Compute key marketing indicators using SQL language
# library(sqldf)
# 
# # Compute recency, frequency, and average purchase amount
# customers_2012 = sqldf("SELECT customer_id,
#                                MIN(days_since) - 365 AS 'recency',
#                                MAX(days_since) - 365 AS 'first_purchase',
#                                COUNT(*) AS 'frequency',
#                                AVG(purchase_amount) AS 'amount'
#                         FROM data
#                         WHERE days_since > 365
#                         GROUP BY 1")
# 
# # Complete segment solution using which, and exploiting previous test as input
# customers_2012$segment = "NA"
# customers_2012$segment[which(customers_2012$recency > 365*3)] = "inactive"
# customers_2012$segment[which(customers_2012$recency <= 365*3 & customers_2012$recency > 365*2)] = "cold"
# customers_2012$segment[which(customers_2012$recency <= 365*2 & customers_2012$recency > 365*1)] = "warm"
# customers_2012$segment[which(customers_2012$recency <= 365)] = "active"
# customers_2012$segment[which(customers_2012$segment == "warm" & customers_2012$first_purchase <= 365*2)] = "new warm"
# customers_2012$segment[which(customers_2012$segment == "warm" & customers_2012$amount < 100)] = "warm low value"
# customers_2012$segment[which(customers_2012$segment == "warm" & customers_2012$amount >= 100)] = "warm high value"
# customers_2012$segment[which(customers_2012$segment == "active" & customers_2012$first_purchase <= 365)] = "new active"
# customers_2012$segment[which(customers_2012$segment == "active" & customers_2012$amount < 100)] = "active low value"
# customers_2012$segment[which(customers_2012$segment == "active" & customers_2012$amount >= 100)] = "active high value"
# 
# # Re-order factor in a way that makes sense
# customers_2012$segment = factor(x = customers_2012$segment, levels = c("inactive", "cold",
#                                                                        "warm high value", "warm low value", "new warm",
#                                                                        "active high value", "active low value", "new active"))
# 
# # Show segmentation results
# table(customers_2012$segment)
# pie(table(customers_2012$segment), col = rainbow(24))

# Show segmentation results
myPieChart(customers_2012, "Pie Chart of Customer Segments\n (Portugal 2012)")
aggregate(x = customers_2012[, 2:5], by = list(customers_2012$segment), mean)


# --- COMPUTING REVENUE GENERATION PER SEGMENT -------------


# Compute how much revenue is generated by segments
# Notice that people with no revenue in 2013 do NOT appear
revenue_2013 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2013'
                      FROM data
                      WHERE year_of_purchase = 2013
                      GROUP BY 1")
summary(revenue_2013)

# Merge 2013 customers and 2013 revenue (the wrong way)
# actual = merge(customers_2013, revenue_2013)

# Merge 2013 customers and 2013 revenue (correct)
actual = merge(customers_2013, revenue_2013, all.x = TRUE)
actual$revenue_2013[is.na(actual$revenue_2013)] = 0

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2013, by = list(customers_2013$segment), mean)

# Merge 2012 customers and 2013 revenue (correct)
forward = merge(customers_2012, revenue_2013, all.x = TRUE)
forward$revenue_2013[is.na(forward$revenue_2013)] = 0

# Show average revenue per customer and per segment
r = aggregate(x = forward$revenue_2013, by = list(customers_2012$segment), mean)
print(r)

# Re-order and display results
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)
