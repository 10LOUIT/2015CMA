# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 2 - MANAGERIAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////

rm(list=ls(all=TRUE))
setwd("~/Documents/github/2015CMA/RFM")
setwd("~/OneDrive/B_Data/Berner_PT")
# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------

# Load necessary packages
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(tcltk)

# Compute key marketing indicators using SQL language
library(sqldf)

# Load text file into local variable called 'data'
raw_data = read.delim(file = 'Customer_purchases_2011_2013_with_empty_amounts.csv', header = FALSE, sep = ',', dec = '.')

# Manipulate data of old customers without purchases in this file

# Select all customers with transactions in the selected period 
new_data = sqldf("SELECT * FROM raw_data WHERE V3 > 0")

# Select all customers without any transactions in the selected period
old_data = sqldf("SELECT * FROM raw_data WHERE V3 = 0")

# Assignment of purchase amount and date of purchase
old_data$V3 = "2008-01-01" 
old_data$V2 = "0.00"

# Select all customers with negative purchase in the selected period
# negative_data = sqldf("SELECT * FROM raw_data WHERE V2 < 0")

# Merge the processed data
data = merge(new_data, old_data, all.x = TRUE, all.y = TRUE)
# Filtering out all transactions with a negative amount of purchase
data = sqldf("SELECT * FROM raw_data WHERE V2 > -0.01")
#data = sqldf("SELECT * FROM raw_data WHERE V2 > -0.01 AND V3 NOT LIKE 0")

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
 
  # return data
  dataOutput
  
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

# Data Exploration and visualization
myPieChart(customers, "Pie Chart of Customer Segments\n (RFM)")
aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)

# Explore the data
head(customers_2013)
summary(customers_2013)
hist(customers_2013$recency)
hist(customers_2013$frequency)
hist(customers_2013$amount)
hist(customers_2013$amount, breaks = 100)

# --- SEGMENTING A DATABASE RETROSPECTIVELY ----------------
# Segment customers with transactions in the previous year
customers_2012 <- rfmSegmentation(data, 150, 365)

# Show segmentation results
myPieChart(customers_2012, "Pie Chart of Customer Segments\n (RFM 2012)")
aggregate(x = customers_2012[, 2:5], by = list(customers_2012$segment), mean)

# --- COMPUTING REVENUE GENERATION PER SEGMENT -------------

# Compute how much revenue is generated by segments
# Notice that people with no revenue in 2013 do NOT appear
revenue_2013 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2013'
                      FROM data
                      WHERE year_of_purchase = 2013
                      GROUP BY 1")
summary(revenue_2013)

# Merge 2013 customers and 2013 revenue (correct)
actual = merge(customers_2013, revenue_2013, all.x = TRUE)
actual$revenue_2013[is.na(actual$revenue_2013)] = 0
myPieChart(actual, "Pie Chart of Customer Segments\n (actual RFM 2013)")

# Show average revenue per customer and per segment
actual_aggregation = aggregate(x = actual$revenue_2013, by = list(customers_2013$segment), mean)
# Re-order and display results
actual_aggregation = actual_aggregation[order(actual_aggregation$x, decreasing = TRUE), ]
print(actual_aggregation)
barplot(actual_aggregation$x, names.arg = actual_aggregation$Group.1, legend.text = round(actual_aggregation$x, 2), xlab = "Customer Segments", ylab = "Average revenue", col = c("darkgreen","green","orange","yellow","darkblue","blue","red"), main="Average revenue per customer and per segment")

# Show total revenue per customer and per segment
actual_total = aggregate(x = actual$revenue_2013, by = list(customers_2013$segment), sum)
actual_total = actual_total[order(actual_total$x, decreasing = TRUE), ]
barplot(actual_total$x, names.arg = actual_total$Group.1, legend.text = round(actual_total$x, 2), xlab = "Customer Segments", ylab = "total revenue", col = c("darkgreen","green","orange","yellow","darkblue","blue","red"), main="Total revenue per customer and per segment")

# Merge 2012 customers and 2013 revenue (correct)
forward = merge(customers_2012, revenue_2013, all.x = TRUE)
forward$revenue_2013[is.na(forward$revenue_2013)] = 0

# Show average revenue per customer and per segment
r = aggregate(x = forward$revenue_2013, by = list(customers_2012$segment), mean)
print(r)

# Re-order and display results
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1, legend.text = round(r$x, 2), xlab = "Customer Segments", ylab = "Average revenue", col = c("darkgreen","green","orange","yellow","darkblue","blue","red"), main="Average revenue per customer and per segment")
