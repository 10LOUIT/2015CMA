# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    DATA PREPARATION
# __________________________________________________________
# //////////////////////////////////////////////////////////

rm(list=ls(all=TRUE))
setwd("~/Documents/github/2015CMA/RFM")
setwd("~/OneDrive/B_Data/Berner_PT")

# Load necessary libraries
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(tcltk)

# Compute key marketing indicators using SQL language
library(sqldf)

# Load text file into local variable called 'raw data'
raw_data1 = read.delim(file = 'Customer_purchases_2011_2013_with_empty_amounts.csv', header = FALSE, sep = ',', dec = '.')
raw_data2 = read.delim(file = 'purchases_2014.txt', header = FALSE, sep = ',', dec = '.')
raw_data = merge(raw_data1, raw_data2, all.x = TRUE, all.y = TRUE)

# Remove raw data from memory
rm(raw_data1, raw_data2)

# Add headers and interpret the last column as a date, extract year of purchase
# Set the starting date for calculating days since the last purchase was done
data = raw_data
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2015-01-01", time2 = data$date_of_purchase, units = "days"))

# Data cleansing of customers without purchases in this file
data = na.omit(data)

# --------------- Data Cleansing, Filter out all puchases with following returns -------------------------------
#

# Select all customer transactions in the selected period 
purchases = sqldf("SELECT * FROM data WHERE purchase_amount > 0")

# Select all customer without any transactions in the selected period
credits = sqldf("SELECT * FROM data WHERE purchase_amount < 0")

# Delete purchases with following returns
#new_data = purchases[ !(purchases$customer_id %in% bookback$customer_id), ]
# Filtering out Customers with order and following return
customers_with_order_and_return = sqldf("DELETE * FROM purchases INNER JOIN credits ON purchases.customer_id = credits.customer_id WHERE -credits.purchase_amount = purchases.purchase_amount")
#clean = merge(purchases, credits, by="customer_id", all.y=TRUE)

# Assignment of purchase amount and date of purchase to customers without transactions
#customers_without_transactions$V3 = "2000-01-01"
#customers_without_transactions$V2 = "0.00"

# Select all customers with negative purchase in the selected period
# negative_data = sqldf("SELECT * FROM raw_data WHERE V2 < 0")

# Merge the processed data together in one object
#data = merge(customers_with_transactions, customers_without_transactions, all.x = TRUE, all.y = TRUE)

# 
# Filtering out all transactions with a negative amount of purchase
# Non Consideration of Orders with following Returns
#

#customers_with_purchases = sqldf("SELECT * FROM raw_data WHERE V2 > -0.01")
#data = sqldf("SELECT * FROM data WHERE V2 > 0.00")
#data = sqldf("SELECT * FROM raw_data WHERE V2 > -0.01 AND V3 NOT LIKE 0")

# Remove data from memory 
#rm(new_data)

