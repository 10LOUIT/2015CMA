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

# ------------------------------------------------------------------------------------------------------------
# Cluster Analysis
cluster_data = negative_data
cluster_data$V3 = as.numeric(difftime(time1 = "2014-01-01",time2 = cluster_data$V3, units = "days"))
clusters = kmeans(cluster_data, 5, 10)
table(clusters$size)
hist(clusters$size)
plot(clusters$cluster, clusters$fitted.values)

library(cluster)
#library(fpc)

# Prepare Data
mydata <- na.omit(customers_with_positive_transactions) # listwise deletion of missing

# Add headers and interpret the last column as a date, extract year of purchase
# Set the starting date for calculating days since the last purchase was done
colnames(mydata) = c('customer_id', 'purchase_amount', 'date_of_purchase')
mydata$date_of_purchase = as.Date(mydata$date_of_purchase, "%Y-%m-%d")
mydata$year_of_purchase = as.numeric(format(mydata$date_of_purchase, "%Y"))
mydata$days_since       = as.numeric(difftime(time1 = "2015-01-01",
                                            time2 = mydata$date_of_purchase,
                                            units = "days"))

# Remove customer id as a variable, store it as row names
row.names(mydata) = mydata$customer_id
mydata$customer_id = NULL
head(mydata)
mydata = mydata[,-3]
mydata <- scale(mydata) # standardize variables

# Filtering out Customers with order and following return
customers_with_negative_transactions = sqldf("SELECT * FROM customers_with_transactions WHERE V2 < 0")
customers_with_positive_transactions = sqldf("SELECT * FROM customers_with_transactions WHERE V2 > 0")
customers_with_order_and_return = sqldf("SELECT customers_with_positive_transactions.V1, customers_with_positive_transactions.V2, customers_with_positive_transactions.V3, customers_with_negative_transactions.V2, customers_with_negative_transactions.V3 FROM customers_with_positive_transactions INNER JOIN customers_with_negative_transactions ON customers_with_positive_transactions.V1 = customers_with_negative_transactions.V1 WHERE NOT -customers_with_negative_transactions.V2 = customers_with_positive_transactions.V2")

# Delete those transactions (not finish)
customers_without_returns = sqldf("SELECT * FROM customers_with_positive_transactions INNER JOIN customers_with_negative_transactions ON customers_with_positive_transactions.V1 = customers_with_negative_transactions.V1 WHERE NOT -customers_with_negative_transactions.V2 = customers_with_positive_transactions.V2")
rm(customers_without_returns)

# ------------------------------------------------------------------------------------------------------------
# Explore the processed data (Not finish)
#table(customers$segment)
d2014 = table(customers_2014$segment)
print(d2014)
d2013 = table(customers_2013$segment)
print(d2013)
d2012 = table(customers_2012$segment)
print(d2012)
y = rbind(d2014, d2013, d2012)
# Create a matrix to compare the segments with years
dm = matrix(ncol = 8, nrow = 3)
dm = y

colnames(dm) = c("active high value", "active low value", "new active", "warm high value", "warm low value", "new warm", "cold", "inactive")
row.names(dm) = c("2014", "2013", "2012")
# How to set rowname of [0]
# rownames(dm)[0] = "years"
d = merge(d2014, d2013, all.x = TRUE, all.y = TRUE)
d = merge(d, d2012, all.x = TRUE, all.y = TRUE)
print(d)

barplot(as.matrix(dm), main="Comparing Customer Segments", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=rainbow(8))

# ------------------------------------------------------------------------------------------------------------
