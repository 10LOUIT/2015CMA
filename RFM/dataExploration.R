# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    DATA EXPLORATION
# __________________________________________________________
# //////////////////////////////////////////////////////////

library(ggplot2)

# Explore customer data with negative amounts
negative_data$date_of_purchase = as.Date(negative_data$date_of_purchase, "%Y-%m-%d")
hist(negative_data$purchase_amount)
summary(negative_data$purchase_amount)
plot(negative_data$purchase_amount, negative_data$date_of_purchase)

# ------------------------------------------------------------------------------------------------------------
# Cluster Analysis
cluster_data = negative_data
cluster_data$date_of_purchase = as.numeric(difftime(time1 = "2014-01-01",time2 = cluster_data$date_of_purchase, units = "days"))
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
customers_with_negative_transactions = sqldf("SELECT * FROM raw_data_6 WHERE purchase_amount < 0")
colSums(customers_with_negative_transactions[2])

customers_with_positive_transactions = sqldf("SELECT * FROM raw_data_6 WHERE purchase_amount > 0")
colSums(customers_with_positive_transactions[2])

customers_clean = sqldf("SELECT * FROM customers_with_positive_transactions INNER JOIN customers_with_negative_transactions ON customers_with_positive_transactions.customer_id = customers_with_negative_transactions.customer_id WHERE NOT -customers_with_negative_transactions.purchase_amount = customers_with_positive_transactions.purchase_amount")
customers_clean = sqldf("SELECT * FROM customers_with_positive_transactions as T1 JOIN customers_with_negative_transactions as T2 ON NOT T1.customer_id = T2.customer_id AND NOT T1.purchase_amount = -T2.purchase_amount")

# DELETE FROM `table1` WHERE `group` in (SELECT DISTINCT `group` FROM `table2`)
customers_clean = sqldf("DELETE FROM customers_with_positive_transactions WHERE customer_id IN (SELECT customer_id FROM customers_with_negative_transactions WHERE customers_with_positive_transactions.purchase_amount = -customers_with_negative_transactions.purchase_amount)")

# Delete those transactions (not finish)
customers_without_returns = sqldf("SELECT * FROM customers_with_positive_transactions INNER JOIN customers_with_negative_transactions ON customers_with_positive_transactions.customer_id = customers_with_negative_transactions.customer_id WHERE NOT -customers_with_negative_transactions.purchase_amount = customers_with_positive_transactions.purchase_amount")
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

# --------- Data Exploration and visualization -----------

myPieChart(customers_2012, "Pie Chart of Customer Segments\n (RFM 2012)")
aggregate(x = customers_2012[, 2:5], by = list(customers_2012$segment), mean)
aggretation_2012 = aggregate(x = customers_2012$amount, by = list(customers_2012$segment), mean)
aggretation_2012 = aggretation_2012[order(aggretation_2012$Group.1, decreasing = TRUE), ]

myPieChart(customers_2013, "Pie Chart of Customer Segments\n (RFM 2013)")
aggregate(x = customers_2013[, 2:5], by = list(customers_2013$segment), mean)
aggretation_2013 = aggregate(x = customers_2013$amount, by = list(customers_2013$segment), mean)
aggretation_2013 = aggretation_2013[order(aggretation_2013$Group.1, decreasing = TRUE), ]

myPieChart(customers_2014, "Pie Chart of Customer Segments\n (RFM 2014)")
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)
aggretation_2014 = aggregate(x = customers_2014$amount, by = list(customers_2014$segment), mean)
aggretation_2014 = aggretation_2014[order(aggretation_2014$Group.1, decreasing = TRUE), ]

myPieChart(customers_2015, "Pie Chart of Customer Segments\n (RFM 2015)")
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)
aggretation_2015 = aggregate(x = customers_2015$amount, by = list(customers_2015$segment), mean)
aggretation_2015 = aggretation_2015[order(aggretation_2015$Group.1, decreasing = TRUE), ]

#aggretation_customers_2015 = aggregate(x = customers_2015$customer_id, by = list(customers_2015$segment), mean)

# Show results in console
print(aggretation_2012)
print(aggretation_2013)
print(aggretation_2014)
print(aggretation_2015)

# Create data object average purchase amount per segments over years
avg_revenue_per_segment_1 = merge(aggretation_2014, aggretation_2015, by = "Group.1", all.x = TRUE, all.y = TRUE)
colnames(avg_revenue_per_segment_1) = c("Segment", "2014", "2015")
avg_revenue_per_segment_2 = merge(aggretation_2012, aggretation_2013, by = "Group.1", all.x = TRUE, all.y = TRUE)
colnames(avg_revenue_per_segment_2) = c("Segment", "2012", "2013")
avg_revenue_per_segment = merge(avg_revenue_per_segment_2, avg_revenue_per_segment_1, by = "Segment", all.x = TRUE, all.y = TRUE)
avg_revenue_per_segment$`2012`[is.na(avg_revenue_per_segment$`2012`)] = 0

# Calculate ratio of change per customer segment compared to the previous year (inf)
avg_revenue_per_segment$Ratio = as.numeric(round((avg_revenue_per_segment$`2015`/avg_revenue_per_segment$`2014`)*100), digits = 2)

# Save average revenue per customer and per segment
yearly_revenue = aggretation_2014$x
yearly_revenue

# Compare revenue per customer
compare_revenue = merge(revenue_2014, revenue_2015)
plot(compare_revenue$revenue_2014, x = compare_revenue$customer_id)
