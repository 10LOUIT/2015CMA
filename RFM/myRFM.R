# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 2 - MANAGERIAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////




# --- PREPARING AND TRANSFORMING DATA ----------------------


# Explore the data
head(customers_2015)
summary(customers_2015)
hist(customers_2015$recency)
hist(customers_2015$frequency)
hist(customers_2015$amount)
hist(customers_2015$amount, breaks = 100)

# --- SEGMENTING A DATABASE RETROSPECTIVELY ----------------

# Merge 2014 customers and 2014 revenue (correct)
previous = merge(customers_2014, revenue_2014, all.x = TRUE)
previous$revenue_2014[is.na(previous$revenue_2014)] = 0
myPieChart(previous, "Pie Chart of Customer Segments\n (previous RFM 2014)")

# Show average revenue per customer and per segment
previous_aggregation = aggregate(x = previous$revenue_2014, by = list(customers_2014$segment), mean)

# Re-order and display results
previous_aggregation = previous_aggregation[order(previous_aggregation$x, decreasing = TRUE), ]
print(previous_aggregation)
barplot(previous_aggregation$x, names.arg = previous_aggregation$Group.1, legend.text = round(previous_aggregation$x, 2), xlab = "Customer Segments", ylab = "Average revenue", col = c("darkgreen","green","orange","yellow","darkblue","blue","red"), main="Average revenue per customer and per segment")

# Show total revenue per customer and per segment
previous_total = aggregate(x = previous$revenue_2014, by = list(customers_2014$segment), sum)
previous_total = previous_total[order(previous_total$x, decreasing = TRUE), ]
myBarplot(previous_total, "Total revenue per customer and per segment", "Customer Segments", "Total revenue")

# Compute the total revenue
colSums(revenue_2014[2])

# Merge 2013 customers and 2014 revenue (correct)
forward = merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0

# Prerequisite: Compute the revenue 2015 and the enhanced data object 
# Merge 2014 customers and 2015 revenue (Enhanced)
in_sample = merge(enhanced_customers_2014, revenue_2015, all.x = TRUE)
in_sample$revenue_2015[is.na(in_sample$revenue_2015)] = 0
in_sample$active_2015 = as.numeric(in_sample$revenue_2015 > 0)

# Display calibration (in-sample) data
head(in_sample)
summary(in_sample)


# --- CALIBRATE THE MODELS ---------------------------------


# Calibrate probability model
library(nnet)
prob.model = multinom(formula = active_2015 ~ recency + first_purchase + frequency + avg_amount + max_amount,
                      data = in_sample)

coef = summary(prob.model)$coefficients
std  = summary(prob.model)$standard.errors
print(coef)
print(std)
print(coef / std)

# For the monetary model, select only those who made a purchase
z = which(in_sample$active_2015 == 1)
head(in_sample[z, ])
summary(in_sample[z, ])

# Calibrate the monetary model (version 1)
amount.model = lm(formula = revenue_2015 ~ avg_amount + max_amount, data = in_sample[z, ])
summary(amount.model)

# Plot the results of the monetary model
plot(x = in_sample[z, ]$revenue_2015, y = amount.model$fitted.values)

# Re-calibrate the monetary model, using a log-transform (version 2)
amount.model = lm(formula = log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = in_sample[z, ])
summary(amount.model)

# Plot the results of this new monetary model
plot(x = log(in_sample[z, ]$revenue_2015), y = amount.model$fitted.values)

# Show average revenue per customer and per segment
r = aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), sum)
print(r)

# Re-order and display results
r = r[order(r$x, decreasing = TRUE), ]
print(r)
myBarplot(r, "Average revenue per customer and per segment", "Customer Segments", "Average revenue")

# --- COMPUTE TRANSITION MATRIX ------------------------------------------------------------------------------------


# Compute transition matrix
new_data = merge(x = customers_2014, y = customers_2015, by = "customer_id", all.x = TRUE, all.y = TRUE)
head(new_data)
transition = table(new_data$segment.x, new_data$segment.y)
print(transition)
table(new_data$segment.x)
table(new_data$segment.y)
# Divide each row by its sum
transition = transition / rowSums(transition)
print(transition)


# --- USE TRANSITION MATRIX TO MAKE PREDICTIONS --------------------------------------------------------------------


# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments = matrix(nrow = 8, ncol = 11)
segments[, 1] = table(customers_2014$segment)
colnames(segments) = 2015:2025
row.names(segments) = levels(customers_2015$segment)
print(segments)

# Compute for each an every period
for (i in 2:11) {
  segments[, i] = segments[, i-1] %*% transition
}

# Plot inactive, active high value customers over time
barplot(segments[1, ], main = "inactive Customers")
barplot(segments[2, ], main = "cold Customers")
barplot(segments[6, ], main = "active high value")
barplot(segments[7, ], main = "active low value")

# Display how segments will evolve over time
print(round(segments))


# --- COMPUTE THE (DISCOUNTED) CLV OF A DATABASE -------------------------------------------------------------------


# Yearly revenue per segment
# This comes directly from module 2, lines 160-161
yearly_revenue = c(0, 0, 0, 0, 0, 240.08, 94.05, 143.53)

# Compute revenue per segment
revenue_per_segment = yearly_revenue * segments
print(revenue_per_segment)

# Compute yearly revenue
yearly_revenue = colSums(revenue_per_segment)
print(round(yearly_revenue))
barplot(yearly_revenue)

# Compute cumulated revenue
cumulated_revenue = cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)

# Create a discount factor
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount
print(round(disc_yearly_revenue))
barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)

# What is the database worth?
print(disc_cumulated_revenue[11] - yearly_revenue[1])
