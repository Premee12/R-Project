# install.packages("RMySQL")
#install.packages("ggplot2")

library(RMySQL)
library(ggplot2)
#library(dplyr)

# this session I incorporated SQL into R to perform analysis

conn <- dbConnect(MySQL(),
                  dbname = "sql_project",
                  host = "localhost",
                  user = "root",  
                  password = "")

dbListTables(conn) # to check the list of tables and confrim my connection

dbListFields(conn, "telecom_customers") # columns view 

churn_data <- dbGetQuery(conn, "SELECT * FROM telecom_customers;")

head(churn_data)  #first few rows

summary(churn_data)  #Summary statistics

# doing some SQL analysis in R to understand the data better

customer_count <- dbGetQuery(conn, "SELECT COUNT(*) AS total_customers FROM telecom_customers;")
print(customer_count)
# we have 7043 customer records


# Data Cleaning 

# Check for missing values in each column
missing_values <- colSums(is.na(churn_data))
print(missing_values[missing_values > 0])  # Show only columns with missing values

# Check for duplicate rows
duplicate_rows <- sum(duplicated(churn_data))
print(paste("Number of duplicate rows:", duplicate_rows))

# Check for Negative or Invalid Values (E.g Monthly Charge)
if (any(churn_data$monthly_charge < 0, na.rm = TRUE)) {
  print("Warning: Negative values detected in monthly_charge column.")
}

summary(churn_data)

# churn distribution
churn_count <- dbGetQuery(conn, 
"SELECT customer_status, COUNT(*) AS count FROM telecom_customers GROUP BY customer_status;")
print(churn_count)

# plot a pie chart
churn_count$percentage <- round(churn_count$count / sum(churn_count$count) * 100, 1)
ggplot(churn_count, aes(x = "", y = count, fill = customer_status)) +
geom_bar(stat = "identity", width = 1, color = "white") +
coord_polar(theta = "y") +  # Convert to a pie chart
geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5), size = 5) +
labs(title = "Customer Churn Distribution", fill = "Customer Status") + theme_void() + 
scale_fill_brewer(palette = "Set2")


# find out what customers are joined using that tenure
joined_tenure <- dbGetQuery(conn, 
"SELECT tenure_in_months, COUNT(*) AS count 
FROM  telecom_customers
WHERE customer_status = 'Joined'
GROUP BY tenure_in_months
ORDER BY tenure_in_months ASC;")
print(joined_tenure)


# Create bar chart

joined_tenure$total <- sum(joined_tenure$count)
joined_tenure$proportion <- joined_tenure$count / joined_tenure$total

ggplot(joined_tenure, aes(x = factor(tenure_in_months), y = proportion, fill = tenure_in_months)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "green", high = "orange") +  # Gradient fill for visual appeal
  labs(title = "Proportion of Joined Customers by Tenure",
       x = "Tenure (Months)",
       y = "Proportion of Joined Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# for more context looked into tenure of customers with churn status
joined_tenure <- dbGetQuery(conn, "SELECT tenure_in_months, COUNT(*) AS count 
FROM  telecom_customers
WHERE customer_status = 'Churned'
GROUP BY tenure_in_months
ORDER BY tenure_in_months ASC;")
print(joined_tenure)
# it also shows here churn is common among 1-3 months

# Analysis into churned customers within 1-3 months tenure who are the majority churn
early_churn <- dbGetQuery(conn, "SELECT * 
FROM telecom_customers 
WHERE customer_status = 'Churned' 
AND tenure_in_months BETWEEN 1 AND 3;")
print(head(early_churn))

# show a chart showing the top 10 tenure months with the highest churn count
top_10_tenure <- head(joined_tenure, 10)  # Selecting first 10 rows
top_10_tenure$tenure_in_months <- factor(top_10_tenure$tenure_in_months, levels = rev(top_10_tenure$tenure_in_months))
ggplot(top_10_tenure, aes(y = tenure_in_months, x = count)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Top 10 Tenure Months with Highest Churn Count", x = "Churn Count", y = "Tenure (Months)") +
  theme_minimal()

# Age Distribution of Early Churners
ggplot(early_churn, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Age Distribution of Early Churned Customers",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Churn Breakdown by Contract Type
ggplot(early_churn, aes(x = contract, fill = contract)) +
  geom_bar() +
  labs(title = "Contract Type Distribution Among 1-3 Month Churners",
       x = "Contract Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

#Churned Customers by Internet Type
ggplot(early_churn, aes(x = internet_type, fill = internet_type)) +
  geom_bar() +
  labs(title = "Internet Type Distribution for Early Churned Customers",
       x = "Internet Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

#  Monthly Charges vs. Tenure for Early Churners
ggplot(early_churn, aes(x = tenure_in_months, y = monthly_charge)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Monthly Charges vs. Tenure (1-3 Month Churners)",
       x = "Tenure (Months)",
       y = "Monthly Charges ($)") +
  theme_minimal()
# this had no important effect

ggplot(early_churn, aes(x = churn_category, fill = churn_category)) +
  geom_bar() +
  labs(title = "Churn Reasons for 1-3 Month Customers",
       x = "Churn Category",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")



# to better understand i looked into stayed customer tenure
joined_tenure <- dbGetQuery(conn, "SELECT tenure_in_months, COUNT(*) AS count 
FROM  telecom_customers
WHERE customer_status = 'Stayed'
GROUP BY tenure_in_months
ORDER BY tenure_in_months ASC;")
print(joined_tenure)
# This analysis confirms that Stayed customers have a tenure >4 months,
# while Joined customers (1-3 months) are  new customers. reinforcing the need to focus retention efforts on new customers within the first 3 months.

# analyse new customers 
joined_behavior <- dbGetQuery(conn, 
"SELECT tenure_in_months, monthly_charge, contract, internet_service, phone_service 
   FROM telecom_customers 
   WHERE customer_status = 'Joined' 
   ORDER BY tenure_in_months;")
print(joined_behavior)

#make some plots

# Bar Chart: Contract Type Distribution
ggplot(joined_behavior, aes(x = contract, fill = contract)) +
  geom_bar() +
  labs(title = "Contract Type Distribution Among New Customers",
       x = "Contract Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Bar Chart: Internet Service Subscription
ggplot(joined_behavior, aes(x = internet_service, fill = internet_service)) +
  geom_bar() +
  labs(title = "Internet Service Subscription Among New Customers",
       x = "Internet Service",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Internet & Phone Service Subscription Among New Customers
ggplot(joined_behavior, aes(x = internet_service, fill = phone_service)) +
  geom_bar(position = "stack") +  # Stacked bar chart
  labs(title = "Internet & Phone Service Subscription Among New Customers",
       x = "Internet Service",
       y = "Count",
       fill = "Phone Service") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# to deep dive into internet type and fiber optics to see whats wrong 
fiber_churn <- dbGetQuery(conn, "SELECT * FROM telecom_customers 
   WHERE customer_status = 'Churned' AND internet_type = 'Fiber Optic';")
print(head(fiber_churn))

# look into churn reason why fibre optic customers churn
ggplot(fiber_churn, aes(x = churn_category, fill = churn_category)) +  geom_bar() +
  labs(title = "Churn Reasons for Fiber Optic Customers",
       x = "Churn Category", y = "Count") + theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# contract type fiber optics churn
ggplot(fiber_churn, aes(x = contract, fill = contract)) +
  geom_bar() +
  labs(title = "Contract Type of Fiber Optic Churners",
       x = "Contract Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# payment method 
ggplot(fiber_churn, aes(x = paperless_billing, fill = payment_method)) +
  geom_bar(position = "dodge") +
  labs(title = "Paperless Billing vs. Payment Method for Fiber Optic Churners",
       x = "Paperless Billing",
       y = "Count",
       fill = "Payment Method") +
  theme_minimal()

# insights into tv streaming for fiber optics customrs
ggplot(fiber_churn, aes(x = streaming_tv, fill = streaming_tv)) +
  geom_bar() +
  labs(title = "Churned Fiber Optic Customers by Streaming TV Subscription",
       x = "Streaming TV Subscription",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Query to analyze long-term churned customers (tenure > 24 months)
long_term_churn <- dbGetQuery(conn, 
"SELECT tenure_in_months, monthly_charge, contract, internet_service, phone_service
FROM telecom_customers
WHERE customer_status = 'Churned' 
AND tenure_in_months > 12
ORDER BY tenure_in_months DESC;")
print(long_term_churn)

#make some plots

# Scatter Plot of Tenure vs Monthly Charges
ggplot(long_term_churn, aes(x = tenure_in_months, y = monthly_charge)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Tenure vs Monthly Charges (Churned Customers)", 
       x = "Tenure (Months)", 
       y = "Monthly Charges ($)") +
  theme_minimal()

# Bar Chart: Contract Type Distribution Among Churned Customers
ggplot(long_term_churn, aes(x = contract, fill = contract)) +
  geom_bar() +
  labs(title = "Contract Type Distribution of Churned Customers",
       x = "Contract Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Boxplot: Monthly Charges by Contract Type
ggplot(long_term_churn, aes(x = contract, y = monthly_charge, fill = contract)) +
  geom_boxplot() +
  labs(title = "Monthly Charges by Contract Type (Churned Customers)",
       x = "Contract Type",
       y = "Monthly Charges ($)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Location analysis for churn 
# Query to get the top 5 cities with the highest churn count
top_churn_cities <- dbGetQuery(conn, "SELECT city, COUNT(*) AS churn_count 
   FROM telecom_customers 
   WHERE customer_status = 'Churned' 
   GROUP BY city 
   ORDER BY churn_count DESC 
   LIMIT 5;")
print(top_churn_cities)


# Bar Chart for Top 5 Churn Cities
ggplot(top_churn_cities, aes(x = reorder(city, -churn_count), y = churn_count, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Cities with Highest Churn",
       x = "City",
       y = "Churn Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# churn reasons 
churn_reasons <- dbGetQuery(conn, 
 "SELECT churn_reason, COUNT(*) AS count 
FROM telecom_customers 
WHERE customer_status = 'Churned' 
GROUP BY churn_reason 
ORDER BY count DESC;")
print(churn_reasons)

# deeper insights into Churn 

churn_analysis <- dbGetQuery(conn, 
                             "SELECT churn_category, churn_reason, gender, age, married, number_of_dependents, 
          tenure_in_months, contract, payment_method, monthly_charge, total_charges, 
          internet_type, phone_service, streaming_tv, streaming_movies 
   FROM telecom_customers 
   WHERE customer_status = 'Churned';")
print(head(churn_analysis))

# analyse Churn Category Breakdown by Gender
ggplot(churn_analysis, aes(x = churn_category, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Category Breakdown by Gender",
       x = "Churn Category",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Category of Churn and Age Distribution 
ggplot(churn_analysis, aes(x = age, fill = churn_category)) +
  geom_histogram(binwidth = 5, position = "stack", alpha = 0.7) +
  labs(title = "Churn Category Distribution by Age",
       x = "Age",
       y = "Count") + theme_minimal()

# Churn reasons
ggplot(churn_analysis, aes(x = churn_reason, fill = churn_reason)) +
  geom_bar() +
  labs(title = "Distribution of Churn Reasons",
       x = "Churn Reason",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# the palette where not enough

ggplot(churn_analysis, aes(x = churn_reason, fill = churn_reason)) +
  geom_bar() +
  labs(title = "Distribution of Churn Reasons",
       x = "Churn Reason",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_manual(values = scales::hue_pal()(length(unique(churn_analysis$churn_reason)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Monthly Charges Distribution by Churn Category    
ggplot(churn_analysis, aes(x = churn_category, y = monthly_charge, fill = churn_category)) +
  geom_boxplot() +
  labs(title = "Monthly Charges Distribution by Churn Category",
       x = "Churn Category",
       y = "Monthly Charges ($)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Churn Category by Contract Type
ggplot(churn_analysis, aes(x = contract, fill = churn_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Category by Contract Type",
       x = "Contract Type",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_brewer(palette = "set2")

# Filter data for only "Competitor" churn category to see the churn reasons
competitor_churn <- subset(churn_analysis, churn_category == "Competitor")

ggplot(competitor_churn, aes(x = churn_reason, fill = churn_reason)) +
  geom_bar() +
  labs(title = "Distribution of Churn Reasons for Competitor Category",
       x = "Churn Reason",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_manual(values = scales::hue_pal()(length(unique(competitor_churn$churn_reason)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Filter data for only "Attitude" churn category
attitude_churn <- subset(churn_analysis, churn_category == "Attitude")

ggplot(attitude_churn, aes(x = churn_reason, fill = churn_reason)) +
  geom_bar() +
  labs(title = "Distribution of Churn Reasons for Attitude Category",
       x = "Churn Reason",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_manual(values = scales::hue_pal()(length(unique(attitude_churn$churn_reason)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter data for only "Dissatisfaction" churn category
dissatisfaction_churn <- subset(churn_analysis, churn_category == "Dissatisfaction")

ggplot(dissatisfaction_churn, aes(x = churn_reason, fill = churn_reason)) +
  geom_bar() +
  labs(title = "Distribution of Churn Reasons for Dissatisfaction Category",
       x = "Churn Reason",
       y = "Count of Churned Customers") +
  theme_minimal() +
  scale_fill_manual(values = scales::hue_pal()(length(unique(dissatisfaction_churn$churn_reason)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Delve deep into R for EDA

#gender distribution
gender_distribution <- aggregate(customer_id ~ gender, data = churn_data, FUN = length)
colnames(gender_distribution) <- c("Gender", "Customer_Count")
print(gender_distribution)

# visualize
ggplot(gender_distribution, aes(x = Gender, y = Customer_Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Distribution of Customers",
       x = "Gender",
       y = "Number of Customers") +
  theme_minimal()
# the distribution of gender is almost same 

## churn rate by gender
churn_rate_gender <- aggregate(customer_id ~ gender + customer_status, data = churn_data, FUN = length)
colnames(churn_rate_gender) <- c("Gender", "Customer_Status", "Count")

total_by_gender <- tapply(churn_rate_gender$Count, churn_rate_gender$Gender, sum)
churn_rate_gender$Retention_Churn_Rate <- round((churn_rate_gender$Count / total_by_gender[churn_rate_gender$Gender]) * 100, 2)
print(churn_rate_gender)

# visualize
ggplot(churn_rate_gender, aes(x = Gender, y = Retention_Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Churn Rate by Gender",
       x = "Gender",
       y = "Churn Rate (%)") +
  theme_minimal()

# Revenue Analysis by Gender
revenue_by_gender <- aggregate(total_charges ~ gender, data = churn_data, FUN = sum)
colnames(revenue_by_gender) <- c("Gender", "Total_Revenue")
revenue_by_gender$Total_Customers <- tapply(churn_data$customer_id, churn_data$gender, length)
print(revenue_by_gender)

ggplot(revenue_by_gender, aes(x = Gender, y = Total_Revenue, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Revenue by Gender",
       x = "Gender",
       y = "Total Revenue") +
  theme_minimal()

# churn distribution 
ggplot(churn_rate_overall, aes(x = Customer_Status, y = Retention_Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Overall Churn Rate",
       x = "Customer Status",
       y = "Churn Rate (%)") +
  theme_minimal()

# Churn Rate by Contract Type

ggplot(churn_by_contract, aes(x = Contract_Type, y = Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Churn Rate by Contract Type",
       x = "Contract Type",
       y = "Churn Rate (%)") +
  theme_minimal()


# Customer Satisfaction Analysis
# Impact of Streaming Services on Churn
ggplot(churn_by_streaming, aes(x = Streaming_TV, y = Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Streaming TV on Churn",
       x = "Streaming TV Subscription",
       y = "Churn Rate (%)") +
  theme_minimal()

# Analysis of churn with  online security 
churn_by_security <- aggregate(customer_id ~ online_security + customer_status, data = churn_data, FUN = length)
colnames(churn_by_security) <- c("Online_Security", "Customer_Status", "Count")
total_by_security <- tapply(churn_by_security$Count, churn_by_security$Online_Security, sum)
churn_by_security$Churn_Rate <- round((churn_by_security$Count / total_by_security[churn_by_security$Online_Security]) * 100, 2)
print(churn_by_security)

ggplot(churn_by_security, aes(x = Online_Security, y = Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Churn Rate by Online Security",
       x = "Online Security",
       y = "Churn Rate (%)") +
  theme_minimal()

# Analysis of churn with tech support 
churn_by_techsupport <- aggregate(customer_id ~ premium_tech_support + customer_status, data = churn_data, FUN = length)
colnames(churn_by_techsupport) <- c("Tech_Support", "Customer_Status", "Count")
total_by_tech <- tapply(churn_by_techsupport$Count, churn_by_techsupport$Tech_Support, sum)
churn_by_techsupport$Churn_Rate <- round((churn_by_techsupport$Count / total_by_tech[churn_by_techsupport$Tech_Support]) * 100, 2)
print(churn_by_techsupport)

ggplot(churn_by_techsupport, aes(x = Tech_Support, y = Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Churn Rate by Tech Support",
       x = "Tech Support",
       y = "Churn Rate (%)") +
  theme_minimal()



#Churn rate by age group

df_balanced$age_group <- cut(df_balanced$age, breaks = c(18, 30, 45, 60, 85), 
                             labels = c("18-30", "31-45", "46-60", "61+"))

ggplot(df_balanced, aes(x = age_group, fill = customer_status)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Rate by Age Group", x = "Age Group", y = "Count") +
  theme_minimal()

# marriage analysis
ggplot(data, aes(x = married, fill = customer_status)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Rate by Marital Status", x = "Marital Status", y = "Count") +
  theme_minimal()

# number of dependate
ggplot(data, aes(x = number_of_dependents, fill = customer_status)) +
  geom_histogram(binwidth = 1, position = "stack", alpha = 0.7) +
  labs(title = "Churn Distribution by Number of Dependents", x = "Number of Dependents", y = "Count") +
  theme_minimal()


# refferal analysis
ggplot(data, aes(x = number_of_referrals, fill = customer_status)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Rate by Number of Referrals", x = "Number of Referrals", y = "Count") +
  theme_minimal()



# Customer segmentation
# Define customer segments based on monthly charges 
churn_data$Usage_Segment <- ifelse(churn_data$monthly_charge > 70, "High-Usage",
                                   ifelse(churn_data$monthly_charge >= 50, 
                                          "Medium-Usage", "Low-Usage"))

usage_segmentation <- aggregate(customer_id ~ Usage_Segment + customer_status, data = churn_data, FUN = length)
colnames(usage_segmentation) <- c("Usage_Segment", "Customer_Status", "Count")

total_by_usage <- tapply(usage_segmentation$Count, usage_segmentation$Usage_Segment, sum)
usage_segmentation$Churn_Rate <- round((usage_segmentation$Count / total_by_usage[usage_segmentation$Usage_Segment]) * 100, 2)
print(usage_segmentation)

ggplot(usage_segmentation, aes(x = Usage_Segment, y = Churn_Rate, fill = Customer_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Churn Rate by Usage Segment",
       x = "Usage Segment",
       y = "Churn Rate (%)") +
  theme_minimal()


# Most importantly calculate the customer life time value

churn_data$tenure_range <- cut(churn_data$tenure_in_months, 
                               breaks = c(-Inf, 12, 24, 36, Inf),
                               labels = c("0-12 months", "12-24 months", "24-36 months", "Over 36 months"))

# Aggregate CLV-related metrics
clv_analysis <- aggregate(cbind(tenure_in_months, monthly_charge, total_charges) ~ tenure_range, 
                          data = churn_data, 
                          FUN = function(x) round(mean(x, na.rm = TRUE), 2))

# Calculate total charges per tenure range
total_charges_per_group <- tapply(churn_data$total_charges, churn_data$tenure_range, sum, na.rm = TRUE)

# Compute CLV per customer (Total Charges / Number of Customers in Each Group)
clv_analysis$total_charges <- as.numeric(total_charges_per_group)
clv_analysis$clv_per_customer <- clv_analysis$total_charges / as.numeric(table(churn_data$tenure_range))
print(clv_analysis)


# formatted correctly for visualization
clv_plot_data <- data.frame(
  tenure_range = clv_analysis$tenure_range,  # Use tenure range directly
  clv_per_customer = as.numeric(clv_analysis$clv_per_customer))

# Bar chat for CLV
if (nrow(clv_plot_data) > 0) {
  ggplot(clv_plot_data, aes(x = tenure_range, y = clv_per_customer, fill = tenure_range)) +
    geom_bar(stat = "identity") +
    labs(title = "Customer Lifetime Value (CLV) by Tenure Range",
         x = "Tenure Range",
         y = "CLV per Customer") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
} else {
  print("Error: CLV data is empty, check data processing steps.")
}


# Line Chart for CLV Trends Over Tenure Ranges
if (nrow(clv_plot_data) > 0) {
  ggplot(clv_plot_data, aes(x = tenure_range, y = clv_per_customer, group = 1)) +
    geom_line(color = "green", size = 1) +
    geom_point(color = "red", size = 3) +
    labs(title = "Trend of Customer Lifetime Value (CLV) by Tenure",
         x = "Tenure Range",
         y = "CLV per Customer") +
    theme_minimal()
} else {
  print("Error: CLV data is empty, check data processing steps.")
}



dbDisconnect(conn)
