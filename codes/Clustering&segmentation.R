#install.packages(c("factoextra", "dplyr", "RMySQL", "ggplot2", "cluster", "ggfortify", "reshape2", "fastDummies", "fpc", "NbClust", "plotly"), dependencies = TRUE)
start_time <- Sys.time()


library(factoextra)
library(dplyr)
library(RMySQL)
library(ggplot2)
library(cluster)
library(ggfortify)
library(reshape2)
library(fastDummies)
library(fpc)
library(NbClust)
library(plotly)



# Connect to MySQL Database
conn <- dbConnect(MySQL(),
                  dbname = "sql_project",
                  host = "localhost",
                  user = "root",  
                  password = "")

dbListFields(conn, "telecom_customers")
churn_data <- dbGetQuery(conn, "SELECT * FROM telecom_customers;")
dbDisconnect(conn)


# Merge "Joined" into "Stayed"
churn_data$customer_status <- ifelse(churn_data$customer_status == "Joined", "Stayed", churn_data$customer_status)

churn_data$customer_status

# Keeping Original Data Intact
churn_processed <- churn_data


# Droping Unnecessary Columns
churn_processed <- churn_processed %>% select(-customer_id, -city, -churn_reason, -churn_category, -customer_status)



# One-Hot Encoding for Categorical Features
cat_features <- c("offer", "internet_type", "contract", "payment_method",
                  "gender", "married", "phone_service", "multiple_lines",
                  "internet_service", "online_security", "online_backup",
                  "device_protection_plan", "premium_tech_support",
                  "streaming_tv", "streaming_movies", "streaming_music",
                  "unlimited_data", "paperless_billing")

churn_processed <- dummy_cols(churn_processed, select_columns = cat_features, 
                              remove_first_dummy = TRUE, remove_selected_columns = TRUE)

num_features <- c("tenure_in_months", "monthly_charge", "total_charges", 
                  "avg_monthly_long_distance_charges", "avg_monthly_gb_download", 
                  "total_long_distance_charges", "total_revenue")

# check for outliers in numerical data
boxplot(churn_processed[num_features], 
        main = "Outlier Detection", col = c("blue", "red", "green"))

# Scaling Numerical Features
churn_processed[num_features] <- scale(churn_processed[num_features])

colnames(churn_processed)

# Visualize outliers after scaling
boxplot(churn_processed[num_features], 
        main = "Outlier Detection After scaling", col = c("blue", "red", "green"))


# Handling Outliers Using Winsorization (Capping)
cap_outliers <- function(x) {
  q1 <- quantile(x, 0.01, na.rm = TRUE)
  q3 <- quantile(x, 0.99, na.rm = TRUE)
  x[x < q1] <- q1
  x[x > q3] <- q3
  return(x)
}

# Apply Winsorization to numeric features
churn_processed[num_features] <- lapply(churn_processed[num_features], cap_outliers)

# Visualize outliers after treatment
boxplot(churn_processed[num_features], 
        main = "Outlier Detection After Winsorization", col = c("blue", "red", "green"))


colnames(churn_processed)

#checking all is numeric
is_numeric <- sapply(churn_processed, is.numeric)
print(all(is_numeric))  


# Apply PCA for dimensionality Reduction
pca_result <- prcomp(churn_processed, center = TRUE, scale. = TRUE)

# Scree Plot to decide the number of components to keep
fviz_eig(pca_result)

# Select Components Retaining 90% Variance
var_explained <- summary(pca_result)$importance[2,]
cumulative_variance <- cumsum(var_explained) * 100
num_components <- min(which(cumulative_variance >= 90))
pca_data <- as.data.frame(pca_result$x[, 1:num_components])

summary(pca_data)


# Select number of components dynamically
num_components <- min(which(cumulative_variance >= 90))  # First component reaching 90%
print(paste("Number of components selected:", num_components))


# Visualize PCA Variance Importance
var_explained_25 <- var_explained[1:25] * 100
pc_numbers <- 1:25
scree_data <- data.frame(PC = factor(pc_numbers), Variance = var_explained_25)

ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_point(size = 3, color = "black") +
  geom_line(group = 1, color = "black") +
  labs(title = "Variance Importance of First 25 PCA Components",
       x = "Principal Components",
       y = "Percentage of Explained Variance") +
  theme_minimal()


# Determine Optimal Clusters Using Multiple Methods
set.seed(40)
fviz_nbclust(pca_data, kmeans, method = "wss")  # Elbow Method gave 3
#fviz_nbclust(pca_data, kmeans, method = "silhouette")
#fviz_nbclust(pca_data, pam, method = "silhouette") # Silhouette Method with pam
#gap_stat <- clusGap(pca_data, FUN = pam, K.max = 10, B = 500)
#fviz_gap_stat(gap_stat) 

#3library(factoextra)
#fviz_nbclust(pca_data, pam, method = "silhouette") +
#  labs(title = "Silhouette Method for Optimal K (PAM Clustering)",
#       x = "Number of Clusters (K)",
#       y = "Average Silhouette Width")


#clus_start <- Sys.time()

# Apply K-Medoids Clustering (PAM) with Optimal K=3
optimal_k <- 3
pam_model <- pam(pca_data, k = optimal_k)
pca_data$cluster <- as.factor(pam_model$clustering)

#clus_end <- Sys.time()
#print(paste("Clustering Execution Time:", round(clus_end - clus_start, 2), "seconds"))


# Merge Cluster Labels Back to Original Data
churn_data$cluster <- pca_data$cluster

# Compute Confusion Matrix
confusion_matrix <- table(churn_data$cluster, churn_data$customer_status)
print("Confusion Matrix:")
print(confusion_matrix)

# Convert confusion matrix to a dataframe for visualization
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Confusion Matrix Heatmap
ggplot(confusion_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Heatmap (Cluster vs. Churn Status)",
       x = "Actual Customer Status",
       y = "Predicted Cluster",
       fill = "Count") +
  theme_minimal()


# Compute Precision, Recall, and Jaccard Index
churn_clusters <- which(rowMeans(confusion_matrix) > 0.5)
TP <- sum(confusion_matrix[churn_clusters, "Churned"])
FP <- sum(confusion_matrix[churn_clusters, "Stayed"])
FN <- sum(confusion_matrix[-churn_clusters, "Churned"])

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
jaccard <- TP / (TP + FP + FN)

print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("Jaccard Index:", round(jaccard, 4)))


# Compute Overall Purity
overall_purity <- sum(apply(confusion_matrix, 1, max)) / sum(confusion_matrix)
print(paste("Overall Cluster Purity:", round(overall_purity, 4)))

# Visualizations
fviz_cluster(list(data = pca_data[, 1:2], cluster = pam_model$clustering),
             geom = "point", ellipse.type = "norm",
             palette = "Set2", ggtheme = theme_minimal())

p <- plot_ly(pca_data, 
             x = ~PC1, y = ~PC2, z = ~PC3, 
             color = ~as.factor(pam_model$clustering), 
             colors = c("red", "blue"), 
             marker = list(size = 5, opacity = 0.8)) %>%
  add_markers() %>%
  layout(title = "3D PCA Visualization of K-Medoids Clusters",
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))
p


aggregate(churn_processed, by = list(cluster = pca_data$cluster), FUN = mean)


table(pca_data$cluster, churn_data$customer_status)

# segmentation analysis

#Plot Churn Distribution by Cluster
ggplot(churn_data, aes(x = cluster, fill = customer_status)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Churn Distribution Across Clusters",
       x = "Cluster",
       y = "Proportion of Customers",
       fill = "Customer Status") +
  theme_minimal()


#  Behavioral Analysis (Influence of Payment Method on Churn)
ggplot(churn_data, aes(x = cluster, fill = payment_method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("blue", "red", "green", "purple")) +
  labs(title = "Payment Method Distribution Across Clusters",
       x = "Cluster",
       y = "Proportion",
       fill = "Payment Method") +
  theme_minimal()

# Segmenting High-Risk vs Low-Risk Churn Customers
# Assign churn risk levels
churn_data <- churn_data %>%
  mutate(churn_risk = case_when(
    cluster == "1" & customer_status == "Churned" ~ "High Risk",
    cluster == "2" & customer_status == "Churned" ~ "Moderate Risk",
    cluster == "3" ~ "Low Risk",
    TRUE ~ "Low Risk"
  ))

# Churn Risk by Cluster
ggplot(churn_data, aes(x = cluster, fill = churn_risk)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("red", "orange", "blue")) +
  labs(title = "Churn Risk Segmentation by Cluster",
       x = "Cluster",
       y = "Proportion",
       fill = "Churn Risk") +
  theme_minimal()


# Create tenure groups
churn_data$tenure_range <- cut(churn_data$tenure_in_months, 
                               breaks = c(-Inf, 12, 24, 36, Inf),
                               labels = c("0-12 months", "12-24 months", "24-36 months", "Over 36 months"))

# Aggregate CLV-related metrics by tenure
clv_analysis <- churn_data %>%
  group_by(tenure_range) %>%
  summarise(
    avg_tenure = round(mean(tenure_in_months, na.rm = TRUE), 2),
    avg_monthly_charge = round(mean(monthly_charge, na.rm = TRUE), 2),
    total_charges = sum(total_charges, na.rm = TRUE),
    num_customers = n()
  )

# Compute CLV per customer (Total Revenue / Number of Customers in Each Group)
clv_analysis$clv_per_customer <- clv_analysis$total_charges / clv_analysis$num_customers

# Print CLV summary
print(clv_analysis)

clv_plot_data <- data.frame(
  tenure_range = clv_analysis$tenure_range,  
  clv_per_customer = as.numeric(clv_analysis$clv_per_customer)
)
ggplot(clv_plot_data, aes(x = tenure_range, y = clv_per_customer, fill = tenure_range)) +
  geom_bar(stat = "identity") +
  labs(title = "Customer Lifetime Value (CLV) by Tenure Range",
       x = "Tenure Range",
       y = "CLV per Customer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability


# Aggregate CLV metrics by cluster
clv_by_cluster <- churn_data %>%
  group_by(cluster) %>%
  summarise(
    avg_tenure = round(mean(tenure_in_months, na.rm = TRUE), 2),
    avg_monthly_charge = round(mean(monthly_charge, na.rm = TRUE), 2),
    total_revenue = sum(total_charges, na.rm = TRUE),
    num_customers = n()
  )

# Compute CLV per customer in each cluster
clv_by_cluster$clv_per_customer <- clv_by_cluster$total_revenue / clv_by_cluster$num_customers

# Print CLV per cluster
print(clv_by_cluster)

ggplot(clv_by_cluster, aes(x = factor(cluster), y = clv_per_customer, fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Customer Lifetime Value (CLV) by Cluster",
       x = "Cluster",
       y = "CLV per Customer") +
  theme_minimal()



# see churn reason and categories

# cluster 1 
cluster1_churned <- churn_data %>%
  filter(cluster == 1 & customer_status == "Churned") %>%
  group_by(churn_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
   
# Plot for Churn Reasons in Cluster-1
ggplot(cluster1_churned, aes(x = reorder(churn_category, count), y = count, fill = churn_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Major Churn Reasons for Cluster-1 Customers",
       x = "Churn Category",
       y = "Number of Customers") +
  theme_minimal()

# cluster 2
cluster2_churned <- churn_data %>%
  filter(cluster == 2 & customer_status == "Churned") %>%
  group_by(churn_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot for Churn Reasons in Cluster-2
ggplot(cluster2_churned, aes(x = reorder(churn_category, count), y = count, fill = churn_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Major Churn Reasons for Cluster-2 Customers",
       x = "Churn Category",
       y = "Number of Customers") +
  theme_minimal()


# deep dive into the competitor-related churn reasons
# Filter churn data for customers who churned due to competitor reasons in Clusters 1 and 2
competitor_churn <- churn_data %>%
  filter(cluster %in% c(1, 2) & churn_category == "Competitor") %>%
  group_by(cluster, churn_reason) %>%
  summarise(count = n(), .groups = "drop")

# Visualize competitor-specific churn reasons for Cluster 1 & 2
ggplot(competitor_churn, aes(x = reorder(churn_reason, -count), y = count, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Competitor-Specific Churn Reasons Across Clusters",
       x = "Churn Reason",
       y = "Number of Customers",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability




# Business deliverables and retention incenties 

#  Code to Categorize Retention Incentives
# Assign retention offers based on tenure and CLV

# Extract high-risk churned customers (Cluster 1)
high_risk_customers <- churn_data %>%
  filter(cluster == "1", customer_status == "Churned")
# Assign retention offers based on tenure
high_risk_customers$retention_strategy <- ifelse(
  high_risk_customers$tenure_in_months < 12, "Discount on First Year Renewal",
  ifelse(high_risk_customers$tenure_in_months < 24, "Exclusive Loyalty Benefits",
         "Premium Support + Discounts"))

# table of retention strategies
table(high_risk_customers$retention_strategy)

# Plot
ggplot(high_risk_customers, aes(x = retention_strategy, fill = retention_strategy)) +
  geom_bar() +
  labs(title = "Retention Strategies for High-Risk Churn Customers (Cluster 1)",
       x = "Retention Strategy",
       y = "Number of Customers") +
  theme_minimal()
 
# Select high CLV customers from Cluster 2
high_value_customers <- churn_data %>%
  filter(cluster == 2) %>%
  arrange(desc(total_revenue))  # Sort by highest revenue

# View High-Value Customers
head(high_value_customers, 10)

# Create VIP Engagement Strategy for Cluster 2
# Assign VIP perks based on CLV and tenure
high_value_customers$vip_program <- ifelse(
  high_value_customers$total_revenue > 5000, "Exclusive VIP Rewards",
  ifelse(high_value_customers$total_revenue > 3000, "Premium Benefits", "Loyalty Discounts")
)

# View Engagement Offers
table(high_value_customers$vip_program)



# Filter customers in Cluster 1 who are likely to churn to run communications to them
high_risk_customers <- churn_data %>%
  filter(cluster == 1 & customer_status == "Churned")

# Display summary statistics
summary(high_risk_customers)

# View a few high-risk churn customers
head(high_risk_customers)



end_time <- Sys.time()
execution_time <- end_time - start_time
print(paste("Total Execution Time:", round(execution_time, 2), "seconds"))

