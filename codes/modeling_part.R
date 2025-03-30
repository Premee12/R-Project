
R.version.string # r version
RStudio.Version()$version
Sys.which("make")


#install.packages("reshape2") 
#install.packages("factoextra", dependencies = TRUE)
library(factoextra)
#install.packages("dplyr", dependencies = TRUE)
library(dplyr)
library(RMySQL)
library(ggplot2)
library(cluster)
library(ggfortify)   
library(reshape2)



conn <- dbConnect(MySQL(),
                  dbname = "sql_project",
                  host = "localhost",
                  user = "root",  
                  password = "")

dbListFields(conn, "telecom_customers") # columns view 

churn_data <- dbGetQuery(conn, "SELECT * FROM telecom_customers;")

dbDisconnect(conn)


# Convert Categorical Variables to Numeric**
cat_features <- c("offer", "internet_type", "contract", "payment_method",
                    "gender", "married", "phone_service", "multiple_lines",
                    "internet_service", "online_security", "online_backup",
                    "device_protection_plan", "premium_tech_support",
                    "streaming_tv", "streaming_movies", "streaming_music",
                    "unlimited_data", "paperless_billing")

# Convert categorical columns to factors, then to numeric
churn_data[cat_features] <- lapply(churn_data[cat_features], function(x) as.numeric(as.factor(x)))

# Print summary of categorical variables after encoding
summary(churn_data[cat_features])

churn_data[cat_features]

# Print first few rows of selected columns
print(head(churn_data[, c("offer", "internet_type", "contract", "payment_method")]))


# check for outliers in numerical data
boxplot(churn_data[, c("tenure_in_months", "monthly_charge", "total_charges", 
                       "avg_monthly_long_distance_charges", "avg_monthly_gb_download", 
                       "total_long_distance_charges", "total_revenue")], 
        main = "Outlier Detection", col = c("blue", "red", "green"))

num_features <- c("tenure_in_months", "monthly_charge", "total_charges", 
                  "avg_monthly_long_distance_charges", "avg_monthly_gb_download", 
                  "total_long_distance_charges", "total_revenue")

# Handling Outliers Using Winsorization (Capping)
cap_outliers <- function(x) {
  q1 <- quantile(x, 0.01, na.rm = TRUE)
  q3 <- quantile(x, 0.99, na.rm = TRUE)
  x[x < q1] <- q1
  x[x > q3] <- q3
  return(x)
}

# Apply Winsorization to numeric features
churn_data[num_features] <- lapply(churn_data[num_features], cap_outliers)

# Visualize outliers after treatment
boxplot(churn_data[num_features], 
        main = "Outlier Detection After Winsorization", col = c("blue", "red", "green"))



# Normalize Using Min-Max Scaling**
min_max_scaling <- function(x) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Normalize numerical features
all_features <- c(num_features, cat_features)
churn_data[num_features] <- lapply(churn_data[num_features], min_max_scaling)

# Kept categorical features as numerical but without normalization
#churn_data[cat_features] <- lapply(churn_data[cat_features], function(x) as.numeric(as.factor(x)))

# Print summary after preprocessing
summary(churn_data[all_features])


# apply PCA for Dimensionality Reduction
pca_result <- prcomp(churn_data[all_features], center = TRUE, scale. = TRUE)

# Print variance explained by each principal component
summary(pca_result)

# Scree Plot to decide the number of components to keep
fviz_eig(pca_result)


# Check feature importance in PCA
pca_loadings <- as.data.frame(pca_result$rotation[, 1:3])  # Top 3 PCs
pca_loadings$Feature <- rownames(pca_result$rotation)

# Reshape for visualization
library(reshape2)
pca_melted <- melt(pca_loadings, id.vars = "Feature")

# Plot feature contributions
ggplot(pca_melted, aes(x = Feature, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "PCA Feature Contributions", x = "Feature", y = "Loading Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Extract first 15 PCA components instead of 10
pca_data <- as.data.frame(pca_result$x[, 1:15])  # Keeping first 15 components


# Adding back customer_id (if needed for reference)
# pca_data$customer_id <- churn_data$customer_id

# Print first few rows of PCA-transformed data
head(pca_data)




# Determine Optimal Clusters Using Elbow Method**
set.seed(42)
wcss <- numeric()

for (k in 1:10) {
  kmeans_result <- kmeans(pca_data[, 1:15], centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}

# Plot Elbow Curve
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, col = "red",
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal k")


# Apply K-Means with Optimal K=4
set.seed(42)
optimal_k <- 4  # Assume elbow method suggests 4 clusters
kmeans_model <- kmeans(pca_data[, 1:15], centers = optimal_k, nstart = 25)


# Add cluster labels to the dataset and print 
pca_data$cluster <- as.factor(kmeans_model$cluster)
print(table(pca_data$cluster))


# 2D PCA Plot with Clusters
fviz_cluster(list(data = pca_data[, 1:15], cluster = kmeans_model$cluster),
             geom = "point",
             ellipse.type = "norm",
             palette = "Set2",
             ggtheme = theme_minimal())


# Add cluster labels to the dataset
churn_data$cluster <- as.factor(kmeans_model$cluster)
print(table(churn_data$cluster))

# Visualize Clustering with PCA
# 2D PCA Plot with Clusters
fviz_cluster(list(data = pca_data[, 1:15], cluster = kmeans_model$cluster),
               geom = "point",
               ellipse.type = "norm",
               palette = "Set2",
               ggtheme = theme_minimal())



library(plotly)
# Create a 3D Scatter Plot using the first three Principal Components
p <- plot_ly(pca_data, 
             x = ~PC1, y = ~PC2, z = ~PC3, 
             color = ~as.factor(kmeans_model$cluster), 
             colors = c("red", "blue", "green", "purple"), 
             marker = list(size = 5, opacity = 0.8)) %>%
  add_markers() %>%
  layout(title = "3D PCA Visualization of K-Means Clusters",
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))
p



library(cluster)

# Compute Gower distance (better for mixed categorical & numerical data)
gower_dist <- daisy(churn_data[all_features], metric = "gower")

# Run K-Medoids (Similar to K-Means but uses Gower distance)
kmedoids_model <- pam(gower_dist, k = 4)  # Test with K=4
churn_data$cluster <- as.factor(kmedoids_model$clustering)

# Check cluster purity again
table(churn_data$cluster, churn_data$customer_status)



# Create a table of cluster vs actual customer status
cluster_purity <- table(churn_data$cluster, churn_data$customer_status)

# Calculate purity for each cluster
cluster_purity_score <- apply(cluster_purity, 1, function(x) max(x) / sum(x))

# Calculate overall purity
overall_purity <- sum(apply(cluster_purity, 1, max)) / sum(cluster_purity)
print(paste("Overall Cluster Purity:", round(overall_purity, 4)))



# Calculate Cluster Entropy
# Function to calculate entropy
calculate_entropy <- function(cluster_counts) {
  proportions <- cluster_counts / sum(cluster_counts)
  entropy <- -sum(proportions * log2(proportions + 1e-10))  # Adding small value to avoid log(0)
  return(entropy)
}

# Compute entropy for each cluster
cluster_entropy_scores <- apply(cluster_purity, 1, calculate_entropy)

# Compute overall entropy (weighted average)
overall_entropy <- sum(cluster_entropy_scores * rowSums(cluster_purity)) / sum(cluster_purity)
print(paste("Overall Cluster Entropy:", round(overall_entropy, 4)))



# Created a contingency table of cluster assignments vs. actual labels
confusion_matrix <- table(churn_data$cluster, churn_data$customer_status)
print("Confusion Matrix:")
print(confusion_matrix)

# Convert confusion matrix to a dataframe for ggplot
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Plot the heatmap
ggplot(confusion_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Actual Customer Status",
       y = "Cluster",
       fill = "Count") +
  theme_minimal()



# Compute Precision for each cluster
precision_scores <- apply(confusion_matrix, 1, function(x) max(x) / sum(x))

# Compute Recall for each actual class
recall_scores <- apply(confusion_matrix, 2, function(x) max(x) / sum(x))

# Compute Jaccard Index for each cluster
jaccard_index <- apply(confusion_matrix, 1, function(x) {
  max_count <- max(x)
  total_count <- sum(x)
  return(max_count / (total_count + sum(x) - max_count))
})

# Compute Overall Precision, Recall, and Jaccard Index
overall_precision <- sum(apply(confusion_matrix, 1, max)) / sum(confusion_matrix)
overall_recall <- sum(apply(confusion_matrix, 2, max)) / sum(confusion_matrix)
overall_jaccard <- mean(jaccard_index)  # Taking the average across clusters

# Print Results
print(paste("Overall Precision:", round(overall_precision, 4)))
print(paste("Overall Recall:", round(overall_recall, 4)))
print(paste("Overall Jaccard Index:", round(overall_jaccard, 4)))














library(reshape2)
# Add cluster labels back to original data
churn_data$cluster <- pca_data$cluster

# Select key numerical features for analysis
num_features <- c("tenure_in_months", "monthly_charge", "total_charges", 
                  "avg_monthly_long_distance_charges", "avg_monthly_gb_download", 
                  "total_long_distance_charges", "total_revenue")

# Melt data for visualization
melted_data <- melt(churn_data[, c(num_features, "cluster")], id.vars = "cluster")

# Boxplot to compare numerical feature distributions
ggplot(melted_data, aes(x = as.factor(cluster), y = value, fill = as.factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Numerical Feature Distributions Across Clusters",
       x = "Cluster",
       y = "Value") +
  theme_minimal()


# Analyze Categorical Features by Cluster

# Melt data for visualization
melted_cat_data <- melt(churn_data[, c(cat_features, "cluster")], id.vars = "cluster")

# Bar plot for categorical features
ggplot(melted_cat_data, aes(x = value, fill = as.factor(cluster))) +
  geom_bar(position = "dodge") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Categorical Feature Distribution Across Clusters",
       x = "Feature Value",
       y = "Count",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare Churn Behavior by Cluster
ggplot(churn_data, aes(x = as.factor(cluster), fill = customer_status)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Cluster",
       x = "Cluster",
       y = "Proportion of Customers") +
  theme_minimal()



# Cluster Distribution


# Code to Analyze Churn Distribution by Cluster
cluster_churn_distribution <- aggregate(customer_status ~ cluster, data = churn_data, table)
print(cluster_churn_distribution)



# Plot cluster-wise churn distribution
ggplot(churn_data, aes(x = factor(cluster), fill = customer_status)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Distribution by Cluster",
       x = "Cluster",
       y = "Proportion of Customers") + theme_minimal()



churn_data













#install.packages("randomForest", dependencies = TRUE)





conn <- dbConnect(MySQL(),
                  dbname = "sql_project",
                  host = "localhost",
                  user = "root",  
                  password = "temidayo")

dbListFields(conn, "telecom_customers") # columns view 

churn_data <- dbGetQuery(conn, "SELECT * FROM telecom_customers;")

dbDisconnect(conn)

# Preprocessing Key Features Before K-Means Clustering


# Convert categorical variables into factors
churn_data$offer <- as.factor(churn_data$offer)
churn_data$internet_type <- as.factor(churn_data$internet_type)
churn_data$contract <- as.factor(churn_data$contract)
churn_data$payment_method <- as.factor(churn_data$payment_method)

# Convert factors to numeric encoding
churn_data$offer <- as.numeric(churn_data$offer)
churn_data$internet_type <- as.numeric(churn_data$internet_type)
churn_data$contract <- as.numeric(churn_data$contract)
churn_data$payment_method <- as.numeric(churn_data$payment_method)

# Print first few rows of selected columns
print(head(churn_data[, c("offer", "internet_type", "contract", "payment_method")]))

# Print summary statistics for these columns
summary(churn_data[, c("offer", "internet_type", "contract", "payment_method")])

cat_features <- c("offer", "internet_type", "contract", "payment_method")


# check for outliers in numerical data
boxplot(churn_data[, c("monthly_charge", "total_charges", "total_revenue")], 
        main = "Outlier Detection", col = c("blue", "red", "green"))

# Handling Outliers Using Winsorization (Capping)
cap_outliers <- function(x) {
  q1 <- quantile(x, 0.01, na.rm = TRUE)
  q3 <- quantile(x, 0.99, na.rm = TRUE)
  x[x < q1] <- q1
  x[x > q3] <- q3
  return(x)
}
# Apply to key numeric columns
churn_data$monthly_charge <- cap_outliers(churn_data$monthly_charge)
churn_data$total_charges <- cap_outliers(churn_data$total_charges)
churn_data$total_revenue <- cap_outliers(churn_data$total_revenue)

# Normalize Numerical Features for k means

num_features <- c("tenure_in_months", "monthly_charge", "total_charges", 
                  "avg_monthly_long_distance_charges", "avg_monthly_gb_download", 
                  "total_long_distance_charges", "total_revenue")

# Normalize using Min-Max Scaling
min_max_scaling <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
# Apply Min-Max Scaling
churn_data[num_features] <- lapply(churn_data[num_features], min_max_scaling)
churn_data[num_features] 

# Print summary after preprocessing
summary(churn_data[, c("tenure_in_months", "monthly_charge", "total_charges", 
                       "offer", "internet_type", "contract", "payment_method")])


# Find Optimal Clusters using Elbow Method

set.seed(42)
wcss <- numeric()

for (k in 1:10) {
  kmeans_result <- kmeans(churn_data[, c(num_features, cat_features)], centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}

# Plot Elbow Curve
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, col = "red",
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal k")


# Apply K-Means with Optimal K=4
set.seed(42)
optimal_k <- 4  # Assume elbow method suggests 4 clusters
kmeans_model <- kmeans(churn_data[, c(num_features, cat_features)], 
                       centers = optimal_k, nstart = 25)

# Add cluster labels to the dataset
churn_data$cluster <- as.factor(kmeans_model$cluster)
print(table(churn_data$cluster))




library(dplyr)

# Create a table of cluster assignments vs actual customer status
cluster_purity <- table(churn_data$cluster, churn_data$customer_status)

# Calculate purity for each cluster
cluster_purity_score <- apply(cluster_purity, 1, function(x) max(x) / sum(x))

# Calculate overall purity
overall_purity <- sum(apply(cluster_purity, 1, max)) / sum(cluster_purity)
print(paste("Overall Cluster Purity:", round(overall_purity, 4)))



# Function to calculate entropy
calculate_entropy <- function(cluster_counts) {
  proportions <- cluster_counts / sum(cluster_counts)
  entropy <- -sum(proportions * log2(proportions + 1e-10))  # Avoid log(0)
  return(entropy)
}

# Compute entropy for each cluster
cluster_entropy_scores <- apply(cluster_purity, 1, calculate_entropy)

# Compute overall entropy (weighted average)
overall_entropy <- sum(cluster_entropy_scores * rowSums(cluster_purity)) / sum(cluster_purity)
print(paste("Overall Cluster Entropy:", round(overall_entropy, 4)))




# Cluster Distribution


# Code to Analyze Churn Distribution by Cluster
cluster_churn_distribution <- aggregate(customer_status ~ cluster, data = churn_data, table)
print(cluster_churn_distribution)



# Plot cluster-wise churn distribution
ggplot(churn_data, aes(x = factor(cluster), fill = customer_status)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Distribution by Cluster",
       x = "Cluster",
       y = "Proportion of Customers") + theme_minimal()

# Deep Dive into Churn Reasons for Clusters 3 & 4
# Filter high-risk clusters (3 & 4) and keep only churned customers
high_risk_clusters <- churn_data[churn_data$cluster %in% c(3, 4) & churn_data$customer_status == "Churned", ]

# Plot Churn Reasons by Cluster (3 & 4)
ggplot(high_risk_clusters, aes(x = churn_category, fill = factor(cluster))) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Reason Category for High-Risk Clusters (3 & 4)",
       x = "Churn Reason",
       y = "Count of Customers",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# payment method
ggplot(high_risk_clusters, aes(x = payment_method, fill = factor(cluster))) +
  geom_bar(position = "fill") +
  labs(title = "Payment Methods for High-Risk Clusters",
       x = "Payment Method",
       y = "Proportion of Customers") +
  theme_minimal()




print(churn_data)



# classification


# Check Feature Correlation
library(reshape2)

# Select only numeric columns for correlation analysis
numeric_features <- churn_data[, sapply(churn_data, is.numeric)]

# Compute correlation matrix
correlation_matrix <- cor(numeric_features, use = "pairwise.complete.obs")

correlation_melted <- melt(correlation_matrix)
ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Feature Correlation Heatmap", x = "Features", y = "Features")


# Identify highly correlated feature pairs (threshold > 0.8)
high_corr_pairs <- which(abs(correlation_matrix) > 0.8 & abs(correlation_matrix) < 1, arr.ind = TRUE)

# Print feature pairs with high correlation
print(high_corr_pairs)



# Drop redundant and unnecessary features
drop_columns <- c("latitude", "longitude", "tenure_in_months", "total_charges", 
                  "churn_category", "number_of_referrals", "customer_id")

# Remove the selected columns from churn_data
churn_data <- churn_data[, !names(churn_data) %in% drop_columns]


# Create a dataframe showing column names and data types
column_info <- data.frame(
  Column = names(churn_data),
  Data_Type = sapply(churn_data, class))
print(column_info)


# Check for missing values in each column
colSums(is.na(churn_data))



# Create a new dataframe to work with
df <- churn_data

# Drop irrelevant columns
df <- df[, !names(df) %in% c("customer_id", "married", "city", "zip_code", "latitude", "longitude", "churn_reason")]

# Display remaining columns
colnames(df)


# Create a dataframe showing column names and data types
column_info <- data.frame(
  Column = names(df),
  Data_Type = sapply(df, class))
print(column_info)


# Define categorical columns to encode
categorical_cols <- c('gender', 'phone_service', 'multiple_lines', 'internet_service',
                      'online_security', 'online_backup', 'device_protection_plan',
                      'premium_tech_support', 'streaming_tv', 'streaming_movies',
                      'streaming_music', 'unlimited_data', 'paperless_billing')


# Check structure to confirm categorical columns exist
str(df[categorical_cols])

# Convert categorical variables to dummy variables
data_dummies <- model.matrix(~ . -1, data = df[, categorical_cols])  # Corrected argument

# Convert matrix to dataframe
data_dummies <- as.data.frame(data_dummies)  # Fixed incorrect function call

# Merge dummy variables back to `df`, excluding original categorical columns
df <- cbind(df[, !names(df) %in% categorical_cols], data_dummies)

# Display updated dataset structure
str(df)



# perform feature importance with random forest
library(randomForest)

# Ensure target variable is a factor (classification problem)
df$customer_status <- as.factor(df$customer_status)

# Define predictor variables (excluding target)
predictors <- df[, !names(df) %in% c("customer_status")]

# Train Random Forest model
set.seed(123)
rf_model <- randomForest(x = predictors, y = df$customer_status, importance = TRUE, ntree = 100)

# Print model summary
print(rf_model)


# extract feature importance
# Extract feature importance
importance_df <- data.frame(Feature = rownames(rf_model$importance),
                            Importance = rf_model$importance[, 1])

# Sort features by importance
importance_df <- importance_df[order(-importance_df$Importance), ]

# Display top features
print(head(importance_df, 10))  # Show top 10 most important features



# visualize feature importance

# Plot Feature Importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance - Random Forest",
       x = "Feature",
       y = "Importance") +
  theme_minimal()




# Identify features with very low importance
low_importance_features <- importance_df$Feature[importance_df$Importance < 0.011]

# Drop low-importance features
df <- df[, !names(df) %in% low_importance_features]

# Display updated dataset structure
str(df)


#next step is churn prediction using esemble method LightGBM

# Install and Load Required Libraries
#install.packages("lightgbm", repos = "https://cran.r-project.org", dependencies = TRUE)
library(lightgbm)

#install.packages("caret")
library(caret)
library(pROC)


#df$customer_status <- as.factor(df$customer_status)  # Target variable
df$customer_status <- as.integer(factor(df$customer_status, levels = c("Stayed", "Churned", "Joined")))


# unique values in cluster 
unique(df$customer_status)


# Splitting the Data into Training and Testing Sets
set.seed(42)
train_index <- createDataPartition(df$customer_status, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Convert Data into LightGBM Format
train_matrix <- lgb.Dataset(data = as.matrix(train_data[, -ncol(train_data)]),
                            label = as.numeric(train_data$customer_status) - 1)  # Convert factor to numeric (0,1)
test_matrix <- as.matrix(test_data[, -ncol(test_data)])

test_data
train_data

unique(train_data$customer_status)

# Set LightGBM Parameters (Inspired by Python Code)
params <- list(
  objective = "binary",
  metric = "auc",
  n_estimators = 661,
  max_depth = 240,
  learning_rate = 0.0549,
  reg_alpha = 0.0053,
  reg_lambda = 0.2593,
  min_child_weight = 0.7581,
  min_child_samples = 17,
  subsample = 0.7232,
  subsample_freq = 1,
  colsample_bytree = 0.4522,
  num_leaves = 204,
  verbosity = -1
)

# Train LightGBM Model
lgbm_model <- lgb.train(params = params, data = train_matrix, nrounds = 100)

# Make Predictions
predictions <- predict(lgbm_model, test_matrix)

# Convert Predictions to Binary (Threshold = 0.7)
predictions_binary <- ifelse(predictions > 0.7, 1, 0)

# Evaluate Model Performance
roc_auc <- roc(as.numeric(test_data$customer_status) - 1, predictions)
auc_value <- auc(roc_auc)
print(paste("LGBM ROC AUC:", round(auc_value, 4)))

# Classification Report
confusion_matrix <- table(Predicted = predictions_binary, Actual = as.numeric(test_data$customer_status) - 1)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate Precision, Recall, and F1-score
classification_metrics <- confusionMatrix(as.factor(predictions_binary), as.factor(as.numeric(test_data$customer_status) - 1))
print(classification_metrics)



# from the metrics seen the data was affected by class imbalance
# Using SMOTE in R

#check for categorical data before the applying smote 
categorical_cols <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]
print(categorical_cols)

for (col in categorical_cols) {
  df[[col]] <- as.numeric(as.factor(df[[col]]))
}

df

# Create a dataframe to see column names and data types
column_info <- data.frame(
  Column = names(df),
  Data_Type = sapply(df, class))
print(column_info)



#install.packages("UBL")
library(UBL)


# Identify Class 3 (Joined) samples
class_3_samples <- df[df$customer_status == 3, ]

# Duplicate Class 3 samples multiple times
oversampled_class_3 <- class_3_samples[rep(seq_len(nrow(class_3_samples)), each = 2), ]

# Combine the original dataset with the oversampled Class 3
df_balanced <- rbind(df, oversampled_class_3)

# Check new class distribution
table(df_balanced$customer_status)

df


# Train and Evaluate the LGBM Model again 


# Load required libraries
library(lightgbm)
library(caret)
library(pROC)

# Convert customer_status to factor before training
df_balanced$customer_status <- as.factor(df_balanced$customer_status)

# Split into training (80%) and testing (20%) sets
set.seed(42)
train_index <- createDataPartition(df_balanced$customer_status, p = 0.8, list = FALSE)
train_data <- df_balanced[train_index, ]
test_data <- df_balanced[-train_index, ]

# Verify class distribution in train and test sets
table(train_data$customer_status)
table(test_data$customer_status)



# Convert categorical target variable into numeric format
train_matrix <- lgb.Dataset(data = as.matrix(train_data[, -which(names(train_data) == "customer_status")]),
                            label = as.numeric(train_data$customer_status) - 1)

test_matrix <- as.matrix(test_data[, -which(names(test_data) == "customer_status")])



# Define LightGBM Parameters
params <- list(
  objective = "multiclass",  # Change from binary to multiclass
  metric = "multi_logloss",
  num_class = length(unique(df_balanced$customer_status)),  # Adjust for 3-class classification
  n_estimators = 661,
  max_depth = 240,
  learning_rate = 0.0549,
  reg_alpha = 0.0053,
  reg_lambda = 0.2593,
  min_child_weight = 0.7581,
  min_child_samples = 17,
  subsample = 0.7232,
  subsample_freq = 1,
  colsample_bytree = 0.4522,
  num_leaves = 204,
  verbosity = -1
)

# Train LightGBM Model
lgbm_model <- lgb.train(params = params, data = train_matrix, nrounds = 100)



#make prediction

# Predict probabilities
predictions <- predict(lgbm_model, test_matrix)

# Convert probabilities to class predictions
predicted_classes <- apply(predictions, 1, which.max) - 1  # Adjusting to match factor levels



# Confusion Matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = as.numeric(test_data$customer_status) - 1)
print(conf_matrix)

# Compute Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("LGBM Accuracy:", round(accuracy, 4)))


print(unique(test_data$customer_status))  # Check class labels in test data
print(colnames(predictions))  # Check column names of predictions


# Assign class labels to predictions (since we have 3 classes)
colnames(predictions) <- c("1", "2", "3")

# Convert test labels to numeric (starting from 1)
true_labels <- as.numeric(test_data$customer_status)

library(pROC)

# Compute Multi-Class ROC-AUC
roc_auc <- multiclass.roc(true_labels, predictions)

# Extract and print AUC score
auc_value <- auc(roc_auc)
print(paste("LGBM ROC AUC:", round(auc_value, 4)))


# Classification Report
classification_metrics <- confusionMatrix(as.factor(predicted_classes), as.factor(as.numeric(test_data$customer_status) - 1))
print(classification_metrics)




#hyper parameter


# Load required libraries
library(lightgbm)
library(caret)

# Define the tuning grid
grid <- expand.grid(
  num_leaves = c(31, 63, 127),
  learning_rate = c(0.01, 0.05, 0.1),
  nrounds = c(100, 300, 500),
  max_depth = c(5, 10, 15),
  reg_alpha = c(0.01, 0.1, 0.5),
  reg_lambda = c(0.01, 0.1, 0.5)
)

# Define 5-fold cross-validation
control <- trainControl(method = "cv", number = 5)

# Train model using grid search
set.seed(42)
lgbm_tuned <- train(
  customer_status ~ ., 
  data = df_balanced, 
  method = "lightgbm", 
  trControl = control,
  tuneGrid = grid
)

# Show best parameters
print(lgbm_tuned$bestTune)





















# Display the column names in churn_data
print(colnames(churn_data))

# View column names along with data types
str(churn_data)


# Identify missing values
missing_values <- colSums(is.na(churn_data))
missing_values[missing_values > 0] 
