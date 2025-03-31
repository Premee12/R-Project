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
library(caret)
library(randomForest)
library(xgboost)
library(pROC)


conn <- dbConnect(MySQL(), dbname = "sql_project", host = "localhost", user = "root", password = "")
dbListFields(conn, "telecom_customers")
churn_data <- dbGetQuery(conn, "SELECT * FROM telecom_customers;")
dbDisconnect(conn)

### ✅ Convert to Binary Classification
churn_data$customer_status <- ifelse(churn_data$customer_status == "Joined", "Stayed", churn_data$customer_status)
churn_data$churn_binary <- ifelse(churn_data$customer_status == "Churned", 1, 0)
churn_data


#Preprocessing
churn_processed <- churn_data %>% select(-customer_id, -city, -churn_reason, -churn_category, -customer_status)

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

churn_processed[num_features] <- scale(churn_processed[num_features])

# Winsorization
cap_outliers <- function(x) {
  q1 <- quantile(x, 0.01, na.rm = TRUE)
  q3 <- quantile(x, 0.99, na.rm = TRUE)
  x[x < q1] <- q1
  x[x > q3] <- q3
  return(x)
}
churn_processed[num_features] <- lapply(churn_processed[num_features], cap_outliers)

#PCA
pca_result <- prcomp(churn_processed, center = TRUE, scale. = TRUE)
var_explained <- summary(pca_result)$importance[2,]
cumulative_variance <- cumsum(var_explained) * 100
num_components <- min(which(cumulative_variance >= 90))
pca_data <- as.data.frame(pca_result$x[, 1:num_components])
pca_data$churn_binary <- churn_data$churn_binary

# split Data
set.seed(42)
train_index <- createDataPartition(pca_data$churn_binary, p = 0.8, list = FALSE)
train_data <- pca_data[train_index, ]
test_data <- pca_data[-train_index, ]

#Random Forest
rf_model <- randomForest(as.factor(churn_binary) ~ ., data = train_data, ntree = 100)
rf_preds <- predict(rf_model, newdata = test_data)
rf_cm <- confusionMatrix(rf_preds, as.factor(test_data$churn_binary))
print(rf_cm)

# RF Metrics
rf_precision <- posPredValue(rf_preds, as.factor(test_data$churn_binary), positive = "1")
rf_recall <- sensitivity(rf_preds, as.factor(test_data$churn_binary), positive = "1")
rf_f1 <- 2 * rf_precision * rf_recall / (rf_precision + rf_recall)
print(paste("Random Forest F1 score:", round(rf_f1, 4)))

# XGBoost
x_train <- as.matrix(train_data[, -ncol(train_data)])
y_train <- train_data$churn_binary
x_test <- as.matrix(test_data[, -ncol(test_data)])
y_test <- test_data$churn_binary

xgb_model <- xgboost(data = x_train, label = y_train, nrounds = 100, objective = "binary:logistic", verbose = 0)
xgb_preds <- predict(xgb_model, x_test)
xgb_class <- ifelse(xgb_preds > 0.5, 1, 0)

xgb_cm <- confusionMatrix(as.factor(xgb_class), as.factor(y_test))
print(xgb_cm)

# XGBoost Metrics
xgb_precision <- posPredValue(as.factor(xgb_class), as.factor(y_test), positive = "1")
xgb_recall <- sensitivity(as.factor(xgb_class), as.factor(y_test), positive = "1")
xgb_f1 <- 2 * xgb_precision * xgb_recall / (xgb_precision + xgb_recall)
print(paste("XGBoost F1:", round(xgb_f1, 4)))

#ROC Curve
rf_probs <- predict(rf_model, test_data, type = "prob")
rf_roc <- roc(as.numeric(as.character(test_data$churn_binary)), rf_probs[,2])
plot(rf_roc, main = "ROC Curve - Random Forest")

xgb_roc <- roc(y_test, xgb_preds)
plot(xgb_roc, main = "ROC Curve - XGBoost", col = "blue", add = TRUE)
legend("bottomright", legend = c("RF", "XGBoost"), col = c("black", "blue"), lwd = 2)

# Save the trained XGBoost model
saveRDS(xgb_model, "xgb_churn_model.rds")

# Save the PCA
saveRDS(ncol(x_train), "pca_component_count.rds")
saveRDS(pca_result, "pca_model.rds")

#End of Timer
end_time <- Sys.time()
execution_time <- end_time - start_time
print(paste("Total Execution Time:", round(execution_time, 2), "seconds"))


