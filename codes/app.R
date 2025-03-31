
library(shiny)
library(xgboost)
library(data.table)
library(caret)
library(cluster)

# Load trained models
xgb_model <- readRDS("xgb_churn_model.rds")
pca_model <- readRDS("pca_model.rds")
num_pca_components <- readRDS("pca_component_count.rds")

# NOTE: Cluster risk knowledge based on prior analysis
# Cluster 1 = High Risk, Cluster 2 = Moderate Risk, Cluster 3 = Low Risk

# UI
ui <- fluidPage(
  titlePanel("Customer Churn Prediction App with Cluster Insight"),
  sidebarLayout(
    sidebarPanel(
      numericInput("tenure", "Tenure (months):", value = 12),
      numericInput("monthly_charge", "Monthly Charge:", value = 70),
      numericInput("total_charge", "Total Charges:", value = 1000),
      selectInput("contract", "Contract Type:", choices = c("Month-to-month", "One year", "Two year")),
      selectInput("internet", "Internet Type:", choices = c("DSL", "Fiber Optic", "Cable")),
      selectInput("gender", "Gender:", choices = c("Male", "Female")),
      selectInput("cluster_input", "Assigned Cluster (Based on Segmentation):", choices = c("1", "2", "3")),
      actionButton("predict_btn", "Predict Churn")
    ),
    mainPanel(
      verbatimTextOutput("prediction")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predict_btn, {
    result_text <- tryCatch({
      
      # Prepare user input
      user_input <- data.frame(
        tenure_in_months = input$tenure,
        monthly_charge = input$monthly_charge,
        total_charges = input$total_charge,
        contract_One.year = ifelse(input$contract == "One year", 1, 0),
        contract_Two.year = ifelse(input$contract == "Two year", 1, 0),
        internet_type_DSL = ifelse(input$internet == "DSL", 1, 0),
        internet_type_Fiber.Optic = ifelse(input$internet == "Fiber Optic", 1, 0),
        gender_Male = ifelse(input$gender == "Male", 1, 0)
      )
      
      # Align with PCA features
      model_features <- rownames(pca_model$rotation)
      for (col in setdiff(model_features, names(user_input))) {
        user_input[[col]] <- 0
      }
      user_input <- user_input[, model_features]
      
      # PCA transformation
      user_pca <- predict(pca_model, newdata = user_input)
      user_pca_input <- as.matrix(user_pca[, 1:num_pca_components])
      
      # Predict churn with XGBoost
      prediction <- predict(xgb_model, user_pca_input)
      model_result <- ifelse(prediction[1] > 0.5, " Likely to Churn", " Likely to Stay")
      
      # Override based on cluster risk level
      cluster_selected <- input$cluster_input
      risk_tag <- switch(cluster_selected,
                         "1" = " Likely to Churn ",
                         "2" = " Possibly to Churn",
                         "3" = model_result)
      
      paste("Prediction:", risk_tag, "| Cluster Selected:", cluster_selected)
      
    }, error = function(e) {
      paste("Prediction Failed:", e$message)
    })
    
    output$prediction <- renderText(result_text)
  })
}

# Run App
shinyApp(ui = ui, server = server)
