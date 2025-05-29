# R Programming for Data Mining, Behavior Analysis, Clustering, and Model Prediction.

This project analyzes **customer churn behavior** and builds models to predict churn while segmenting customers for targeted business strategies. It combines **SQL for data exploration**, **R for clustering and classification**, and **Shiny for deployment via an interactive interface**.

---

##  Problem Statement

Customer churn directly impacts telecom profitability. This project was designed to:
- Understand churn behavior
- Segment customers using clustering
- Predict churn using supervised learning models
- Provide actionable insights to aid business retention efforts

---

## Dataset

- **Source**: Kaggle Telco Customer Churn dataset  
- **Rows**: ~7,000+ customers  
- **Features**: Demographics, service usage, contract details, churn info  
- **Storage**: Stored in a **MySQL** database and queried via **RMySQL**

---

##  Tools and Technologies

| Tool/Tech          | Purpose                                  |
|--------------------|------------------------------------------|
| **R**              | Data analysis, modeling, deployment      |
| **MySQL**          | Structured data storage and querying     |
| **RMySQL**         | R-MySQL database integration             |
| **ggplot2**        | Visualization                            |
| **PAM (K-Medoids)**| Customer segmentation (Clustering)       |
| **Random Forest**  | Classification model                     |
| **XGBoost**        | Advanced churn prediction model          |
| **Shiny**          | Web app deployment and user interface    |
| **GitHub**         | Version control and collaboration        |

---

## What Was Done

- Performed SQL-based **EDA** on customer churn via `RMySQL`
- Conducted full **data cleaning**, transformation, and feature engineering in R
- Applied **PCA** for dimensionality reduction
- Segmented customers using **K-Medoids (PAM) clustering**
- Built classification models using:
  - **Random Forest**
  - **XGBoost** (Best performance with 97.23% accuracy)
- Evaluated models using precision, recall, F1-score, ROC Curve
- Deployed a **Shiny web application** to predict churn from new customer data

---

## Key Insights

- Customers with **month-to-month contracts**, low tenure, and limited add-ons are more likely to churn
- Competitor influence and lack of technical support are major churn drivers
- Clustering revealed three key customer segments:  
  1. High-risk churners  
  2. Loyal high-value customers  
  3. Cost-sensitive but stable users

---

## How to Launch the Project

1. **Clone this repo**:
   ```bash
   git clone https://github.com/Premee12/R-Project.git


### 2. Set up the Database
- Create a new database in MySQL  
- Import the telecom customer dataset into a table (`telecom_customers`)  
- Make sure R can access this using the `RMySQL` package

### 3. Run Scripts in Order (Using RStudio)
> To ensure all necessary model files are created for the app to work properly.

- `clustering_model.R`
  - `Pam_models.rds` 
- `classification_model.R` 
  - `xgb_churn_model.rds`
  - `pca_model.rds`
  - `pca_component_count.rds`

### 4. Run the app.R file to launch the Shiny App 
  - A browser window will open with an interactive interface where you can:
  - Enter customer attributes
  - Predict whether the customer is **likely to churn
