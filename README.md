# Customer Behavior Analysis and Segmentation Using SQL and R 

This project analyzes **customer behavior** to uncover why customers churn and how they can be segmented for targeted business strategies. It combines **SQL for data exploration**, **R for clustering and classification**, and **GitHub for collaboration and deployment**.

---

## Problem Statement

Understanding why customers churn is critical for any business. This project uses data mining techniques to:
- Analyze customer churn patterns and behavior
- Segment customers into meaningful groups
- Derive actionable insights for business retention strategies

---

## 🗂 Dataset

- **Source**: Customer behavior dataset from the telecom domain  
- **Rows**: ~7,000 customers  
- **Features**: Demographics, contract details, service usage, churn info  
- **Storage**: Managed in a **MySQL database**, queried via **RMySQL**

---

## Tools and Technologies

| Tool              | Purpose                             |
|-------------------|-------------------------------------|
| **R**             | Data analysis, clustering, modeling |
| **MySQL**         | Structured data querying            |
| **RMySQL**        | R-MySQL database integration        |
| **ggplot2**       | Visualization  
| **K-Medoids**     | Clustering
| **LightGBM**       | Classification modeling             |
| **GitHub**        | Collaboration and version control   |

---

## What Was Done

- Connected R to a MySQL database using `RMySQL`
- Performed SQL-based exploratory analysis on customer churn
- Cleaned and prepared data in R
- Built a **Esemble Model** model for churn classification **LightGBM**
- Applied **K-Medoids clustering** for customer segmentation
- Validated clustering using **Purity**, **Jaccard Index**, and **Entropy**

---

## Key Insights

- **Top churn drivers** include:
  - **Month-to-month contracts**
  - **Influence of competitors**
  - Lack of technical support and online security features
- Segmented customers into clusters for targeted marketing and retention strategies

---

##  How to Run

1. Clone the repo:
   ```bash
   https://github.com/Premee12/R-Project.git


