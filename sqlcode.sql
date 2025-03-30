USE sql_project;

-- This SQL analysis is to gain insights into the data before proceeding to R studio

DESCRIBE sql_project.telecom_customers; -- to see what features of the database looks like 

-- wrote this code to revamp the feature names to be in a better structure for querying
SELECT CONCAT('ALTER TABLE telecom_customers CHANGE `', COLUMN_NAME, '` ',
              LOWER(REPLACE(COLUMN_NAME, ' ', '_')), ' ', COLUMN_TYPE, ';')
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'telecom_customers' AND TABLE_SCHEMA = 'sql_project';

ALTER TABLE telecom_customers CHANGE `Customer ID` customer_id text;
ALTER TABLE telecom_customers CHANGE `Gender` gender text;
ALTER TABLE telecom_customers CHANGE `Age` age int;
ALTER TABLE telecom_customers CHANGE `Married` married text;
ALTER TABLE telecom_customers CHANGE `Number of Dependents` number_of_dependents int;
ALTER TABLE telecom_customers CHANGE `City` city text;
ALTER TABLE telecom_customers CHANGE `Zip Code` zip_code int;
ALTER TABLE telecom_customers CHANGE `Latitude` latitude double;
ALTER TABLE telecom_customers CHANGE `Longitude` longitude double;
ALTER TABLE telecom_customers CHANGE `Number of Referrals` number_of_referrals int;
ALTER TABLE telecom_customers CHANGE `Tenure in Months` tenure_in_months int;
ALTER TABLE telecom_customers CHANGE `Offer` offer text;
ALTER TABLE telecom_customers CHANGE `Phone Service` phone_service text;
ALTER TABLE telecom_customers CHANGE `Avg Monthly Long Distance Charges` avg_monthly_long_distance_charges double;
ALTER TABLE telecom_customers CHANGE `Multiple Lines` multiple_lines text;
ALTER TABLE telecom_customers CHANGE `Internet Service` internet_service text;
ALTER TABLE telecom_customers CHANGE `Internet Type` internet_type text;
ALTER TABLE telecom_customers CHANGE `Avg Monthly GB Download` avg_monthly_gb_download int;
ALTER TABLE telecom_customers CHANGE `Online Security` online_security text;
ALTER TABLE telecom_customers CHANGE `Online Backup` online_backup text;
ALTER TABLE telecom_customers CHANGE `Device Protection Plan` device_protection_plan text;
ALTER TABLE telecom_customers CHANGE `Premium Tech Support` premium_tech_support text;
ALTER TABLE telecom_customers CHANGE `Streaming TV` streaming_tv text;
ALTER TABLE telecom_customers CHANGE `Streaming Movies` streaming_movies text;
ALTER TABLE telecom_customers CHANGE `Streaming Music` streaming_music text;
ALTER TABLE telecom_customers CHANGE `Unlimited Data` unlimited_data text;
ALTER TABLE telecom_customers CHANGE `Contract` contract text;
ALTER TABLE telecom_customers CHANGE `Paperless Billing` paperless_billing text;
ALTER TABLE telecom_customers CHANGE `Payment Method` payment_method text;
ALTER TABLE telecom_customers CHANGE `Monthly Charge` monthly_charge double;
ALTER TABLE telecom_customers CHANGE `Total Charges` total_charges double;
ALTER TABLE telecom_customers CHANGE `Total Refunds` total_refunds double;
ALTER TABLE telecom_customers CHANGE `Total Extra Data Charges` total_extra_data_charges int;
ALTER TABLE telecom_customers CHANGE `Total Long Distance Charges` total_long_distance_charges double;
ALTER TABLE telecom_customers CHANGE `Total Revenue` total_revenue double;
ALTER TABLE telecom_customers CHANGE `Customer Status` customer_status text;
ALTER TABLE telecom_customers CHANGE `Churn Category` churn_category text;
ALTER TABLE telecom_customers CHANGE `Churn Reason` churn_reason text;

DESC telecom_customers;

SELECT * FROM sql_project.telecom_customers limit 100; -- overview with few columns

SELECT COUNT(*) AS total_customers FROM telecom_customers;
-- total customers details 7043

SELECT `customer_status`, COUNT(*) AS count FROM telecom_customers GROUP BY customer_status;

SELECT distinct `customer_status` FROM telecom_customers
