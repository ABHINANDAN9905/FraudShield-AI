# 🛡️ FraudShield AI
### Intelligent Credit Card Fraud Detection, Explainable AI & Financial Risk Intelligence

**Detect · Analyze · Explain · Score · Investigate**

An end-to-end fraud analytics and decision-support platform built entirely with **R** and **Shiny**.

![Built with R](https://img.shields.io/badge/Built%20with-R-276DC3?logo=r)
![Dashboard](https://img.shields.io/badge/Dashboard-Shiny-blue)
![Machine Learning](https://img.shields.io/badge/Machine%20Learning-Multi--Model-orange)
![Status](https://img.shields.io/badge/Status-Research%20%26%20Development-informational)

---

## 📌 Table of Contents

1. Overview
2. Problem Statement
3. Project Objectives
4. Key Features
5. Dashboard Modules
6. Machine Learning Models
7. Model Evaluation Results
8. Explainable AI
9. Risk Scoring & Prediction
10. Dataset Description
11. System Architecture
12. Technology Stack
13. Project Structure
14. Installation & Execution
15. Screenshots
16. Limitations
17. Future Enhancements
18. Author

---

## 1. Overview

**FraudShield AI** is an interactive financial fraud detection and risk intelligence platform developed entirely using the R programming language.

The project focuses on identifying potentially fraudulent credit card transactions using machine learning, statistical analysis, explainable AI, risk scoring, and interactive data visualization.

FraudShield provides a centralized environment where users can:

- Analyze credit card transactions.
- Identify fraudulent transaction patterns.
- Compare machine learning models.
- Evaluate model performance.
- Understand model predictions through explainable AI.
- Generate transaction risk scores.
- Investigate suspicious transactions.
- Monitor data quality and generate reports.

The application is developed using **R, Shiny, and machine learning libraries in R**.

> **Project Scope:** FraudShield is an academic research and simulation platform. It is not connected to live banking systems, payment gateways, or card networks.

---

## 2. Problem Statement

Credit card fraud is a major challenge in the financial industry. Financial institutions process millions of transactions, and identifying fraudulent activities among legitimate transactions is difficult.

One of the biggest challenges is the extreme imbalance between fraudulent and legitimate transactions.

Traditional classification models may achieve high accuracy while failing to detect fraudulent transactions.

Fraud detection systems must address several challenges:

1. Identifying fraudulent transactions accurately.
2. Reducing false-positive alerts.
3. Minimizing financial losses.
4. Handling highly imbalanced datasets.
5. Explaining why a transaction was classified as suspicious.
6. Supporting investigation and risk-based decisions.
7. Maintaining data quality and model reliability.

**FraudShield AI addresses these challenges through an integrated machine learning and financial risk analytics platform.**

---

## 3. Project Objectives

The major objectives are:

- Develop an end-to-end fraud detection pipeline using R.
- Perform data cleaning, validation, and feature engineering.
- Conduct exploratory data analysis.
- Handle extreme class imbalance.
- Train and compare multiple machine learning algorithms.
- Evaluate models using fraud-sensitive performance metrics.
- Implement explainable AI techniques.
- Develop interactive risk scoring functionality.
- Build a transaction investigation workflow.
- Create a professional multi-page Shiny dashboard.
- Generate downloadable analytical and data-quality reports.

---

## 4. Key Features

| Feature | Description |
|---|---|
| Fraud Detection | Machine learning-based transaction classification |
| Data Analytics | Transaction and fraud pattern analysis |
| Model Benchmarking | Comparison of multiple ML algorithms |
| Explainable AI | Global and local feature attribution |
| Risk Scoring | Transaction-level risk assessment |
| Threshold Optimization | Precision-recall trade-off analysis |
| Investigation Center | Suspicious transaction review workflow |
| Data Quality | Validation and integrity monitoring |
| Interactive Dashboard | Multi-page R Shiny application |
| Report Generation | Downloadable analytical reports |

---

## 5. Dashboard Modules

FraudShield contains eight major dashboard modules.

### 5.1 Executive Overview

Provides a high-level summary of the transaction dataset and fraud activity.

**Key indicators:**
- Total Transactions
- Fraud Transactions
- Legitimate Transactions
- Fraud Prevalence
- Total Fraud Loss
- Average Ticket Size

**Visualizations:**
- Fraud vs legitimate distribution.
- Transaction volume timeline.
- Fraud activity timeline.
- Transaction amount distribution.
- High-risk incident table.

![Executive Overview](screenshots/01-executive-overview.png)

### 5.2 Transaction Explorer

An interactive transaction investigation and filtering interface.

**Features:**
- Transaction search.
- Ground-truth class filtering.
- Transaction amount filtering.
- Elapsed-time filtering.
- Model selection.
- Decision threshold adjustment.
- Filtered transaction statistics.
- Transaction-level data exploration.
- CSV export functionality.

![Transaction Explorer](screenshots/02-transaction-explorer.png)

### 5.3 Fraud Analytics

Provides detailed statistical analysis of fraudulent and legitimate transaction patterns.

**Features:**
- Dynamic fraud prevalence analysis.
- Feature correlation analysis.
- Interactive correlation heatmap.
- Fraud vs legitimate density comparison.
- Transaction amount distribution.
- Hourly fraud concentration.
- Transaction volume analysis.
- Interactive analytical filters.
- Fraud pattern discovery.

![Fraud Analytics](screenshots/03-fraud-analytics-overview.png)

![Fraud Analytics Deep Dive](screenshots/04-fraud-analytics-deep-dive.png)

### 5.4 ML Model Performance

Evaluates and compares machine learning algorithms.

**Models:**
- Logistic Regression
- Random Forest
- XGBoost
- Isolation Forest

**Evaluation metrics:**
- Precision
- Recall
- F1-Score
- ROC-AUC
- PR-AUC
- Confusion Matrix
- False Positive Rate
- False Negative Rate

**Advanced functionality:**
- Precision-Recall curves.
- ROC curves.
- Threshold optimization.
- Model comparison.
- Champion model selection.
- Statistical validation.

![ML Model Performance](screenshots/05-ml-model-performance.png)

### 5.5 Explainable AI

Provides model interpretability and transaction-level explanation.

**Features:**
- Global feature importance.
- Local transaction attribution.
- SHAP-compatible explanation workflows.
- Feature contribution analysis.
- Partial dependence analysis where implemented.
- Counterfactual analysis where implemented.
- Cross-model explainability comparison.

This module helps users understand which mathematical features influence model predictions.

![Explainable AI](screenshots/06-explainable-ai.png)

**Important:** The dataset features V1–V28 are anonymized PCA components. They do not represent directly identifiable merchant, customer, device, or location attributes.

### 5.6 Risk Scoring & Prediction

Provides transaction-level risk assessment and decision-support functionality.

**Features:**
- Single transaction assessment.
- Batch assessment workflow where configured.
- Model selection.
- Adjustable decision threshold.
- Normalized risk score.
- Risk classification.
- Financial risk simulation.
- Prediction history.
- Explainability integration.

Risk categories can include:

- Very Low Risk
- Low Risk
- Medium Risk
- High Risk
- Critical Risk

![Risk Scoring](screenshots/07-risk-scoring-prediction.png)

### 5.7 Investigation Center

Provides a local investigation workflow for suspicious transactions.

**Features:**
- Flagged transaction queue.
- Pending investigation status.
- Under-review status.
- Reviewed cases.
- Escalated cases.
- Analyst annotations.
- Investigation history.
- SQLite-based local persistence where configured.

![Investigation Center](screenshots/08-investigation-center.png)

### 5.8 Reports & Data Quality

Provides data governance, validation, and reporting functionality.

**Features:**
- Schema validation.
- Missing-value detection.
- Duplicate transaction audit.
- Non-finite value detection.
- Target-label validation.
- Data completeness monitoring.
- Data cleaning reports.
- Validation reports.
- Investigation audit exports.

![Reports and Data Quality](screenshots/09-reports-data-quality.png)

---

## 6. Machine Learning Models

FraudShield explores supervised classification and unsupervised anomaly detection.

### Logistic Regression

Used as a statistical baseline for binary classification.

### Random Forest

An ensemble learning algorithm that combines multiple decision trees to classify transactions.

### XGBoost

A gradient boosting algorithm designed for high-performance classification and complex feature relationships.

### Isolation Forest

An unsupervised anomaly detection algorithm that identifies unusual observations based on their isolation characteristics.

**Important:** Isolation Forest anomaly scores are not automatically calibrated fraud probabilities.

---

## 7. Model Evaluation Results

The current dashboard reports the following Random Forest holdout evaluation results:

| Metric | Reported Result |
|---|---:|
| PR-AUC | 0.8782 |
| Peak F1-Score | 0.8772 |
| Recall | 80.65% |
| Precision | 96.15% |
| True Positives | 75 |
| False Positives | 3 |
| False Negatives | 18 |
| True Negatives | 56,666 |

### Confusion Matrix

| Actual / Predicted | Fraud | Legitimate |
|---|---:|---:|
| Fraud | 75 | 18 |
| Legitimate | 3 | 56,666 |

These values are transcribed from the dashboard's displayed evaluation. They should be independently reproduced from the saved model evaluation artifacts before being presented as verified experimental results.

### Why PR-AUC?

Credit card fraud datasets are highly imbalanced. Accuracy alone can be misleading because a model may classify nearly every transaction as legitimate.

Precision-Recall analysis provides a more useful view of minority-class detection performance.

---

## 8. Explainable AI

FraudShield incorporates explainability workflows to improve the transparency of machine learning predictions.

### Global Feature Importance

Identifies features that contribute strongly to the model's overall predictive behavior.

### Local Feature Attribution

Explains the contribution of features for an individual transaction.

### SHAP-Compatible Analysis

Provides additive feature attribution workflows where supported by the selected model and implementation.

### Partial Dependence

Helps examine how changes in selected features relate to model predictions.

**Note:** Feature importance and attribution explain model behavior; they do not establish causation.

---

## 9. Risk Scoring & Prediction

FraudShield converts supported supervised model probabilities into a normalized risk display.

**Risk Score = Predicted Fraud Probability × 100**

Example risk categories:

| Risk Score | Classification |
|---|---|
| 0–19 | Very Low |
| 20–39 | Low |
| 40–59 | Medium |
| 60–79 | High |
| 80–100 | Critical |

Users can adjust decision thresholds and observe how classification decisions affect precision, recall, and false alarms.

Risk categories are configurable application policies, not universal financial-industry standards.

---

## 10. Dataset Description

**Dataset:** European Credit Card Fraud Detection

**Source:** [Kaggle - Credit Card Fraud Detection](https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud)

### Dataset Statistics

| Attribute | Value |
|---|---:|
| Total Transactions | 284,807 |
| Legitimate Transactions | 284,315 |
| Fraudulent Transactions | 492 |
| Total Original Features | 31 |
| Fraud Prevalence | Approximately 0.1727% |

### Dataset Features

- Time
- V1 to V28
- Amount
- Class

### Target Variable

| Class | Meaning |
|---|---|
| 0 | Legitimate Transaction |
| 1 | Fraudulent Transaction |

**Time** represents elapsed seconds from the first transaction in the dataset. It is not an actual clock timestamp.

**V1–V28** are anonymized PCA-transformed features.

The dataset is historical and anonymized. Its findings may not generalize to current real-world payment traffic.

---

## 11. System Architecture

```text
                 PUBLIC DATASET
                       |
                       v
              DATA INGESTION
                       |
                       v
             DATA VALIDATION
                       |
                       v
              DATA CLEANING
                       |
                       v
           FEATURE ENGINEERING
                       |
                       v
             DATA PARTITIONING
                       |
          +------------+------------+
          |            |            |
          v            v            v
       Logistic     Random        XGBoost
      Regression    Forest
          |            |            |
          +------------+------------+
                       |
                       v
              MODEL EVALUATION
                       |
          +------------+------------+
          |            |            |
          v            v            v
      Explainable   Risk Scoring   Reporting
          AI        & Prediction   & Audit
          |            |            |
          +------------+------------+
                       |
                       v
                R SHINY DASHBOARD
```

---

## 12. Technology Stack

| Category | Technologies |
|---|---|
| Programming Language | R |
| Dashboard Framework | Shiny |
| UI Framework | bslib |
| Data Processing | dplyr, data.table |
| Data Visualization | ggplot2 |
| Interactive Charts | plotly |
| Interactive Tables | DT |
| Random Forest | ranger |
| Gradient Boosting | xgboost |
| Explainable AI | DALEX and implemented attribution packages |
| Database | SQLite, RSQLite |
| Development Environment | RStudio Desktop |
| AI-Assisted Development | Antigravity |
| Version Control | Git, GitHub |

**Development constraint:** Data processing, machine learning, and dashboard implementation are designed to remain within the R ecosystem.

---

## 13. Project Structure

Illustrative project structure:

```text
FraudShield-AI/
│
├── app/
│   ├── modules/
│   └── app.R
│
├── R/
│   ├── data_validation.R
│   ├── data_cleaning.R
│   ├── eda_analysis.R
│   ├── feature_engineering.R
│   ├── model_training.R
│   ├── model_evaluation.R
│   ├── model_comparison.R
│   └── prediction.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── models/
│
├── outputs/
│   ├── plots/
│   └── reports/
│
├── screenshots/
│
├── tests/
│
└── README.md
```

The structure is illustrative. Refer to the actual repository for the current implementation.

---

## 14. Installation & Execution

### Prerequisites

- R
- RStudio Desktop
- Git
- European Credit Card Fraud Detection dataset

### Step 1: Clone Repository

```bash
git clone https://github.com/ABHINANDAN9905/FraudShield-AI.git
cd FraudShield-AI
```

### Step 2: Open in RStudio

Open the project in RStudio Desktop.

### Step 3: Install Required Packages

Run the following command in the R console:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "data.table",
  "ggplot2",
  "plotly",
  "DT",
  "ranger",
  "xgboost",
  "RSQLite",
  "DALEX"
))
```

Install any additional packages required by the actual project source files.

### Step 4: Add Dataset

Download the dataset from Kaggle and place the extracted CSV file in the configured data directory.

Example:

```text
data/raw/creditcard.csv
```

### Step 5: Run Application

If the Shiny entry point is inside the `app` directory:

```r
shiny::runApp("app")
```

If the entry point is in the project root:

```r
shiny::runApp()
```

Use the command matching the actual project structure.

---

## 15. Screenshots

The `screenshots/` directory contains dashboard captures for the major application modules.

Replace development captures containing visible errors with updated screenshots after testing the latest application version.

---

## 16. Limitations

- The dataset is historical and anonymized.
- PCA features do not directly represent named business attributes.
- Model performance depends on data partitioning, preprocessing, and decision thresholds.
- Accuracy alone is not a reliable metric for extreme class imbalance.
- Anomaly scores are not automatically fraud probabilities.
- Risk decisions are simulated and not connected to payment infrastructure.
- The application is a research and portfolio project, not a certified production banking system.

---

## 17. Future Enhancements

Potential future improvements include:

- Advanced probability calibration.
- Cost-sensitive model optimization.
- Time-aware model validation.
- Concept drift detection.
- Automated model monitoring.
- Advanced experiment tracking.
- Improved model governance.
- Secure authentication and role-based access.
- Production-grade deployment and logging.
- Streaming transaction ingestion.

---

## 18. Author

**Abhinandan Kumar**

GitHub: [@ABHINANDAN9905](https://github.com/ABHINANDAN9905)

Project Repository: [FraudShield-AI](https://github.com/ABHINANDAN9905/FraudShield-AI)

FraudShield AI was developed to explore the complete fraud analytics lifecycle using R, including data preparation, machine learning, explainable AI, risk assessment, and investigation workflows.

---

## ⭐ Feedback & Collaboration

If you find this project interesting:

- Star the repository.
- Report bugs or issues.
- Suggest new features.
- Share feedback and improvements.
- Connect for technical discussions and collaboration.

**FraudShield AI — Detect. Analyze. Explain. Score. Investigate.**

Built with R ❤️
