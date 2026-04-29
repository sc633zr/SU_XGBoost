# XGBoost Classification of Medical Data

## Project Overview

This project focuses on binary classification of medical data using the **XGBoost** algorithm.  
The objective was to classify tumour samples from the **Breast Cancer Wisconsin Dataset** into two categories:

- **0 – malignant**
- **1 – benign**

The project combines theoretical analysis, machine learning implementation, model evaluation, custom algorithm development, and deployment through an interactive application.

---

## Dataset

The project uses the **Breast Cancer Wisconsin Dataset** available in `scikit-learn`.

### Dataset Information

- Number of samples: **569**
- Number of features: **30**
- Classes:
  - 212 malignant
  - 357 benign
- Missing values: **none**

The dataset contains numerical features describing cell nuclei extracted from digitized breast mass images.

---

## Project Structure

The project consists of five main parts:

### 1. Exploratory Data Analysis (EDA)

Initial data inspection included:

- dataset structure analysis
- class distribution visualization
- feature statistics
- data quality verification

---

### 2. XGBoost Library Implementation

The first model was created using the official **XGBoost** library.

Hyperparameter tuning was performed using **GridSearchCV**.

### Tuned Parameters

- `n_estimators`
- `max_depth`
- `learning_rate`
- `subsample`
- `colsample_bytree`

### Best Results

| Metric | Value |
|-------|------:|
| Accuracy | 0.956 |
| Precision | 0.947 |
| Recall | 0.986 |
| F1-score | 0.966 |
| ROC-AUC | 0.996 |

---

### 3. Custom Simplified XGBoost Implementation

A simplified custom implementation of XGBoost was developed in Python to better understand the internal mechanism of the algorithm.

The implementation includes:

- logistic loss optimization
- gradients and Hessians
- gain-based split selection
- leaf weight calculation
- boosting iterations

Main classes:

- `XGBoostStump`
- `SimpleXGBClassifier`

---

### 4. Model Comparison

The custom implementation achieved results close to the official XGBoost model.

| Model | Accuracy | Recall | ROC-AUC |
|------|---------:|-------:|--------:|
| XGBoost Library | 0.956 | 0.986 | 0.996 |
| Custom Version | 0.956 | 0.986 | 0.993 |

---

### 5. R Shiny Application

An interactive **R Shiny** application was created to present the trained model.

### Application Features

- dataset overview
- feature importance visualization
- prediction for new patient data
- real-time probability output
- clean user interface

The application runs locally on a laptop without API deployment.

---

## Technologies Used

### Python

- pandas
- numpy
- scikit-learn
- xgboost
- matplotlib
- seaborn

### R

- shiny
- ggplot2
- DT

