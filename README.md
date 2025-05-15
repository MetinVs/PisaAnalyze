PISA 2023 Great Britain Student Data Analysis – Educational Performance & Modeling with R

This project explores the 2022 PISA dataset for Great Britain students to analyze and model the factors influencing mathematics performance. It was completed as part of an independent educational data science study using R.

Objectives:
- Understand the relationship between math, reading, and science scores.
- Apply data cleaning and preprocessing (handling missing values, sampling).
- Use statistical and machine learning models to predict math scores.

Steps:
- Exploratory Data Analysis (EDA): Boxplots, descriptive statistics, correlation matrices.
- Data Cleaning: Replacing invalid zeros with `NA`, mean imputation.
- Modeling Approaches:
  - Linear Regression: Two models tested with cognitive dimensions (e.g., RSN, UNDR) and general scores.
  - Random Forest: Feature importance analysis and prediction comparison.
  - Cross-validation: RMSE & MAE comparison between models.

Tools & Libraries:
`dplyr`, `ggplot2`, `corrplot`, `caret`, `randomForest`, `psych`, and `naniar`.

Results:
- Math performance shows significant correlation with reading and science.
- Random Forest models provide slightly better generalization than linear regression.
- The analysis highlights how cognitive sub-skills and general performance are linked.

---

> 📁 Dataset Source: 2023 PISA Student Data (Great Britain)
> 📍 Language: R

https://www.oecd.org/en/data/datasets/pisa-2022-database.html#data
