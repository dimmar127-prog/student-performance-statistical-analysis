# 🎓 Student Performance: Statistical Analysis & Inference

## 📌 Project Overview
This project applies rigorous statistical testing to educational data to understand the factors that drive student success. By analyzing metrics like Study Hours, Attendance Rates, and Pre/Post-Lecture scores, this analysis aims to identify statistically significant predictors of a student's final score.

## 🛠️ Methods & Technologies Used
* **Language:** R
* **Libraries:** `tidyverse`, `ggpubr`, `rstatix`, `jtools`, `GGally`
* **Statistical Methods:**
  * Data manipulation and factor conversion
  * Shapiro-Wilk tests for normality
  * Spearman Rank Correlation (chosen due to non-normal distributions)
  * Paired t-tests (Pre- vs. Post-Lecture analysis)
  * Multiple Linear Regression

## 📊 Key Insights
* **Normality Testing:** Shapiro-Wilk tests revealed that key variables (Final Score, Age, Study Hours, Attendance) were not normally distributed, necessitating the use of non-parametric correlation methods (Spearman) to ensure mathematical validity.
* **Predictive Modeling:** A Multiple Linear Regression model was fitted (`FinalScore ~ StudyHours + AttendanceRate + Age + Gender`) to evaluate the weighted impact of different behavioral and demographic factors on final academic outcomes. 

---
*Note: This project was co-authored with Xondropoulos as part of university coursework.*
