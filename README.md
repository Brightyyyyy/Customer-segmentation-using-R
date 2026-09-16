# 🛍️ Customer Segmentation using K-Means Clustering (R)

An unsupervised machine learning project that segments mall customers into distinct groups based on age, annual income, and spending behavior — using K-Means clustering in R.

---

## 📌 Overview

Customer segmentation is one of the most practical applications of unsupervised learning: rather than treating all customers the same, businesses can group them by shared characteristics to target marketing, personalize offers, and identify their most valuable customer segments. This project applies **K-Means clustering** to a mall customer dataset to uncover natural groupings based on income and spending patterns — the kind of analysis a retail or marketing analytics team would use to decide who to target and how.

---

## 🛠️ Tools & Technologies

- **R**
- **ggplot2 / ggthemes** — data visualization
- **dplyr / tidyr** — data manipulation
- **cluster / factoextra** — clustering, silhouette analysis, and gap statistic
- **scales, lubridate, DT** — supporting utilities

---

## 🔍 Exploratory Data Analysis

- Computed summary statistics (mean, standard deviation) for Age, Annual Income, and Spending Score
- Visualized **gender distribution** using bar and pie charts
- Explored **age distribution** with histograms and boxplots
- Analyzed **annual income** using histograms, density plots, and boxplots
- Examined **spending score** distribution through histograms and boxplots

---

## 📊 Clustering Approach

Rather than picking an arbitrary number of clusters, the optimal cluster count was determined using **three separate validation methods**:

1. **Elbow Method** — plotted total within-cluster sum of squares across k = 1 to 10 to identify the point of diminishing returns
2. **Average Silhouette Method** — evaluated cluster cohesion and separation across k = 2 to 10 to find the value of k that best balances distinct, well-separated clusters
3. **Gap Statistic** — compared clustering performance against a reference null distribution to further validate the optimal number of clusters

Based on this analysis, **K-Means clustering with k = 6** was applied to segment customers using Annual Income, Spending Score, and Age.

---

## 📈 Visualizing the Segments

- Plotted customer segments on **Annual Income vs. Spending Score**, revealing clear, interpretable groups (e.g., high income/low spending, low income/high spending, average income/average spending)
- Plotted segments on **Spending Score vs. Age** to explore how spending behavior varies across age groups
- Applied **Principal Component Analysis (PCA)** to reduce dimensionality and visualize all clusters together in 2D space, with distinct colors for each segment

---

## 🎯 What This Project Demonstrates

- Applying unsupervised learning (K-Means) to solve a real business problem — customer targeting
- Using multiple statistical validation methods (elbow, silhouette, gap statistic) to choose cluster count rigorously, rather than guessing
- Conducting structured exploratory data analysis before modeling
- Using PCA to visualize multi-dimensional clustering results in an interpretable way
- Translating raw customer data into actionable, business-relevant segments
