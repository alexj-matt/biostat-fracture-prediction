# Fracture Prediction Model

An R-based logistic regression project from the **Applied Biostatistics** course at EPFL to predict the probability of a bone fracture. The analysis identifies **bone mineral density (BMD)** and **sex** as the most significant predictors through stepwise selection and rigorous validation.

## Statistical methods

Exploratory data analysis and model selection have been done using:
*   ROC-AUC analysis
*   Cook's Distance
*   Stepwise model selection with AIC and BIC criteria.

## Final Model

With an AUC of 0.99, the best-performing model is:
`logit(p) = 31.12 + 2.20 * sex - 46.88 * bmd`

The analysis confirms that lower BMD significantly increases fracture risk.

## Repository Contents

*   [`script.R`](script.R): The main R script performing the complete analysis.
*   [`bmd.csv`](bmd.csv): The dataset required to run the analysis.
*   [`odermatt-logistic.pdf`](odermatt-logistic.pdf): The full project report with detailed methodology and conclusions.
*   [`plots/`](plots/): Folder containing generated graphs and figures.

## How to Run

1.  Place the `bmd.csv` file in your working directory.
2.  Run the `fracture_analysis.R` script in R/RStudio to reproduce the entire analysis and generate all plots.
