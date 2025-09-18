library(MASS)
library(ggplot2)
library(caret)
library(corrplot)
library(xtable)

# load data
bmd.data <- read.csv("./bmd.csv", stringsAsFactors = TRUE)

# (bmd = bone mineral density; bmi = body mass index)
bmd.data$bmi <- bmd.data$weight_kg / (bmd.data$height_cm/100)^2




# # Summary Statistics
# summary_age <- summary(bmd.data$age)
# summary_bmd <- summary(bmd.data$bmd)
# summary_bmi <- summary(bmd.data$bmi)
# 
# # Frequency Tables
# table_sex <- table(bmd.data$sex)
# table_fracture <- table(bmd.data$fracture)
# 
# # Create a data frame for LaTeX table
# latex_table <- data.frame(
#   Variable = c("Age", "BMD", "BMI", "Sex (F/M)", "Fracture (Yes/No)"),
#   Summary = c(
#     paste("Min:", summary_age[1]),
#     paste("Min:", summary_bmd[1]),
#     paste("Min:", summary_bmi[1]),
#     paste("F:", table_sex[1], " / M:", table_sex[2]),
#     paste("Yes:", table_fracture[2], " / No:", table_fracture[1])
#   ),
#   stringsAsFactors = FALSE
# )
# latex_code <- xtable(latex_table, align = c("l", "l"), caption = "Summary Statistics and Frequencies")
# print(latex_code, include.rownames = FALSE)





# make fracture column binary ("fracture" -> 1, "no fracture" -> 0)
bmd.data$fracture <- ifelse(bmd.data$fracture == "fracture", 1, 0)

# predict fracture from age, sex, bmi, bmd (or possibly a subset of these or possibly interactions)

# plot histograms for continuous variables: age, weight, height, bmd
pdf("continuousV_histograms.pdf", width = 10, height = 3)

par(mfrow = c(1, 4))
hist(bmd.data$age, breaks = 10, col = "skyblue", main = "Histogram of age", xlab = "age")
hist(bmd.data$weight_kg, breaks = 10, col = "lightgreen", main = "Histogram of weight", xlab = "weight [kg]")
hist(bmd.data$height_cm, breaks = 10, col = "salmon", main = "Histogram of height", xlab = "height [cm]")
hist(bmd.data$bmd, breaks = 10, col = "lightyellow", main = "Histogram of bmd", xlab = "bmd")

dev.off()

# chat ##############################
pdf("continuousV_histograms.pdf", width = 10, height = 3)

par(mfrow = c(1, 4))

# Define the number of bins
num_bins <- 10

# Calculate the bin widths based on the range of the variables
bin_width_age <- (max(bmd.data$age) - min(bmd.data$age)) / num_bins
bin_width_weight <- (max(bmd.data$weight_kg) - min(bmd.data$weight_kg)) / num_bins
bin_width_height <- (max(bmd.data$height_cm) - min(bmd.data$height_cm)) / num_bins
bin_width_bmd <- (max(bmd.data$bmd) - min(bmd.data$bmd)) / num_bins

# Use the same number of bins and calculated bin widths for each histogram
hist(bmd.data$age, breaks = seq(min(bmd.data$age), max(bmd.data$age), by = bin_width_age), col = "skyblue", main = "Histogram of age", xlab = "age")
hist(bmd.data$weight_kg, breaks = seq(min(bmd.data$weight_kg), max(bmd.data$weight_kg), by = bin_width_weight), col = "lightgreen", main = "Histogram of weight", xlab = "weight [kg]")
hist(bmd.data$height_cm, breaks = seq(min(bmd.data$height_cm), max(bmd.data$height_cm), by = bin_width_height), col = "salmon", main = "Histogram of height", xlab = "height [cm]")
hist(bmd.data$bmd, breaks = seq(min(bmd.data$bmd), max(bmd.data$bmd), by = bin_width_bmd), col = "lightyellow", main = "Histogram of bmd", xlab = "bmd")

dev.off()
##############################


# make qq plots
pdf("qq_plots.pdf", width = 10, height = 3)

par(mfrow = c(1, 4))

qqnorm(bmd.data$age, pch = 1, frame = TRUE, main = "Q-Q Plot age")
qqline(bmd.data$age, col = "red", lwd = 1)

qqnorm(bmd.data$weight_kg, pch = 1, frame = TRUE, main = "Q-Q Plot weight")
qqline(bmd.data$weight_kg, col = "red", lwd = 1)

qqnorm(bmd.data$height_cm, pch = 1, frame = TRUE, main = "Q-Q Plot height")
qqline(bmd.data$height_cm, col = "red", lwd = 1)

qqnorm(bmd.data$bmd, pch = 1, frame = TRUE, main = "Q-Q Plot bmd")
qqline(bmd.data$bmd, col = "red", lwd = 1)

dev.off()




reduced_bmd.data <- bmd.data[, c("fracture", "age", "sex", "bmi", "bmd")]
reduced_bmd.data$sex <- as.numeric(reduced_bmd.data$sex == "M")

cor_matrix <- cor(reduced_bmd.data, use = "complete.obs")

pdf("correlation_matrix.pdf", width = 3.5, height = 3.8)
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black",
         tl.srt = 45, number.cex = 0.7, title = "Correlation Matrix", mar = c(0, 0, 1, 0),
         cl.pos = "n")
# corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")
dev.off()

# pair plot
pdf("EDApair_plot.pdf", width = 10, height = 5)
pairs(reduced_bmd.data)
dev.off()
# --> positive correlation between bmd and bmi


##  MODEL SELECTION:
#   Step-wise selection (which contains forward and backwards search, keyword "both")
#   Selection criterion: AIC/BIC, R-adj, Likehood Ratio Test (LRT)?, p-value?, NO F-test

# define the scope as including everything from a constant up to all terms + interaction terms
scope_ = list(upper = ~age * sex * bmi * bmd, lower = ~1)

# Different start points --> see if they converge to the same model
lr_startP_1 <- glm(formula = fracture ~ age * sex * bmi * bmd,
                   family = binomial(), data = bmd.data)
lr_startP_2 <- glm(formula = fracture ~ 1,
                   family = binomial(), data = bmd.data)
lr_startP_3 <- glm(formula = fracture ~ sex + bmi * age,
                   family = binomial(), data = bmd.data)

### AIC (k = 2) ####################################
stepAIC(lr_startP_1,
        scope = scope_,
        direction = "both",
        k = 2)
# -->
# Step:  AIC=118.13
# fracture ~ sex + bmd

stepAIC(lr_startP_2,
        scope = scope_,
        direction = "both",
        k = 2)
# -->
# Step:  AIC=118.13
# fracture ~ bmd + sex

stepAIC(lr_startP_3,
        scope = scope_,
        direction = "both",
        k = 2)
# -->
# Step:  AIC=118.13
# fracture ~ sex + bmd

# --> ALL STARTPOINTS AGREE ON FINAL MODEL: fracture ~ sex + bmd

### BIC (k = log(n)) ########################################
n <- nrow(bmd.data)
stepAIC(lr_startP_1,
        scope = scope_,
        direction = "both",
        k = log(n))
# -->
# Step:  AIC=126.28
# fracture ~ bmd

stepAIC(lr_startP_2,
        scope = scope_,
        direction = "both",
        k = log(n))
# -->
# Step:  AIC=126.28
# fracture ~ bmd

stepAIC(lr_startP_3,
        scope = scope_,
        direction = "both",
        k = log(n))
# -->
# Step:  AIC=126.28
# fracture ~ bmd

# --> ALL STARTPOINTS AGREE ON FINAL MODEL: fracture ~ bmd
###############################################################

lr_model1 <- glm(formula = fracture ~ sex + bmd,
                 family = binomial(), data = bmd.data)
#lr_model1.fit()
lr_model2 <- glm(formula = fracture ~ bmd,
                 family = binomial(), data = bmd.data)
summary(lr_model1)
summary(lr_model2)

# Contingency table
# https://bookdown.org/ndphillips/YaRrr/logistic-regression-with-glmfamily-binomial.html
predictions1 <- predict(lr_model1)
predictions2 <- predict(lr_model2)
predictions_prob1 <- 1 / (1 + exp(-predictions1))
predictions_prob2 <- 1 / (1 + exp(-predictions2))

predicted_binary1 <- ifelse(predictions_prob1 >= 0.5, 1, 0)
predicted_binary2 <- ifelse(predictions_prob2 >= 0.5, 1, 0)

confusion_matrix1 <- confusionMatrix(factor(predicted_binary1), factor(bmd.data$fracture))
#           Reference
# Prediction   0   1
#          0 110  13
#          1   9  37
confusion_matrix2 <- confusionMatrix(factor(predicted_binary2), factor(bmd.data$fracture))
#             Reference
# Prediction   0   1
#          0 110  16
#          1   9  34

# --> model 1 should be preferred

# Prediction Histogram (TRUEs and FALSEs overlapping)
# select predictions for either "fracture" or "no fracture"
predictions_prob1_NEG <- predictions_prob1[bmd.data$fracture == 0]
predictions_prob1_POS <- predictions_prob1[bmd.data$fracture == 1]
predictions_prob2_NEG <- predictions_prob2[bmd.data$fracture == 0]
predictions_prob2_POS <- predictions_prob2[bmd.data$fracture == 1]

pdf("histogram_prediction.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))
hist(predictions_prob1_NEG, xlim = c(0, 1), breaks = 30, col = rgb(1,0,0,0.5), main = "AIC Model fitted on full dataset", xlab = "Predicted Probability")
hist(predictions_prob1_POS, xlim = c(0, 1), breaks = 30, col = rgb(0,0,1,0.5), add = T)
hist(predictions_prob2_NEG, xlim = c(0, 1), breaks = 30, col = rgb(1,0,0,0.5), main = "BIC Model fitted on full dataset", xlab = "Predicted Probability", ylab = "")
hist(predictions_prob2_POS, xlim = c(0, 1), breaks = 30, col = rgb(0,0,1,0.5), add = T)

legend("topright", legend=c("no fracture","fracture"), col=c(rgb(1,0,0,0.5),
                                                             rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

dev.off()


pdf("cooksDistance.pdf", width = 10, height = 5)
cooks1 <- cooks.distance(lr_model1)
cooks2 <- cooks.distance(lr_model2)

# Calculate the maximum Cook's distance across both models
max_cooks_distance <- max(c(cooks1, cooks2))

# Set the cutoff value (e.g., 4 / nrow(bmd.data))
cutoff <- 4 / nrow(bmd.data)

par(mfrow = c(1, 2))
# Plot Cook's distance for AIC Model
plot(cooks1, type = "b", pch = 18, col = "black", ylab = "Cook's Distance", main = "AIC Model", xlim = c(0, 170), ylim = c(0, max_cooks_distance), frame = FALSE)
abline(h = cutoff, lty = 2)

# Identify points above the cutoff
above_cutoff1 <- which(cooks1 > cutoff)
text(above_cutoff1, cooks1[above_cutoff1], labels = above_cutoff1, pos = 2) # Add indices

# Plot Cook's distance for BIC Model
plot(cooks2, type = "b", pch = 18, col = "black", ylab = "", main = "BIC Model", xlim = c(0, 170), ylim = c(0, max_cooks_distance), frame = FALSE)
abline(h = cutoff, lty = 2)

# Identify points above the cutoff
above_cutoff2 <- which(cooks2 > cutoff)
text(above_cutoff2, cooks2[above_cutoff2], labels = above_cutoff2, pos = 2) # Add indices

legend("topright", legend = "cutoff", col = "black", lty = 2)
                                                             
dev.off()



# remove data points above cook's distance cutoff

bmd.data_red1 <- bmd.data[-above_cutoff1, ]
bmd.data_red2 <- bmd.data[-above_cutoff2, ]

lr_model_red1 <- glm(formula = fracture ~ sex + bmd,
                 family = binomial(), data = bmd.data_red1)
#lr_model1.fit()
lr_model_red2 <- glm(formula = fracture ~ bmd,
                 family = binomial(), data = bmd.data_red2)

predictions_red1 <- predict(lr_model_red1, bmd.data)
predictions_red2 <- predict(lr_model_red2, bmd.data)
predictions_prob_red1 <- 1 / (1 + exp(-predictions_red1))
predictions_prob_red2 <- 1 / (1 + exp(-predictions_red2))

predicted_binary_red1 <- ifelse(predictions_prob_red1 >= 0.5, 1, 0)
predicted_binary_red2 <- ifelse(predictions_prob_red2 >= 0.5, 1, 0)

confusion_matrix_red1 <- confusionMatrix(factor(predicted_binary_red1), factor(bmd.data$fracture))

confusion_matrix_red2 <- confusionMatrix(factor(predicted_binary_red2), factor(bmd.data$fracture))

# Prediction Histogram (TRUEs and FALSEs overlapping)
# select predictions for either "fracture" or "no fracture"
predictions_prob_red1_NEG <- predictions_prob_red1[bmd.data$fracture == 0]
predictions_prob_red1_POS <- predictions_prob_red1[bmd.data$fracture == 1]
predictions_prob_red2_NEG <- predictions_prob_red2[bmd.data$fracture == 0]
predictions_prob_red2_POS <- predictions_prob_red2[bmd.data$fracture == 1]

pdf("histogram_prediction_red.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))
hist(predictions_prob_red1_NEG, xlim = c(0, 1), breaks = 30, col = rgb(1,0,0,0.5), main = "AIC Model fitted on reduced dataset", xlab = "Predicted Probability")
hist(predictions_prob_red1_POS, xlim = c(0, 1), breaks = 30, col = rgb(0,0,1,0.5), add = T)
hist(predictions_prob_red2_NEG, xlim = c(0, 1), breaks = 30, col = rgb(1,0,0,0.5), main = "BIC Model fitted on reduced dataset", xlab = "Predicted Probability", ylab = "")
hist(predictions_prob_red2_POS, xlim = c(0, 1), breaks = 30, col = rgb(0,0,1,0.5), add = T)

legend("topright", legend=c("no fracture","fracture"), col=c(rgb(1,0,0,0.5),
                                                             rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

dev.off()


# merged histograms
pdf("histogram_prediction_merged.pdf", width = 9, height = 3.)

ylim_ <- 100
par(mfrow = c(1, 4))
hist(predictions_prob1_NEG, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(1,0,0,0.5), main = "AIC Model, full dataset", xlab = "Predicted Probability")
hist(predictions_prob1_POS, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(0,0,1,0.5), add = T)
hist(predictions_prob2_NEG, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(1,0,0,0.5), main = "BIC Model, full dataset", xlab = "Predicted Probability", ylab = "")
hist(predictions_prob2_POS, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(0,0,1,0.5), add = T)

hist(predictions_prob_red1_NEG, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(1,0,0,0.5), main = "AIC Model, reduced dataset", xlab = "Predicted Probability", ylab = "")
hist(predictions_prob_red1_POS, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(0,0,1,0.5), add = T)
hist(predictions_prob_red2_NEG, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(1,0,0,0.5), main = "BIC Model, reduced dataset", xlab = "Predicted Probability", ylab = "")
hist(predictions_prob_red2_POS, xlim = c(0, 1), ylim = c(0, ylim_), breaks = 30, col = rgb(0,0,1,0.5), add = T)

legend("topright", legend=c("no fracture","fracture"), col=c(rgb(1,0,0,0.5),
                                                             rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

dev.off()







# Perform likelihood ratio test for the models fitted on full dataset
anova_1_2 <- anova(lr_model1, lr_model2, test = "Chisq")

library(pROC)
library(xtable)

# Predict probabilities
prob_lr_model1 <- predict(lr_model1, type = "response")
prob_lr_model2 <- predict(lr_model2, type = "response")
prob_lr_model_red1 <- predict(lr_model_red1, type = "response")
prob_lr_model_red2 <- predict(lr_model_red2, type = "response")

true_labels_full <- lr_model1$y
true_labels_red1 <- lr_model_red1$y
true_labels_red2 <- lr_model_red2$y

# Generate ROC curves
roc_lr_model1 <- roc(true_labels_full, prob_lr_model1)
roc_lr_model2 <- roc(true_labels_full, prob_lr_model2)
roc_lr_model_red1 <- roc(true_labels_red1, prob_lr_model_red1)
roc_lr_model_red2 <- roc(true_labels_red2, prob_lr_model_red2)

# Compute AUC values
auc_lr_model1 <- auc(roc_lr_model1)
auc_lr_model2 <- auc(roc_lr_model2)
auc_lr_model_red1 <- auc(roc_lr_model_red1)
auc_lr_model_red2 <- auc(roc_lr_model_red2)

pdf("roc_curves.pdf", width = 5, height = 5)
plot(roc_lr_model1, col = "blue", main = "ROC Curves")
plot(roc_lr_model2, add = TRUE, col = "red")
plot(roc_lr_model_red1, add = TRUE, col = "green")
plot(roc_lr_model_red2, add = TRUE, col = "purple")
legend("bottomright", legend = c("AIC Model", "BIC Model", "Reduced AIC Model", "Reduced BIC Model"), 
       col = c("blue", "red", "green", "purple"), lwd = 2)
dev.off()


auc_table <- data.frame(
  Model = c("AIC Model", "BIC Model", "Reduced AIC Model", "Reduced BIC Model"),
  AUC = c(auc_lr_model1, auc_lr_model2, auc_lr_model_red1, auc_lr_model_red2)
)

# Export the table to LaTeX
auc_latex <- xtable(auc_table, caption = "AUC Values for different Models", label = "tab:auc_values")
print(auc_latex, type = "latex", file = "auc_values.tex", include.rownames = FALSE)