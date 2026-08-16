#Joseph Tarlin, ALY6015, 07/12/2026
#Load Ames housing dataset
ames <- read.csv("AmesHousing.csv")
#Exploratory data analysis & descriptive statistics
str(ames[, 1:15])
summary(ames$SalePrice)
sd(ames$SalePrice)
na_counts <- sort(colSums(is.na(ames)), decreasing = TRUE)
na_counts <- na_counts[na_counts > 0]
print(na_counts)
hist(ames$SalePrice, breaks = 40, col = "steelblue",
main = "Distribution of Sale Price", xlab = "Sale Price ($)")
#Input missing values
num_cols <- sapply(ames, is.numeric)
numeric_names <- names(ames)[num_cols]
for (col in numeric_names) {
if (any(is.na(ames[[col]]))) {
ames[[col]][is.na(ames[[col]])] <- mean(ames[[col]], na.rm = TRUE)}}
char_cols <- names(ames)[sapply(ames, is.character)]
get_mode <- function(x) {
ux <- unique(x[!is.na(x)])
ux[which.max(tabulate(match(x, ux)))]}
for (col in char_cols) {
if (any(is.na(ames[[col]]))) {
ames[[col]][is.na(ames[[col]])] <- "None"}}
cat("Remaining NAs after imputation:", sum(is.na(ames)), "\n")
#Correlation matrix of numeric values
num_df <- ames[, sapply(ames, is.numeric)]
num_df$X...Order <- NULL
num_df$PID <- NULL
cor_matrix <- cor(num_df, use = "pairwise.complete.obs")
write.csv(round(cor_matrix, 3), "cor_matrix.csv")
#Plot of the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper",
tl.cex = 0.5, tl.col = "black", order = "hclust",
title = "Correlation Matrix of Numeric Ames Housing Variables",
mar = c(0, 0, 2, 0))
price_cors <- sort(cor_matrix[, "SalePrice"], decreasing = TRUE)
print(price_cors)
write.csv(as.data.frame(price_cors), "saleprice_correlations.csv")
#Scatterplots highest, lowest, and ~0.5 correlation with SalePrice
price_cors_no_self <- price_cors[names(price_cors) != "SalePrice"]
highest_var <- names(price_cors_no_self)[which.max(price_cors_no_self)]
lowest_var <- names(price_cors_no_self)[which.min(price_cors_no_self)]
closest_05 <- names(price_cors_no_self)[which.min(abs(price_cors_no_self - 0.5))]
cat("Highest correlation with SalePrice:", highest_var, "r =",
price_cors_no_self[highest_var], "\n")
cat("Lowest correlation with SalePrice:", lowest_var, "r =",
price_cors_no_self[lowest_var], "\n")
cat("Correlation closest to 0.5 with SalePrice:", closest_05, "r =",
price_cors_no_self[closest_05], "\n")
plot_scatter <- function(var, titletxt) {
plot(ames[[var]], ames$SalePrice,
xlab = var, ylab = "Sale Price ($)",
main = titletxt, pch = 19, col = rgb(0.2, 0.4, 0.6, 0.4))
abline(lm(ames$SalePrice ~ ames[[var]]), col = "red", lwd = 2)}
plot_scatter(highest_var, paste0("SalePrice vs ", highest_var, " (Highest Correlation)"))
plot_scatter(lowest_var, paste0("SalePrice vs ", lowest_var, " (Lowest Correlation)"))
plot_scatter(closest_05, paste0("SalePrice vs ", closest_05, " (Correlation ~ 0.5)"))
#Regression model using at least 3 continuous variables
model1 <- lm(SalePrice ~ Gr.Liv.Area + Total.Bsmt.SF + Year.Built +
Overall.Qual + Garage.Cars, data = ames)
summary(model1)
#Model in equation form & coefficient interpretation
coef(model1)
#Diagnostic plots of the regression model
png("figs/diagnostics_model1.png", width = 1000, height = 1000)
par(mfrow = c(2, 2))
plot(model1)
dev.off()
#Multicollinearity check
vif_model1 <- vif(model1)
print(vif_model1)
#Outlier & influence check
cooksd <- cooks.distance(model1)
influential <- which(cooksd > 4 / nrow(ames))
cat("Number of influential points (Cook's D > 4/n):", length(influential), "\n")
std_resid <- rstandard(model1)
outliers <- which(abs(std_resid) > 3)
cat("Number of standardized-residual outliers (|z| > 3):", length(outliers), "\n")
plot(cooksd, type = "h", main = "Cook's Distance - Model 1",
ylab = "Cook's Distance")
abline(h = 4 / nrow(ames), col = "red", lty = 2)
#Correct issues discovered in the model
model2 <- lm(log(SalePrice) ~ Gr.Liv.Area + Total.Bsmt.SF + Year.Built +
Overall.Qual + Garage.Cars, data = ames)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
vif_model2 <- vif(model2)
print(vif_model2)
cat("Model 1 Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("Model 2 Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")
#All-subsets regression to identify the best model
candidate_vars <- c("Lot.Area", "Overall.Qual", "Overall.Cond", "Year.Built",
"Year.Remod.Add", "Mas.Vnr.Area", "Total.Bsmt.SF",
"X1st.Flr.SF", "X2nd.Flr.SF", "Gr.Liv.Area", "Full.Bath",
"Half.Bath", "Bedroom.AbvGr", "TotRms.AbvGrd",
"Fireplaces", "Garage.Cars", "Garage.Area",
"Wood.Deck.SF", "Open.Porch.SF")
subset_data <- ames[, c("SalePrice", candidate_vars)]
subset_data <- na.omit(subset_data)
regfit_full <- regsubsets(SalePrice ~ ., data = subset_data,
nvmax = length(candidate_vars), really.big = TRUE)
reg_summary <- summary(regfit_full)
par(mfrow = c(1, 3))
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2",
type = "b", main = "Adjusted R2")
best_adjr2 <- which.max(reg_summary$adjr2)
points(best_adjr2, reg_summary$adjr2[best_adjr2], col = "red", pch = 19, cex = 1.5)
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Mallows Cp",
type = "b", main = "Mallows Cp")
best_cp <- which.min(reg_summary$cp)
points(best_cp, reg_summary$cp[best_cp], col = "red", pch = 19, cex = 1.5)
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC",
type = "b", main = "BIC")
best_bic <- which.min(reg_summary$bic)
points(best_bic, reg_summary$bic[best_bic], col = "red", pch = 19, cex = 1.5)
cat("Best model size by Adj R2:", best_adjr2, "\n")
cat("Best model size by Cp:", best_cp, "\n")
cat("Best model size by BIC:", best_bic, "\n")
best_vars_bic <- names(coef(regfit_full, best_bic))[-1]
cat("Variables selected (BIC):\n")
print(best_vars_bic)
best_vars_adjr2 <- names(coef(regfit_full, best_adjr2))[-1]
cat("Variables selected (Adj R2):\n")
print(best_vars_adjr2)
formula_bic <- as.formula(paste("SalePrice ~", paste(best_vars_bic, collapse = " + ")))
model_bic <- lm(formula_bic, data = subset_data)
summary(model_bic)
#Compare preferred subset model with step 12 model
cat("Model 2 (Step 12) Adjusted R2:", summary(model2)$adj.r.squared,"\n")
cat("Best-subset (BIC) Model Adjusted R2:", summary(model_bic)$adj.r.squared,"\n")
AIC(model2, model_bic)
BIC(model2, model_bic)