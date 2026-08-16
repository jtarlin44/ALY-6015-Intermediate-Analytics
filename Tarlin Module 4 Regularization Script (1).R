#Joseph Tarlin, ALY 6015, 07/28/2026, Regularization Assignment Module 4
set.seed(123)
data(College)
College$Private <- as.factor(College$Private)
str(College)
sum(is.na(College))
#Descriptive Statistics
numeric_vars <- College[, sapply(College, is.numeric)]
desc_stats <- data.frame(Variable = names(numeric_vars),
Mean = round(sapply(numeric_vars, mean, na.rm = TRUE), 2),
SD = round(sapply(numeric_vars, sd, na.rm = TRUE), 2),
Min = round(sapply(numeric_vars, min, na.rm = TRUE), 2),
Median = round(sapply(numeric_vars, median, na.rm = TRUE), 2),
Max = round(sapply(numeric_vars, max, na.rm = TRUE), 2),
row.names = NULL)
print(desc_stats)
#Correlation matrix with 5 variables
corr_vars <- College[, c("Grad.Rate", "perc.alumni", "S.F.Ratio", "Top25perc", "Outstate")]
corr_matrix <- round(cor(corr_vars), 2)
print(corr_matrix)
corrplot(cor(corr_vars), method = "color", type = "upper", addCoef.col = "black",
tl.col = "black", tl.srt = 45, number.cex = 1.1,
title = "Correlation Matrix: Grad.Rate and Top Predictors", mar = c(0,0,2,0))
#Train / test split, 70 / 30
trainIndex <- sample(x = nrow(College), size = nrow(College) * 0.7)
train <- College[trainIndex, ]
test <- College[-trainIndex, ]
cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")
train_x <- model.matrix(Grad.Rate ~ ., data = train)[, -1]
test_x <- model.matrix(Grad.Rate ~ ., data = test)[, -1]
train_y <- train$Grad.Rate
test_y <- test$Grad.Rate
#Ridge regression
set.seed(123)
cv_ridge <- cv.glmnet(train_x, train_y, alpha = 0)
print(cv_ridge)
ridge_lambda_min <- cv_ridge$lambda.min
ridge_lambda_1se <- cv_ridge$lambda.1se
cat("Ridge lambda.min:", ridge_lambda_min, "\n")
cat("Ridge lambda.1se:", ridge_lambda_1se, "\n")
plot(cv_ridge)
title("Ridge Regression - CV MSE vs Log(Lambda)", line = 2.5)
ridge_model <- glmnet(train_x, train_y, alpha = 0, lambda = ridge_lambda_min)
#Predictions
ridge_pred_train <- predict(ridge_model, newx = train_x)
ridge_pred_test <- predict(ridge_model, newx = test_x)
ridge_rmse_train <- sqrt(mean((train_y - ridge_pred_train)^2))
ridge_rmse_test <- sqrt(mean((test_y - ridge_pred_test)^2))
cat("Ridge RMSE (train):", ridge_rmse_train, "\n")
cat("Ridge RMSE (test):", ridge_rmse_test, "\n")
#Lasso regression
set.seed(123)
cv_lasso <- cv.glmnet(train_x, train_y, alpha = 1)
print(cv_lasso)
lasso_lambda_min <- cv_lasso$lambda.min
lasso_lambda_1se <- cv_lasso$lambda.1se
cat("LASSO lambda.min:", lasso_lambda_min, "\n")
cat("LASSO lambda.1se:", lasso_lambda_1se, "\n")
plot(cv_lasso)
title("LASSO Regression - CV MSE vs Log(Lambda)", line = 2.5)
lasso_model <- glmnet(train_x, train_y, alpha = 1, lambda = lasso_lambda_min)
print(coef(lasso_model))
lasso_pred_train <- predict(lasso_model, newx = train_x)
lasso_pred_test <- predict(lasso_model, newx = test_x)
lasso_rmse_train <- sqrt(mean((train_y - lasso_pred_train)^2))
lasso_rmse_test <- sqrt(mean((test_y - lasso_pred_test)^2))
cat("LASSO RMSE (train):", lasso_rmse_train, "\n")
cat("LASSO RMSE (test):", lasso_rmse_test, "\n")
#Stepwise selection comparison
full_model <- lm(Grad.Rate ~ ., data = train)
step_model <- step(full_model, direction = "both", trace = 0)
cat("Stepwise formula:\n")
print(formula(step_model))
#Exporting stepwise coefficient table
step_coef_table <- as.data.frame(coef(summary(step_model)))
step_coef_table <- data.frame(Predictor = rownames(step_coef_table), round(step_coef_table, 4))
rownames(step_coef_table) <- NULL
names(step_coef_table) <- c("Predictor", "Estimate", "Std.Error", "t.value", "p.value")
print(step_coef_table)
cat("Stepwise Adjusted R-squared:", summary(step_model)$adj.r.squared, "\n")
step_pred_train <- predict(step_model, newdata = train)
step_pred_test <- predict(step_model, newdata = test)
step_rmse_train <- sqrt(mean((train_y - step_pred_train)^2))
step_rmse_test <- sqrt(mean((test_y - step_pred_test)^2))
cat("Stepwise RMSE (train):", step_rmse_train, "\n")
cat("Stepwise RMSE (test):", step_rmse_test, "\n")
#Summary table
results <- data.frame(
Model = c("Ridge", "LASSO", "Stepwise"),
Lambda_min = c(ridge_lambda_min, lasso_lambda_min, NA),
RMSE_Train = c(ridge_rmse_train, lasso_rmse_train, step_rmse_train),
RMSE_Test  = c(ridge_rmse_test, lasso_rmse_test, step_rmse_test))
print(results)
ridge_coefs <- as.data.frame(as.matrix(coef(ridge_model)))
lasso_coefs <- as.data.frame(as.matrix(coef(lasso_model)))
write.csv(ridge_coefs, "ridge_coefs.csv")
write.csv(lasso_coefs, "lasso_coefs.csv")