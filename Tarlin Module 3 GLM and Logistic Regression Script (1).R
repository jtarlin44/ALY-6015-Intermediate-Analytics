#Joseph Tarlin, ALY 6015, 07/22/2026
data("College")
str(College)
sum(is.na(College))
table(College$Private)
prop.table(table(College$Private))
#Exploratory data analysis
key_vars <- c("Outstate", "F.Undergrad", "S.F.Ratio", "perc.alumni", "Grad.Rate")
desc_stats <- data.frame(Variable = key_vars,
Mean = sapply(College[key_vars], mean),
Median = sapply(College[key_vars], median),
SD = sapply(College[key_vars], sd),
Min = sapply(College[key_vars], min),
Max = sapply(College[key_vars], max),
row.names = NULL)
desc_stats[, 2:6] <- round(desc_stats[, 2:6], 1)
kable(desc_stats, caption = "Descriptive Statistics for Key Numeric Variables")
#Statistics split by private/public
desc_by_group <- College %>%
group_by(Private) %>%
summarise(across(all_of(key_vars),
list(mean = mean, median = median, sd = sd),
.names = "{.col}_{.fn}"))
kable(desc_by_group, caption = "Descriptive Statistics by Institution Type")
#Bar chart of private vs public counts
ggplot(College, aes(x = Private, fill = Private)) +
geom_bar() +
labs(title = "Count of Private vs. Public Universities",
x = "Private", y = "Count") +
theme_minimal()
#Boxplot for out of state tuition by institution type
ggplot(College, aes(x = Private, y = Outstate, fill = Private)) +
geom_boxplot() +
labs(title = "Out-of-State Tuition by Institution Type",
x = "Private", y = "Out-of-State Tuition ($)") +
theme_minimal()
#Boxplot for full-time undergraduate enrollment by institution type
ggplot(College, aes(x = Private, y = F.Undergrad, fill = Private)) +
geom_boxplot() +
labs(title = "Full-Time Undergraduates by Institution Type",
x = "Private", y = "Full-Time Undergraduates") +
theme_minimal()
#Correlation matrix limited to 5 variables, response recoded as 0/1
College_corr <- College %>%
mutate(PrivateBin = ifelse(Private == "Yes", 1, 0)) %>%
select(PrivateBin, Outstate, F.Undergrad, S.F.Ratio, perc.alumni)
corr_matrix <- round(cor(College_corr), 2)
kable(corr_matrix, caption = "Correlation Matrix (5 Variables)")
#Train/test split, 70/30
set.seed(42)
trainIndex <- createDataPartition(College$Private, p = 0.7, list = FALSE, times = 1)
train <- College[trainIndex, ]
test <- College[-trainIndex, ]
nrow(train); nrow(test)
prop.table(table(train$Private))
prop.table(table(test$Private))
#Fit logistic regression model glm
model <- glm(Private ~ Outstate + F.Undergrad + S.F.Ratio + perc.alumni,
data = train, family = binomial(link = "logit"))
model_coefs <- tidy(model, conf.int = TRUE, exponentiate = FALSE)
kable(model_coefs, digits = 4, caption = "Logistic Regression Coefficients")
model_fit <- glance(model)
kable(model_fit, digits = 2, caption = "Model Fit Statistics")
odds_ratios <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
select(term, estimate, conf.low, conf.high)
names(odds_ratios) <- c("Term", "Odds Ratio", "CI Lower", "CI Upper")
kable(odds_ratios, digits = 3, caption = "Odds Ratios (95% CI)")
#Confusion matrix training set
train_prob <- predict(model, newdata = train, type = "response")
train_pred <- factor(ifelse(train_prob >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
cm_train <- confusionMatrix(train_pred, train$Private, positive = "Yes")
cm_train
cm_train_table <- as.data.frame.matrix(cm_train$table)
kable(cm_train_table, caption = "Confusion Matrix - Training Set")
#Accuracy, precision, recall, specificity
train_metrics <- data.frame(Metric = c("Accuracy", "Precision", "Recall", "Specificity"),
Value  = c(cm_train$overall["Accuracy"],
cm_train$byClass["Precision"],
cm_train$byClass["Recall"],
cm_train$byClass["Specificity"]))
train_metrics$Value <- round(train_metrics$Value, 3)
kable(train_metrics, caption = "Training Set Performance Metrics")
#Confusion matrix test set
test_prob <- predict(model, newdata = test, type = "response")
test_pred <- factor(ifelse(test_prob >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
cm_test <- confusionMatrix(test_pred, test$Private, positive = "Yes")
cm_test
cm_test_table <- as.data.frame.matrix(cm_test$table)
kable(cm_test_table, caption = "Confusion Matrix - Test Set")
test_metrics <- data.frame(Metric = c("Accuracy", "Precision", "Recall", "Specificity"),
Value  = c(cm_test$overall["Accuracy"],
cm_test$byClass["Precision"],
cm_test$byClass["Recall"],
cm_test$byClass["Specificity"]))
test_metrics$Value <- round(test_metrics$Value, 3)
kable(test_metrics, caption = "Test Set Performance Metrics")
#ROC Curve and AUC
roc_obj <- roc(response = test$Private, predictor = test_prob, levels = c("No", "Yes"))
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve - Test Set")
abline(a = 0, b = 1, lty = 2, col = "gray")
auc_value <- auc(roc_obj)
auc_value
auc_table <- data.frame(Metric = "AUC", Value = round(as.numeric(auc_value), 3))
kable(auc_table, caption = "Area Under the ROC Curve")