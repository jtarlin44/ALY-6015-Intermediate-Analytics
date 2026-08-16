# Yingkai Ren
# ALY 6015 
# July 28, 2026

#Clean canvas ----
# clears console
cat("\014")
# clears global environment
rm(list = ls())
# clears plots
graphics.off()
# clears packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
# disables scientific notation for entire R session
options(scipen = 100)

# 1. Load Required Libraries
library(tidyverse)
library(broom)
library(pROC)

# 2. Load the Cleaned Dataset
# Ensure your working directory contains the merged/cleaned file
df <- read.csv("d311_clean_analysis_file.csv", stringsAsFactors = FALSE)

# 3. Feature Engineering & Transformations (Using actual column names)
df <- df %>%
  mutate(
    # Convert grouping and control variables to factors
    income_group = as.factor(income_group),
    department = as.factor(department),
    complaint_type = as.factor(type),
    complaint_reason = as.factor(reason),
    # Use 'on_time' or 'overdue' as the compliance/urgency control for RQ3
    compliance_status = as.factor(on_time) 
  )

# 4. Summary Statistics Table by Income Tier (Mean and SD format matching course examples)
summary_table <- df %>%
  group_by(income_group) %>%
  summarise(
    N = n(),
    Mean_Response_Hours = round(mean(response_hours, na.rm = TRUE), 2),
    SD_Response_Hours = round(sd(response_hours, na.rm = TRUE), 2),
    Mean_Poverty = round(mean(poverty_rate, na.rm = TRUE), 2),
    SD_Poverty = round(sd(poverty_rate, na.rm = TRUE), 2)
  )

cat("\n=== Table 1. Summary Statistics by Income Tier ===\n")
print(summary_table)
write.csv(summary_table, "Table1_SummaryStats_IncomeTier.csv", row.names = FALSE)

# 5. RQ3 Analysis: Controlling for Complaint Type and Compliance/Urgency
# Model 1: Base model (Poverty & Income only)
m_base <- lm(response_hours ~ poverty_rate + income_group, data = df)

# Model 2: Controlled model (Adding compliance status and complaint type)
m_controlled <- lm(response_hours ~ poverty_rate + income_group + compliance_status + complaint_type, data = df)

cat("\n=== RQ3: Base Model Summary ===\n")
print(summary(m_base))

cat("\n=== RQ3: Controlled Model Summary ===\n")
print(summary(m_controlled))

# Export RQ3 regression results as a structured CSV table for reporting
rq3_results <- bind_rows(
  tidy(m_base) %>% mutate(Model = "Base Model"),
  tidy(m_controlled) %>% mutate(Model = "Controlled Model")
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

write.csv(rq3_results, "Table2_RQ3_Regression_Results.csv", row.names = FALSE)

# 6. RQ4 Analysis: Departmental Differences (Interaction Models)
# Model 3: Interaction between Income/Poverty tiers and Municipal Departments
m_dept_int <- lm(response_hours ~ (poverty_rate + income_group) * department, data = df)

cat("\n=== RQ4: Department Interaction Model Summary ===\n")
print(summary(m_dept_int))

# Export RQ4 interaction results
rq4_results <- tidy(m_dept_int) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

write.csv(rq4_results, "Table3_RQ4_Interaction_Results.csv", row.names = FALSE)

# 7. Visualization & Export (High-Resolution PNG)
plot_data <- df %>%
  filter(!is.na(department) & !is.na(income_group) & !is.na(response_hours)) %>%
  group_by(department, income_group) %>%
  summarise(mean_response = mean(response_hours, na.rm = TRUE), .groups = 'drop')

p1 <- ggplot(plot_data, aes(x = reorder(department, mean_response), y = mean_response, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Figure 1. Mean 311 Response Time by Department and Income Tier",
    subtitle = "Evaluating operational uniformity across municipal units (RQ4)",
    x = "Municipal Department",
    y = "Mean Response Time (Hours)",
    fill = "Income Tier"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Save plot to file
png("Figure1_ResponseTime_by_Department.png", width = 1200, height = 900, res = 150)
print(p1)
dev.off()

cat("\n=== Analysis complete. All tables and figures exported successfully. ===\n")

