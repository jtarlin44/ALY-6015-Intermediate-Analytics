#Joseph Tarlin, ALY6015, 07/17/2026
export_table <- function(df, filename) {
write.csv(df, file = file.path("output", filename), row.names = FALSE)
}
#11-1 Blood Types
observed_blood <- c(A = 12, B = 8, O = 24, AB = 6)
p_blood <- c(A = 0.20, B = 0.28, O = 0.36, AB = 0.16)       
n_blood <- sum(observed_blood)
expected_blood <- n_blood * p_blood
blood_table <- data.frame(BloodType = names(observed_blood), Observed = as.numeric(observed_blood), ExpectedProportion = as.numeric(p_blood), Expected = as.numeric(expected_blood))
print(blood_table)
alpha_blood <- 0.10
df_blood <- length(observed_blood) - 1
crit_blood <- qchisq(1 - alpha_blood, df_blood)
chisq_blood <- sum((blood_table$Observed - blood_table$Expected)^2 / blood_table$Expected)
p_val_blood <- 1 - pchisq(chisq_blood, df_blood)
cat("Critical value (df=", df_blood,", alpha=", alpha_blood,"):", round(crit_blood, 4),"\n")
cat("Chi-square test statistic:", round(chisq_blood, 4),"\n")
cat("p-value:", round(p_val_blood, 4),"\n")
cat("Decision:", ifelse(chisq_blood > crit_blood, "Reject H0", "Fail to reject H0"),"\n")
#Verify with base R test
chisq.test(x = observed_blood, p = p_blood)
#11-1 On-Time Performance by Airlines
p_airline <- c(OnTime = 0.708, NASDelay = 0.082, ArrivingLate = 0.090, Other = 0.120)
n_flights <- 200
observed_airline <- c(OnTime = 125, NASDelay = 10, ArrivingLate = 200 - 125 - 10 - 40, Other = 40)
#Records: 125 on time; 40 delayed because of weather (-> "Other" category);
#10 NAS delay; remainder arriving late
expected_airline <- n_flights * p_airline
airline_table <- data.frame(Category = names(observed_airline), Observed = as.numeric(observed_airline), ExpectedProportion = as.numeric(p_airline), Expected = as.numeric(expected_airline))
print(airline_table)
alpha_air <- 0.05
df_air <- length(observed_airline) - 1
crit_air <- qchisq(1 - alpha_air, df_air)
chisq_air <- sum((airline_table$Observed - airline_table$Expected)^2 / airline_table$Expected)
p_val_air <- 1 - pchisq(chisq_air, df_air)
cat("Critical value (df=", df_air, ", alpha=", alpha_air, "):", round(crit_air, 4),"\n")
cat("Chi-square test statistic:", round(chisq_air, 4),"\n")
cat("p-value:", round(p_val_air, 4),"\n")
cat("Decision:", ifelse(chisq_air > crit_air, "Reject H0", "Fail to reject H0"),"\n")
chisq.test(x = observed_airline, p = p_airline)
#11-2 Ethnicity and Movie Admissions
movie_matrix <- matrix(c(724, 335, 174, 107, 370, 292, 152, 140), nrow = 2, byrow = TRUE, dimnames = list(Year = c("2013", "2014"), Ethnicity = c("Caucasian", "Hispanic", "African American", "Other")))
print(movie_matrix)
alpha_movie <- 0.05
movie_test <- chisq.test(movie_matrix)
print(movie_test)
df_movie <- movie_test$parameter
crit_movie <- qchisq(1 - alpha_movie, df_movie)
cat("Critical value (df=", df_movie, ", alpha=", alpha_movie, "):", round(crit_movie, 4),"\n")
cat("Chi-square test statistic:", round(movie_test$statistic, 4),"\n")
cat("p-value:", round(movie_test$p.value, 4),"\n")
cat("Decision:", ifelse(movie_test$statistic > crit_movie, "Reject H0", "Fail to reject H0"),"\n")
#11-2 Women in the Military
military_matrix <- matrix(c(10791, 62491, 7816, 42750, 932, 9525, 11819, 54344), nrow = 4, byrow = TRUE, dimnames = list(Branch = c("Army", "Navy", "Marine Corps", "Air Force"), Rank = c("Officers", "Enlisted")))
print(military_matrix)
alpha_mil <- 0.05
military_test <- chisq.test(military_matrix)
print(military_test)
df_mil <- military_test$parameter
crit_mil <- qchisq(1 - alpha_mil, df_mil)
cat("Critical value (df=", df_mil, ", alpha=", alpha_mil, "):", round(crit_mil, 4),"\n")
cat("Chi-square test statistic:", round(military_test$statistic, 4),"\n")
cat("p-value:", round(military_test$p.value, 4),"\n")
cat("Decision:", ifelse(military_test$statistic > crit_mil, "Reject H0", "Fail to reject H0"),"\n")
#12-1 One-Way ANOVA 
#Sodium Contents of Foods, 3 groups one-way ANOVA
condiments <- c(270, 130, 230, 180, 80, 70, 200)
cereals    <- c(260, 220, 290, 290, 200, 320, 140)
desserts   <- c(100, 180, 250, 250, 300, 360, 300, 160)
sodium_df <- data.frame(sodium = c(condiments, cereals, desserts), food = factor(c(rep("Condiments", length(condiments)), rep("Cereals", length(cereals)), rep("Desserts", length(desserts)))))
#Descrptive Statistics
sodium_desc <- sodium_df %>%
group_by(food) %>%
summarise(n = n(), mean = round(mean(sodium), 2), sd = round(sd(sodium), 2), min = min(sodium), max = max(sodium))
print(sodium_desc)
sodium_aov <- aov(sodium ~ food, data = sodium_df)
sodium_anova_table <- summary(sodium_aov)
print(sodium_anova_table)
alpha_sodium <- 0.05
df_between_sodium <- sodium_anova_table[[1]]$Df[1]
df_within_sodium <- sodium_anova_table[[1]]$Df[2]
crit_sodium <- qf(1 - alpha_sodium, df_between_sodium, df_within_sodium)
f_sodium <- sodium_anova_table[[1]]$`F value`[1]
p_sodium <- sodium_anova_table[[1]]$`Pr(>F)`[1]
cat("Critical F value (df1=", df_between_sodium, ", df2=", df_within_sodium, "):", round(crit_sodium, 4),"\n")
cat("F test statistic:", round(f_sodium, 4),"\n")
cat("p-value:", round(p_sodium, 4),"\n")
cat("Decision:", ifelse(f_sodium > crit_sodium, "Reject H0", "Fail to reject H0"),"\n")
ggplot(sodium_df, aes(x = food, y = sodium, fill = food)) +
geom_boxplot() +
labs(title = "Sodium Content by Food Category", x = "Food Category", y = "Sodium (mg)") +
theme_minimal() + theme(legend.position = "none")
#12-2 Sales for Leading Companies 
cereal_sales <- c(578, 320, 264, 249, 237)
candy_sales <- c(311, 106, 109, 125, 173)
coffee_sales <- c(261, 185, 302, 689)
sales_df <- data.frame(
sales = c(cereal_sales, candy_sales, coffee_sales),
category = factor(c(rep("Cereal", length(cereal_sales)),
rep("Chocolate Candy", length(candy_sales)),
rep("Coffee", length(coffee_sales)))))
sales_desc <- sales_df %>%
group_by(category) %>%
summarise(n = n(), mean = round(mean(sales), 2), sd = round(sd(sales), 2),
min = min(sales), max = max(sales))
print(sales_desc)
sales_aov <- aov(sales ~ category, data = sales_df)
sales_anova_table <- summary(sales_aov)
print(sales_anova_table)
alpha_sales <- 0.01
df_between_sales <- sales_anova_table[[1]]$Df[1]
df_within_sales <- sales_anova_table[[1]]$Df[2]
crit_sales <- qf(1 - alpha_sales, df_between_sales, df_within_sales)
f_sales <- sales_anova_table[[1]]$`F value`[1]
p_sales <- sales_anova_table[[1]]$`Pr(>F)`[1]
cat("Critical F value (df1=", df_between_sales, ", df2=", df_within_sales, "):", round(crit_sales, 4),"\n")
cat("F test statistic:", round(f_sales, 4),"\n")
cat("p-value:", round(p_sales, 4),"\n")
cat("Decision:", ifelse(f_sales > crit_sales, "Reject H0", "Fail to reject H0"),"\n")
sales_anova_export <- tibble::rownames_to_column(as.data.frame(sales_anova_table[[1]]), "Source")
if (f_sales > crit_sales) {
sales_tukey <- TukeyHSD(sales_aov)
print(sales_tukey)
ggplot(sales_df, aes(x = category, y = sales, fill = category)) +
geom_boxplot() +
labs(title = "Annual Sales by Product Category", x = "Category", y = "Sales ($ millions)") +
theme_minimal() + theme(legend.position = "none")
#12-2 Per-Pupil Expenditures 
eastern <- c(4946, 5953, 6202, 7243, 6113)
middle  <- c(6149, 7451, 6000, 6479)
western <- c(5282, 8605, 6528, 6911)
pupil_df <- data.frame(
expenditure = c(eastern, middle, western),
region = factor(c(rep("Eastern third", length(eastern)),
rep("Middle third", length(middle)),
rep("Western third", length(western)))))
pupil_desc <- pupil_df %>%
group_by(region) %>%
summarise(n = n(), mean = round(mean(expenditure), 2), sd = round(sd(expenditure), 2),
min = min(expenditure), max = max(expenditure))
print(pupil_desc)
pupil_aov <- aov(expenditure ~ region, data = pupil_df)
pupil_anova_table <- summary(pupil_aov)
print(pupil_anova_table)
alpha_pupil <- 0.05
df_between_pupil <- pupil_anova_table[[1]]$Df[1]
df_within_pupil <- pupil_anova_table[[1]]$Df[2]
crit_pupil <- qf(1 - alpha_pupil, df_between_pupil, df_within_pupil)
f_pupil <- pupil_anova_table[[1]]$`F value`[1]
p_pupil <- pupil_anova_table[[1]]$`Pr(>F)`[1]
cat("Critical F value (df1=", df_between_pupil, ", df2=", df_within_pupil, "):", round(crit_pupil, 4),"\n")
cat("F test statistic:", round(f_pupil, 4),"\n")
cat("p-value:", round(p_pupil, 4),"\n")
cat("Decision:", ifelse(f_pupil > crit_pupil, "Reject H0", "Fail to reject H0"),"\n")
pupil_anova_export <- tibble::rownames_to_column(as.data.frame(pupil_anova_table[[1]]), "Source")
if (f_pupil > crit_pupil) {
pupil_tukey <- TukeyHSD(pupil_aov)
print(pupil_tukey)
ggplot(pupil_df, aes(x = region, y = expenditure, fill = region)) +
geom_boxplot() +
labs(title = "Per-Pupil Expenditures by Region", x = "Region", y = "Expenditure ($)") +
theme_minimal() + theme(legend.position = "none")
#12-3 Two-Way ANOVA Plant Growth
plant_df <- data.frame(
growth = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6),
food   = factor(rep(c("A", "A", "B", "B"), each = 3)),
light  = factor(rep(c("Light1", "Light2"), each = 3, times = 2)))
print(plant_df)
plant_cellmeans <- plant_df %>%
group_by(food, light) %>%
summarise(mean_growth = round(mean(growth), 3), .groups = "drop")
print(plant_cellmeans)
plant_aov <- aov(growth ~ food * light, data = plant_df)
plant_anova_table <- summary(plant_aov)
print(plant_anova_table)
plant_anova_export <- tibble::rownames_to_column(as.data.frame(plant_anova_table[[1]]), "Source")
alpha_plant <- 0.05
df_food <- plant_anova_table[[1]]$Df[1]
df_light <- plant_anova_table[[1]]$Df[2]
df_interact <- plant_anova_table[[1]]$Df[3]
df_error_plant <- plant_anova_table[[1]]$Df[4]
crit_food <- qf(1 - alpha_plant, df_food, df_error_plant)
crit_light <- qf(1 - alpha_plant, df_light, df_error_plant)
crit_interact <- qf(1 - alpha_plant, df_interact, df_error_plant)
cat("Critical F (Food):", round(crit_food, 4), " | F stat:", round(plant_anova_table[[1]]$`F value`[1], 4),
" | p:", round(plant_anova_table[[1]]$`Pr(>F)`[1], 4),"\n")
cat("Critical F (Light):", round(crit_light, 4), " | F stat:", round(plant_anova_table[[1]]$`F value`[2], 4),
" | p:", round(plant_anova_table[[1]]$`Pr(>F)`[2], 4),"\n")
cat("Critical F (Interaction):", round(crit_interact, 4), " | F stat:", round(plant_anova_table[[1]]$`F value`[3], 4),
" | p:", round(plant_anova_table[[1]]$`Pr(>F)`[3], 4),"\n")
interaction.plot(plant_df$light, plant_df$food, plant_df$growth,
col = c("darkblue", "darkred"), lwd = 2, type = "b", pch = c(16, 17),
xlab = "Grow-Light Strength", ylab = "Mean Plant Growth (inches)",
trace.label = "Plant Food", main = "Interaction Plot: Plant Growth by Food and Light")
#Baseball 
bb <- read.csv("baseball__1_.csv", stringsAsFactors = FALSE)
cat("Dimensions of baseball data:", dim(bb)[1], "rows x", dim(bb)[2], "columns\n")
str(bb)
#Descriptive statistics table for key numeric variables
bb_desc <- bb %>%
select(W, RS, RA, OBP, SLG, BA) %>%
pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
group_by(Variable) %>%
summarise(n = sum(!is.na(value)),
mean = round(mean(value, na.rm = TRUE), 3),
sd = round(sd(value, na.rm = TRUE), 3),
min = round(min(value, na.rm = TRUE), 3),
max = round(max(value, na.rm = TRUE), 3))
print(bb_desc)
#Frequency table teams per league
league_table <- bb %>% count(League)
print(league_table)
#Playoff appearances 
playoff_table <- bb %>% count(Playoffs)
print(playoff_table)
#Correlation matrix limited to 5 or fewer variables
bb_corr_vars <- bb %>% select(W, RS, RA, OBP, SLG) %>% na.omit()
bb_corr_matrix <- round(cor(bb_corr_vars), 3)
print(bb_corr_matrix)
ggplot(bb, aes(x = W)) +
geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
labs(title = "Distribution of Team Wins (1962-2012)", x = "Wins", y = "Frequency") +
theme_minimal()
ggplot(bb, aes(x = RS, y = W)) +
geom_point(alpha = 0.5, color = "darkgreen") +
geom_smooth(method = "lm", se = FALSE, color = "red") +
labs(title = "Wins vs Runs Scored", x = "Runs Scored", y = "Wins") +
theme_minimal()
ggplot(bb, aes(x = League, y = W, fill = League)) +
geom_boxplot() +
labs(title = "Wins by League", x = "League", y = "Wins") +
theme_minimal()
#Chi-square goodness of fit wins by decade
bb$Decade <- bb$Year - (bb$Year %% 10)
wins <- bb %>%
group_by(Decade) %>%
summarize(wins = sum(W)) %>%
as_tibble()
print(wins)
k_decades <- nrow(wins)
expected_wins <- rep(sum(wins$wins) / k_decades, k_decades)
alpha_bb <- 0.05
df_bb <- k_decades - 1
crit_bb <- qchisq(1 - alpha_bb, df_bb)
chisq_bb <- sum((wins$wins - expected_wins)^2 / expected_wins)
bb_gof_test <- chisq.test(x = wins$wins, p = rep(1 / k_decades, k_decades))
print(bb_gof_test)
cat("Number of decades (categories):", k_decades,"\n")
cat("Critical value (df=", df_bb, ", alpha=", alpha_bb, "):", round(crit_bb, 4),"\n")
cat("Chi-square test statistic:", round(chisq_bb, 4),"\n")
cat("p-value (manual):", round(1 - pchisq(chisq_bb, df_bb), 6),"\n")
cat("p-value (R chisq.test):", format.pval(bb_gof_test$p.value),"\n")
cat("Decision:", ifelse(chisq_bb > crit_bb, "Reject H0", "Fail to reject H0"),"\n")
wins_expected_table <- wins %>% mutate(expected = round(expected_wins, 2))
ggplot(wins, aes(x = factor(Decade), y = wins)) +
geom_col(fill = "tomato") +
geom_hline(yintercept = mean(wins$wins), linetype = "dashed", color = "black") +
labs(title = "Total Wins by Decade", x = "Decade", y = "Total Wins") +
theme_minimal()
#Crop data two-way ANOVA
crop <- read.csv("crop_data.csv", stringsAsFactors = FALSE)
str(crop)
crop$density <- as.factor(crop$density)
crop$fertilizer <- as.factor(crop$fertilizer)
crop$block <- as.factor(crop$block)
cat("Levels of density:", levels(crop$density),"\n")
cat("Levels of fertilizer:", levels(crop$fertilizer),"\n")
cat("Levels of block:", levels(crop$block),"\n")
crop_desc <- crop %>%
group_by(fertilizer, density) %>%
summarise(n = n(), mean_yield = round(mean(yield), 3), sd_yield = round(sd(yield), 3), .groups = "drop")
print(crop_desc)
crop_aov <- aov(yield ~ fertilizer * density, data = crop)
crop_anova_table <- summary(crop_aov)
print(crop_anova_table)
crop_anova_export <- tibble::rownames_to_column(as.data.frame(crop_anova_table[[1]]), "Source")
alpha_crop <- 0.05
df_fert <- crop_anova_table[[1]]$Df[1]
df_dens <- crop_anova_table[[1]]$Df[2]
df_fd_int <- crop_anova_table[[1]]$Df[3]
df_error_crop <- crop_anova_table[[1]]$Df[4]
crit_fert <- qf(1 - alpha_crop, df_fert, df_error_crop)
crit_dens <- qf(1 - alpha_crop, df_dens, df_error_crop)
crit_fd_int <- qf(1 - alpha_crop, df_fd_int, df_error_crop)
cat("Critical F (Fertilizer):", round(crit_fert, 4), " | F stat:", round(crop_anova_table[[1]]$`F value`[1], 4),
" | p:", round(crop_anova_table[[1]]$`Pr(>F)`[1], 6),"\n")
cat("Critical F (Density):", round(crit_dens, 4), " | F stat:", round(crop_anova_table[[1]]$`F value`[2], 4),
" | p:", round(crop_anova_table[[1]]$`Pr(>F)`[2], 6),"\n")
cat("Critical F (Interaction):", round(crit_fd_int, 4), " | F stat:", round(crop_anova_table[[1]]$`F value`[3], 4),
" | p:", round(crop_anova_table[[1]]$`Pr(>F)`[3], 6),"\n")
interaction.plot(crop$density, crop$fertilizer, crop$yield,
col = c("darkblue", "darkred", "darkgreen"), lwd = 2, type = "b", pch = c(16, 17, 18),
xlab = "Density", ylab = "Mean Yield", trace.label = "Fertilizer",
main = "Interaction Plot: Yield by Fertilizer and Density")
ggplot(crop, aes(x = fertilizer, y = yield, fill = density)) +
geom_boxplot() +
labs(title = "Crop Yield by Fertilizer and Density", x = "Fertilizer", y = "Yield") +
theme_minimal()