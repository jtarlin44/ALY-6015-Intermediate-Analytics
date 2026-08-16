# Yingkai Ren
# ALY 6015 
# July 19, 2026

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

library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(broom)
library(knitr)

setwd("E:/01 NEU xuexi/ALY6015/Final Proj")

## ------------------------------------------------------------
## 1. LOAD AND ROW-BIND FIVE YEARS OF 311 DATA
## ------------------------------------------------------------
## Only the columns we actually need are loaded (col_select) to keep
## memory usage manageable -- the raw files also include photo URLs
## and a WKT geometry string we don't need for this analysis.

needed_cols <- c("case_enquiry_id", "open_dt", "closed_dt", "on_time",
                 "case_status", "subject", "reason", "type", "queue",
                 "department", "neighborhood", "source")

years <- 2015:2019
file_names <- paste0("311 SERVICE REQUESTS - ", years, ".csv")

read_year <- function(f) {
  read_csv(f, col_select = all_of(needed_cols),
           col_types = cols(.default = "c"))   # read as character first,
}                                                 # convert types after bind

d_list <- lapply(file_names, read_year)
d311 <- bind_rows(d_list)          # <-- REQUIRED METHOD (not merge())

rm(d_list); gc()

cat("Total rows after binding 2015-2019:", nrow(d311), "\n")

## ------------------------------------------------------------
## 2. CLEAN TYPES / DATES
## ------------------------------------------------------------
d311 <- d311 %>%
  mutate(
    open_dt   = ymd_hms(open_dt),
    closed_dt = ymd_hms(closed_dt),
    year      = year(open_dt),
    month     = month(open_dt),
    season    = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  )

## ------------------------------------------------------------
## 3. NEIGHBORHOOD CROSSWALK  (311 labels -> Census labels)
## ------------------------------------------------------------
## The 311 "neighborhood" field uses some combined/informal labels
## (e.g. "Allston / Brighton") that don't exist in the Census table.
## We map each 311 label to one or more Census neighborhoods; when a
## 311 label spans multiple Census neighborhoods we later take a
## population-weighted average of their Census characteristics.
## Rows with no identifiable neighborhood ("Boston", blank, or
## "Chestnut Hill", which the Census table does not break out
## separately) are excluded from the neighborhood-level analysis.

crosswalk <- tribble(
  ~neighborhood,                                    ~neighborhood_census,
  "Allston",                                         "Allston",
  "Back Bay",                                        "Back Bay",
  "Beacon Hill",                                     "Beacon Hill",
  "Brighton",                                        "Brighton",
  "Charlestown",                                     "Charlestown",
  "Dorchester",                                      "Dorchester",
  "East Boston",                                     "East Boston",
  "Hyde Park",                                       "Hyde Park",
  "Jamaica Plain",                                   "Jamaica Plain",
  "Mattapan",                                         "Mattapan",
  "Mission Hill",                                     "Mission Hill",
  "Roslindale",                                       "Roslindale",
  "Roxbury",                                          "Roxbury",
  "South Boston",                                     "South Boston",
  "South End",                                        "South End",
  "West Roxbury",                                     "West Roxbury",
  "Allston / Brighton",                                "Allston",
  "Allston / Brighton",                                "Brighton",
  "Downtown / Financial District",                     "Downtown",
  "Fenway / Kenmore / Audubon Circle / Longwood",       "Fenway",
  "Fenway / Kenmore / Audubon Circle / Longwood",       "Longwood",
  "Greater Mattapan",                                   "Mattapan",
  "South Boston / South Boston Waterfront",             "South Boston",
  "South Boston / South Boston Waterfront",             "South Boston Waterfront"
)

## ------------------------------------------------------------
## 3b. EXTRACT CENSUS DATA DIRECTLY FROM THE BPDA .xlsm WORKBOOK
## ------------------------------------------------------------
## The workbook has one sheet per Census topic. Each sheet has the
## same layout: a title row, a blank/spacer row, a header row, then
## data rows starting with "United States", "Massachusetts", "Boston",
## and then each of Boston's 22 official planning neighborhoods,
## followed by a few trailing "Source:" / "Table ID:" footnote rows.
## We skip the first 3 rows (title, spacer, header) and pull columns
## by fixed position, then keep only rows whose first column matches
## one of the 22 known neighborhood names -- this automatically drops
## United States/Massachusetts/Boston (city-wide) and the footnotes.

xlsm_path <- "2015-2019_neighborhood_tables_2021.12.21.xlsm"

valid_neighborhoods <- c("Allston", "Back Bay", "Beacon Hill", "Brighton", "Charlestown",
                         "Dorchester", "Downtown", "East Boston", "Fenway", "Hyde Park",
                         "Jamaica Plain", "Longwood", "Mattapan", "Mission Hill", "North End",
                         "Roslindale", "Roxbury", "South Boston", "South Boston Waterfront",
                         "South End", "West End", "West Roxbury")

## Helper: read one sheet, keep only the columns we need (by position),
## rename them, and restrict rows to the 22 valid neighborhoods.
read_bpda_sheet <- function(sheet, col_positions, col_names) {
  raw <- read_excel(xlsm_path, sheet = sheet, skip = 3, col_names = FALSE)
  raw <- raw[, col_positions]
  names(raw) <- col_names
  raw %>%
    mutate(neighborhood_census = trimws(neighborhood_census)) %>%
    filter(neighborhood_census %in% valid_neighborhoods)
}

age_tbl  <- read_bpda_sheet("Age",
                            c(1, 2, 3),
                            c("neighborhood_census", "total_population", "median_age"))

race_tbl <- read_bpda_sheet("Race",
                            c(1, 4, 6, 8, 10),
                            c("neighborhood_census", "pct_white", "pct_black", "pct_hispanic", "pct_asian"))

hhinc_tbl <- read_bpda_sheet("Household Income",
                             c(1, 2),
                             c("neighborhood_census", "median_household_income"))

pci_tbl  <- read_bpda_sheet("Per Capita Income",
                            c(1, 4),
                            c("neighborhood_census", "per_capita_income"))

pov_tbl  <- read_bpda_sheet("Poverty Rates",
                            c(1, 4),
                            c("neighborhood_census", "poverty_rate"))

## Combine the five topic tables into one neighborhood-level Census profile
census <- age_tbl %>%
  left_join(race_tbl, by = "neighborhood_census") %>%
  left_join(hhinc_tbl, by = "neighborhood_census") %>%
  left_join(pci_tbl, by = "neighborhood_census") %>%
  left_join(pov_tbl, by = "neighborhood_census")

stopifnot(nrow(census) == 22)   # sanity check: all 22 neighborhoods present

## Population-weighted Census profile for every 311 neighborhood label
census_weighted <- crosswalk %>%
  left_join(census, by = "neighborhood_census") %>%
  group_by(neighborhood) %>%
  summarise(
    median_age               = weighted.mean(median_age, total_population),
    pct_white                = weighted.mean(pct_white, total_population),
    pct_black                = weighted.mean(pct_black, total_population),
    pct_hispanic             = weighted.mean(pct_hispanic, total_population),
    pct_asian                = weighted.mean(pct_asian, total_population),
    median_household_income  = weighted.mean(median_household_income, total_population),
    per_capita_income        = weighted.mean(per_capita_income, total_population),
    poverty_rate             = weighted.mean(poverty_rate, total_population),
    total_population         = sum(total_population)
  ) %>%
  ungroup()

## ------------------------------------------------------------
## 4. MERGE 311 DATA WITH CENSUS PROFILE  (horizontal merge - OK here,
##    this is the *optional supplemental* dataset, not the 5 yearly files)
## ------------------------------------------------------------
d311 <- d311 %>%
  filter(!neighborhood %in% c("Boston", "Chestnut Hill", " ", NA)) %>%
  left_join(census_weighted, by = "neighborhood")

## ------------------------------------------------------------
## 5. FEATURE ENGINEERING
## ------------------------------------------------------------
d311 <- d311 %>%
  mutate(
    ## --- Outcome 1: response/closure time in hours (closed cases only) ---
    response_hours = as.numeric(difftime(closed_dt, open_dt, units = "hours")),
    
    ## --- Outcome 2: binary overdue flag (from the SLA the City itself sets) ---
    overdue = case_when(
      on_time == "OVERDUE" ~ 1,
      on_time == "ONTIME"  ~ 0,
      TRUE ~ NA_real_
    ),
    
    ## --- Predictor: collapse 11 raw "subject" (dept.) values into
    ##     6 broader categories so the regression has manageable levels ---
    dept_group = case_when(
      subject %in% c("Public Works Department", "Boston Water & Sewer Commission") ~ "Public Works/Water",
      subject == "Transportation - Traffic Division" ~ "Transportation",
      subject == "Inspectional Services" ~ "Inspectional Services",
      subject == "Parks & Recreation Department" ~ "Parks & Recreation",
      subject %in% c("Animal Control", "Property Management",
                     "Boston Police Department", "Consumer Affairs & Licensing",
                     "Neighborhood Services") ~ "Other City Services",
      subject == "Mayor's 24 Hour Hotline" ~ "Mayor's Hotline",
      TRUE ~ "Other"
    ),
    
    ## --- Predictor: community income tier (feature engineering: continuous -> binary) ---
    income_group = if_else(median_household_income >= median(median_household_income, na.rm = TRUE),
                           "Higher-income neighborhood", "Lower-income neighborhood"),
    
    ## --- Predictor: racial composition summary ---
    pct_minority = 1 - pct_white,
    
    ## --- Predictor: submission channel, collapsed to 4 categories ---
    channel_group = case_when(
      source == "Constituent Call" ~ "Phone",
      source %in% c("Citizens Connect App", "City Worker App") ~ "Mobile App",
      source == "Self Service" ~ "Web/Self-Service",
      TRUE ~ "Other/Internal"
    ),
    
    year = factor(year),
    dept_group = factor(dept_group),
    channel_group = factor(channel_group),
    income_group = factor(income_group)
  )

## Drop implausible durations (data entry errors: negative or >1 year)
d_model <- d311 %>%
  filter(case_status == "Closed",
         !is.na(response_hours),
         response_hours >= 0,
         response_hours <= 24 * 365)

## Collapse the near-empty "Other" dept category into "Other City Services"
## -- it caused a quasi-separation problem in the logistic regression
## (see Section 7): almost no variation in outcome, so the model estimated
## an unstable, meaningless coefficient for it.
d_model <- d_model %>%
  mutate(dept_group = if_else(dept_group == "Other", "Other City Services", as.character(dept_group)),
         dept_group = factor(dept_group))

## ------------------------------------------------------------
## 6. DESCRIPTIVE STATISTICS
## ------------------------------------------------------------

## --- Table 1: Full-sample descriptive statistics ---
table1_overall <- d_model %>%
  summarise(
    n_cases            = n(),
    mean_response_hrs   = mean(response_hours),
    median_response_hrs = median(response_hours),
    sd_response_hrs     = sd(response_hours),
    pct_overdue         = mean(overdue, na.rm = TRUE) * 100
  )
print(kable(table1_overall, digits = 1, caption = "Table 1. Full-sample descriptive statistics"))

## --- Table 2: Descriptive statistics stratified by income group ---
table2_by_income <- d_model %>%
  filter(!is.na(income_group)) %>%
  group_by(income_group) %>%
  summarise(
    n_cases            = n(),
    mean_response_hrs   = mean(response_hours),
    median_response_hrs = median(response_hours),
    sd_response_hrs     = sd(response_hours),
    pct_overdue         = mean(overdue, na.rm = TRUE) * 100
  )
print(kable(table2_by_income, digits = 1, caption = "Table 2. Descriptive statistics by neighborhood income tier"))

## --- Chart 1: Distribution of response time, full sample ---
chart1 <- ggplot(d_model %>% filter(response_hours <= 24*30),
                 aes(x = response_hours)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  labs(title = "Distribution of 311 Case Closure Time (capped at 30 days)",
       x = "Response time (hours)", y = "Number of cases") +
  theme_minimal()
ggsave("chart1_response_time_distribution.png", chart1, width = 7, height = 5)

## --- Chart 2: Response time by neighborhood income tier and year ---
chart2 <- d_model %>%
  filter(!is.na(income_group)) %>%
  group_by(year, income_group) %>%
  summarise(mean_hrs = mean(response_hours), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_hrs, color = income_group, group = income_group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(title = "Average 311 Response Time by Neighborhood Income Tier, 2015-2019",
       x = "Year", y = "Mean response time (hours)", color = "") +
  theme_minimal()
ggsave("chart2_response_time_by_income_year.png", chart2, width = 7, height = 5)

## ------------------------------------------------------------
## 7. ANALYTICAL METHODS
## ------------------------------------------------------------

## --- Method 1: Multiple linear regression on response_hours ---
lm_model <- lm(response_hours ~ dept_group + channel_group + year + season +
                 median_household_income + poverty_rate + pct_minority,
               data = d_model)
summary(lm_model)
write.csv(tidy(lm_model), "lm_model_results.csv", row.names = FALSE)

## --- Method 2: Logistic regression on overdue (1 = overdue) ---
logit_model <- glm(overdue ~ dept_group + channel_group + year + season +
                     median_household_income + poverty_rate + pct_minority,
                   data = d_model, family = binomial)
summary(logit_model)
## Odds ratios are easier to interpret than raw log-odds coefficients
## (confint.default uses the fast Wald CI instead of slow profile-likelihood CI,
## which is essential given how large this dataset is)
logit_or <- exp(cbind(OddsRatio = coef(logit_model), confint.default(logit_model)))
print(logit_or)
write.csv(data.frame(term = rownames(logit_or), logit_or, row.names = NULL),
          "logit_model_results.csv", row.names = FALSE)

## ------------------------------------------------------------
## 8. SAVE CLEANED DATA (optional, for the Appendix / reproducibility)
## ------------------------------------------------------------
write_csv(d_model, "d311_clean_analysis_file.csv")

