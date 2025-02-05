# Load the necessary libraries for data manipulation, analysis, and reading files
library(tidyverse)   # Includes ggplot2, dplyr, tidyr, readr, and more for data manipulation and visualization
library(data.table)  # For fast data manipulation and aggregation
library(chron)       # For handling dates and times
library(readxl)      # For reading Excel files
library(stats)       # For statistical functions

# Read data from files into data frames
data1 <- read.csv(file = "data_for_BC_team_original.csv", header = TRUE)
ae_dc_time <- read_excel("ae_dc_time.xlsx")

# Merge data1 with ae_dc_time based on common columns and store the result in data
data <- left_join(data1, ae_dc_time)

# Clean up by removing the data1 data frame to free up memory
rm(data1)

# Computing waiting time to be registered or triaged
# Convert date and time columns to POSIXct objects for easier date-time manipulation
data <- data %>%
  mutate(
    reg_date_time = as.POSIXct(paste(reg_date, reg_time), format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone()),
    ed_assessment_date_time = as.POSIXct(paste(ed_assessment_date, ed_assessment_time), format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone()),
    triage_date = as.POSIXct(triage_date, format = "%Y-%m-%d %H:%M", tz = Sys.timezone()))


# Update specific records' registration date-time
data <- data %>%
  mutate(
    reg_date_time = ifelse(record_id == "463-1132", as.POSIXct("2015-01-07 15:21:00", tz = Sys.timezone()), reg_date_time),
    reg_date_time = ifelse(record_id == "470-563", as.POSIXct("2015-07-29 23:50:00", tz = Sys.timezone()), reg_date_time),
    reg_date_time = ifelse(record_id == "471-791", as.POSIXct("2015-08-27 15:02:00", tz = Sys.timezone()), reg_date_time)
  )

# Compute waiting time to be registered or triaged in minutes
data <- data %>%
  mutate(
    waiting_reg_triage = abs(as.numeric(difftime(reg_date_time, triage_date, units = "mins")))
  )
#Computing waiting time to be seen--------------------------------------
# Update specific records' ED assessment date-time
data <- data %>%
  mutate(
    ed_assessment_date_time = case_when(
      record_id == "465-103" ~ as.POSIXct("2014-12-16 00:30:00", tz = Sys.timezone()),
      record_id == "469-31" ~ as.POSIXct("2014-12-13 20:02:00", tz = Sys.timezone()),
      record_id == "469-339" ~ as.POSIXct("2015-01-19 01:50:00", tz = Sys.timezone()),
      record_id == "469-30" ~ as.POSIXct("2014-12-13 19:30:00", tz = Sys.timezone()),
      record_id == "469-313" ~ as.POSIXct("2015-01-19 00:25:00", tz = Sys.timezone()),
      TRUE ~ ed_assessment_date_time  # Keep existing value if no match
    )
  )

# Compute waiting time to be seen in minutes
data <- data %>%
  mutate(
    waiting_to_be_seen = as.numeric(difftime(ed_assessment_date_time, pmin(triage_date, reg_date_time, na.rm = TRUE), units = "mins"))
  )
#Computing waiting time to be moved to the inpatient bed or OR-------------------------

# Create POSIXct date-time columns and calculate waiting times
data <- data %>%
  mutate(
    bed_req_date_time = as.POSIXct(paste(inpt_bed_req_date, inpt_bed_req_time), format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone()),
    bed_move_date_time = as.POSIXct(paste(inpt_move_date, impt_move_time), format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone()),
    or_book_date_time = as.POSIXct(paste(or_book_date, or_book_time), format = "%m/%d/%Y %H:%M:%S", tz = Sys.timezone()),
    inpt_bed_waiting = as.numeric(difftime(bed_move_date_time, bed_req_date_time, units = "mins"))
  )
#summary(data$inpt_bed_waiting)

#Computing LOS--------------------------------------------------------

# Convert 'dc_date' to POSIXct format
data <- data %>%
  mutate(dc_date = as.POSIXct(dc_date, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))

# Calculate length of stay (los) based on pt_disposition values
data <- data %>%
  mutate(
    los = case_when(
      (pt_disposition %in% c(1, 4) & !is.na(pt_disposition)) ~ difftime(bed_move_date_time, pmin(triage_date, reg_date_time, na.rm = TRUE), units = "mins"),
      (pt_disposition %in% c(6, 7) & !is.na(pt_disposition)) ~ difftime(or_book_date_time, pmin(triage_date, reg_date_time, na.rm = TRUE), units = "mins"),
      (pt_disposition %in% c(8, 0) & !is.na(pt_disposition)) ~ difftime(dc_date, pmin(triage_date, reg_date_time, na.rm = TRUE), units = "mins"),
      TRUE ~ NA_real_  # Handle cases where conditions are not met
    ),
    los = as.numeric(los)  # Convert 'los' to numeric
    )

#Computing month of the shift date------------------------------------------

# Convert 'shift_date' to POSIXct format and extract the month
data <- data %>%
  mutate(
    shift_date = as.POSIXct(shift_date, format = "%Y-%m-%d", tz = Sys.timezone()),
    shift_month = format(shift_date, "%B")  # Extract the month as a full name (e.g., "January")
  )
#Removing the outliers-----------------------------------------------------------
# Define a function to remove outliers
remove_outliers <- function(x, lower_quantile = 0, upper_quantile = 0.999) {
  lower_bound <- quantile(x, probs = lower_quantile, na.rm = TRUE)
  upper_bound <- quantile(x, probs = upper_quantile, na.rm = TRUE)
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

# Apply the function to the relevant columns to remove negative values and high outliers
data <- data %>%
  mutate(
    waiting_to_be_seen = ifelse(waiting_to_be_seen < 0, NA, waiting_to_be_seen),
    inpt_bed_waiting = ifelse(inpt_bed_waiting < 0, NA, inpt_bed_waiting),
    los = ifelse(los < 0, NA, los),
    waiting_to_be_seen = remove_outliers(waiting_to_be_seen, upper_quantile = 0.999),
    inpt_bed_waiting = remove_outliers(inpt_bed_waiting, upper_quantile = 0.99),
    los = remove_outliers(los, upper_quantile = 0.999),
    wtbshour = remove_outliers(wtbshour, upper_quantile = 0.999)
  )
#Adding staff data---------------------------------------------------------------
# Read and prepare the staff data
census_data <- read_excel("adverse_events_census_data.xlsx")
names(census_data)[5] <- "shift_block"

# Select relevant columns from census_data
staff_data <- census_data %>%
  select(site, shift_date, shift_block, contains("nurses"), contains("np"), contains("md"), contains("jr"), contains("sr"))

# Use mutate and case_when to fill BP1 and BP2 columns based on shift_block
staff_data <- staff_data %>%
  mutate(
    BP1_nurses = case_when(
      shift_block == 0 ~ eight_nine_am_nurses,
      shift_block == 1 ~ four_five_pm_nurses,
      shift_block == 2 ~ midnight_one_nurses
    ),
    BP1_np = case_when(
      shift_block == 0 ~ eight_nine_am_np,
      shift_block == 1 ~ four_five_pm_np,
      shift_block == 2 ~ midnight_one_np
    ),
    BP1_md = case_when(
      shift_block == 0 ~ eight_nine_am_md,
      shift_block == 1 ~ four_five_pm_md,
      shift_block == 2 ~ midnight_one_md
    ),
    BP1_jr = case_when(
      shift_block == 0 ~ eight_nine_am_jr,
      shift_block == 1 ~ four_five_pm_jr,
      shift_block == 2 ~ midnight_one_jr
    ),
    BP1_sr = case_when(
      shift_block == 0 ~ eight_nine_am_sr,
      shift_block == 1 ~ four_five_pm_sr,
      shift_block == 2 ~ midnight_one_sr
    ),
    BP2_nurses = case_when(
      shift_block == 0 ~ twelve_one_pm_nurses,
      shift_block == 1 ~ eight_nine_pm_nurses,
      shift_block == 2 ~ four_five_am_nurses
    ),
    BP2_np = case_when(
      shift_block == 0 ~ twelve_one_pm_np,
      shift_block == 1 ~ eight_nine_pm_np,
      shift_block == 2 ~ four_five_am_np
    ),
    BP2_md = case_when(
      shift_block == 0 ~ twelve_one_pm_md,
      shift_block == 1 ~ eight_nine_pm_md,
      shift_block == 2 ~ four_five_am_md
    ),
    BP2_jr = case_when(
      shift_block == 0 ~ twelve_one_pm_jr,
      shift_block == 1 ~ eight_nine_pm_jr,
      shift_block == 2 ~ four_five_am_jr
    ),
    BP2_sr = case_when(
      shift_block == 0 ~ twelve_one_pm_sr,
      shift_block == 1 ~ eight_nine_pm_sr,
      shift_block == 2 ~ four_five_am_sr
    )
  )

# Select the relevant columns for merging
staff_data_2 <- staff_data %>%
  select(site, shift_date, shift_block, BP1_nurses, BP1_np, BP1_md, BP1_jr, BP1_sr, BP2_nurses, BP2_np, BP2_md, BP2_jr, BP2_sr)

# Merge the updated staff data with the main data
data_updated <- left_join(data, staff_data_2, by = c("site", "shift_date", "shift_block"))

# Calculate the total staff for BP1 and BP2
data_updated <- data_updated %>%
  mutate(
    BP1_total_staff = rowSums(select(., BP1_nurses, BP1_np, BP1_md, BP1_jr, BP1_sr), na.rm = TRUE),
    BP2_total_staff = rowSums(select(., BP2_nurses, BP2_np, BP2_md, BP2_jr, BP2_sr), na.rm = TRUE)
  )

#computing number of staff at the time of registration for each patient and also 
#column that shows whether patient arrived at the first or second half of the shift

# Define a function to determine the shift and number of staff based on conditions
data_updated <- data_updated %>%
  mutate(
    shift_first_half = case_when(
      shift_block == 0 & !is.na(shift_block) & hour(pmin(triage_date, reg_date_time, na.rm = TRUE)) < 12 ~ 1,
      shift_block == 1 & !is.na(shift_block) & hour(pmin(triage_date, reg_date_time, na.rm = TRUE)) < 20 ~ 1,
      shift_block == 2 & !is.na(shift_block) & hour(pmin(triage_date, reg_date_time, na.rm = TRUE)) < 4 ~ 1,
      TRUE ~ 0
    ),
    number_staff = case_when(
      shift_first_half == 1 ~ BP1_total_staff,
      shift_first_half == 0 ~ BP2_total_staff,
      TRUE ~ NA_real_
    )
  )

# Clean up temporary data frames to free up memory
rm(staff_data, staff_data_2)

# Update the original data frame and clean up
data <- data_updated
rm(data_updated)

# Optionally, view the first few rows of the updated data
View(head(data))
#Preventable Adverse Events-----------------------------------------------------------------

# Create a new 'preventable' column using vectorized operations
data <- data %>%
  mutate(preventable = ifelse(
    (preventable_ae > 1 & !is.na(preventable_ae)) |
      (preventable_ae_2 > 1 & !is.na(preventable_ae_2)) |
      (preventable_ae_3 > 1 & !is.na(preventable_ae_3)) |
      (preventable_ae_4 > 1 & !is.na(preventable_ae_4)) |
      (preventable_ae_5 > 1 & !is.na(preventable_ae_5)), 1, 0
  ))

# Select relevant columns and calculate the number of preventable cases
data1 <- data %>%
  select(record_id, starts_with("preventable_ae")) %>%
  mutate(across(starts_with("preventable_ae"), ~ ifelse(. > 1 & !is.na(.), 1, 0), .names = "{.col}_bin")) %>%
  mutate(preventable_num = rowSums(select(., ends_with("_bin")), na.rm = TRUE))

# Join the 'preventable_num' back to the main data frame
data <- data %>%
  left_join(data1 %>% select(record_id, preventable_num), by = "record_id")

# Clean up temporary data frames
rm(data1)


#Computing the total number of preventable & ED related AEs-------------------------------------------

# Select relevant columns and create binary indicators
data1 <- data %>%
  select(record_id, contains("preventable"), contains("index")) %>%
  mutate(
    preventable_EDrelated_1 = ifelse(preventable_ae > 1 & !is.na(preventable_ae) & ae_index == 1 & !is.na(ae_index), 1, 0),
    preventable_EDrelated_2 = ifelse(preventable_ae_2 > 1 & !is.na(preventable_ae_2) & ae_index_2 == 1 & !is.na(ae_index_2), 1, 0),
    preventable_EDrelated_3 = ifelse(preventable_ae_3 > 1 & !is.na(preventable_ae_3) & ae_index_3 == 1 & !is.na(ae_index_3), 1, 0),
    preventable_EDrelated_4 = ifelse(preventable_ae_4 > 1 & !is.na(preventable_ae_4) & ae_index_4 == 1 & !is.na(ae_index_4), 1, 0),
    preventable_EDrelated_5 = ifelse(preventable_ae_5 > 1 & !is.na(preventable_ae_5) & ae_index_5 == 1 & !is.na(ae_index_5), 1, 0),
    preventable_EDrelated_num = rowSums(select(., starts_with("preventable_EDrelated_")), na.rm = TRUE)
  )

# Join the 'preventable_EDrelated_num' back to the main data frame
data <- data %>%
  left_join(data1 %>% select(record_id, preventable_EDrelated_num), by = "record_id") %>%
  mutate(
    preventable_EDrelated = ifelse(preventable_EDrelated_num > 0, 1, 0)
  )

# Clean up temporary data frames
rm(data1)

### Creating new column after pairing up the diagnosis groupings-----------------------------------

# Assign diagnosis based on primary_dx_grouping using case_when
data <- data %>%
  mutate(
    diagnosis = case_when(
      primary_dx_grouping %in% c(1, 2, 3, 13) ~ 1,
      primary_dx_grouping %in% c(17, 25) ~ 2,
      primary_dx_grouping %in% c(5, 7, 9, 10, 11, 14, 18, 19, 21, 22, 24, 26, 27) ~ 3,
      primary_dx_grouping %in% c(8, 15, 28) ~ 4,
      primary_dx_grouping %in% c(4, 12, 16, 23) ~ 5,
      primary_dx_grouping == 29 ~ 6,
      primary_dx_grouping %in% c(6, 20) ~ 7,
      TRUE ~ NA_real_  # Assign NA to all other cases
    )
  )
#Creating Shift Level Data-----------------------------------------------------------------------------


# Create shift-level data by grouping and summarizing
data <- data %>%
  mutate(AE = ifelse(AEoutcome == TRUE, 1, 0),
         lwbs = ifelse(pt_disposition == 2, 1, 0)) %>%
  group_by(site, shift_block, shift_date, ctas) %>%
  mutate(
    num_ae_shift = sum(AE, na.rm = TRUE),                  # Number of AEs for each ctas level within each shift
    avg_los_shift = mean(los, na.rm = TRUE),               # Avg LOS of each ctas level within each shift
    num_preventable_ae_shift = sum(preventable, na.rm = TRUE), # Number of preventable AEs
    num_preventable_ae_ed_shift = sum(preventable_EDrelated, na.rm = TRUE) # Preventable ED-related AEs
  ) %>%
  ungroup() %>%
  group_by(site, shift_block, shift_date) %>%
  mutate(
    avg_staff_shift = mean(number_staff, na.rm = TRUE),    # Average number of staff
    num_lwbs_shift = sum(lwbs, na.rm = TRUE),              # Number of LWBS
    avg_census_shift = mean(censushour, na.rm = TRUE),     # Average census during the shift
    avg_wtbs_shift = mean(wtbshour, na.rm = TRUE),         # Average wait to be seen
    avg_tba_shift = mean(tbahour, na.rm = TRUE),           # Average time to bed assignment
    avg_wtbs_time_shift = mean(waiting_to_be_seen, na.rm = TRUE), # Average waiting time to be seen
    num_discharged_shift = n_distinct(record_id[pt_disposition == 0]), # Number of discharged patients
    num_admitted_shift = n_distinct(record_id[pt_disposition == 1]),   # Number of admitted patients
    num_chronic_shift = sum(chronic_condition, na.rm = TRUE)  # Number of chronic conditions
  ) %>%
  ungroup()

# Calculate the 75th percentile of avg_los_shift for each ctas level
thresholds <- data %>%
  group_by(ctas) %>%
  summarize(threshold = quantile(avg_los_shift, probs = 0.75, na.rm = TRUE), .groups = 'drop')

# The 'thresholds' data frame now contains the 75th percentile threshold for each ctas level
# This can be used for further analysis or processing

# Example of how to use these thresholds (if needed):
# Join thresholds back to the main data or use in a conditional statement
data <- data %>%
  left_join(thresholds, by = "ctas") %>%
  mutate(
    is_significant = ifelse(avg_los_shift > threshold, TRUE, FALSE)
  )

# Cleaning up if needed
rm(thresholds)
# Calculate the 75th percentile threshold for avg_los_shift for each ctas level
los_thresholds <- data %>%
  group_by(ctas) %>%
  summarize(
    avg_los_threshold = quantile(avg_los_shift, probs = 0.75, na.rm = TRUE),
    los_discontinuity_threshold = quantile(los, probs = 0.6, na.rm = TRUE),
    .groups = 'drop'
  )

# Join thresholds back to the main data frame
data <- data %>%
  left_join(los_thresholds, by = "ctas") %>%
  mutate(
    crowded_shift = ifelse(avg_los_shift > avg_los_threshold, 1, 0),
    los_discontinuity = ifelse(los > los_discontinuity_threshold, 1, 0)
  )

# Clean up temporary variables if needed
rm(los_thresholds)

# Calculate the 70th percentile threshold for waiting_to_be_seen for each ctas level
wtbs_thresholds <- data %>%
  group_by(ctas) %>%
  summarize(
    wtbs_threshold = quantile(waiting_to_be_seen, probs = 0.7, na.rm = TRUE),
    .groups = 'drop'
  )

# Join thresholds back to the main data frame and determine discontinuity
data <- data %>%
  left_join(wtbs_thresholds, by = "ctas") %>%
  mutate(
    wtbs_discontinuity = ifelse(waiting_to_be_seen > wtbs_threshold, 1, 0)
  )

# Clean up temporary variables if needed
rm(wtbs_thresholds)
# Calculate thresholds
wtbs_threshold <- quantile(data$wtbshour, probs = 0.6, na.rm = TRUE)
wtbs_shift_threshold <- quantile(data$avg_wtbs_shift, probs = 0.5, na.rm = TRUE)
wtbs_time_threshold <- quantile(data$waiting_to_be_seen, probs = 0.7, na.rm = TRUE)
wtbs_time_shift_threshold <- quantile(data$avg_wtbs_time_shift, probs = 0.45, na.rm = TRUE)

# Create discontinuity flags using calculated thresholds
data <- data %>%
  mutate(
    crowded_wtbs = ifelse(wtbshour > wtbs_threshold, 1, 0),
    crowded_wtbs_shift = ifelse(avg_wtbs_shift > wtbs_shift_threshold, 1, 0),
    crowded_wtbs_time = ifelse(waiting_to_be_seen > wtbs_time_threshold, 1, 0),
    crowded_wtbs_time_shift = ifelse(avg_wtbs_time_shift > wtbs_time_shift_threshold, 1, 0)
  )

# Optional: View the first few rows of the updated data
View(head(data))
# Calculate the frequency of AEs in different primary_dx_groupings
ae_summary <- data %>%
  group_by(primary_dx_grouping) %>%
  summarize(
    diagnosis_count = n_distinct(record_id),
    diagnosis_AE_count = n_distinct(record_id[AEoutcome == TRUE]),
    .groups = 'drop'
  )

# Write the summary to a CSV file
write.csv(ae_summary, "/Users/mona/Desktop/AE_Diagnosis.csv", row.names = FALSE)

# Create a group_mental flag for primary_dx_grouping 8
data <- data %>%
  mutate(group_mental = ifelse(primary_dx_grouping == 8, 1, 0))

# Define low, medium, and high acuity levels from ctas using case_when
data <- data %>%
  mutate(
    acuity = case_when(
      ctas %in% c(1, 2) ~ 1,  # High acuity
      ctas == 3 ~ 2,          # Medium acuity
      ctas %in% c(4, 5) ~ 3,  # Low acuity
      TRUE ~ NA_real_         # Assign NA for undefined ctas values
    )
  )

# Optional: View the first few rows of the updated data
View(head(data))
#Regression Models-------------------------------------------------------------------------------------------------------------

##Addressing reverse causality: using 2SLS to run LOS=AE+.... in the first stage and put the fitted values for the second
##Source:http://www.gwilympryce.co.uk/teach/AQIM_L1_Reverse_Causation.pdf
##Useful link:https://stats.stackexchange.com/questions/141750/2sls-logit-probit-in-the-second-stage
##They suggested to have 2 Stage Residual Inclusion Model (2SRI) instead of 2SLS as the second stage is logit

##Running simultanous equations for preventable AEs--------------------------

#data1 <- data[rep(row.names(data), data$preventable_num), 1:369]
#data2 <- data[data$preventable_num==0,]
#data.extended <- rbind(data1, data2)

model1 <- lm(formula = los ~ preventable + ageyrs + wtbshour+ number_staff+ as.factor(shift_block)+ as.factor(ctas)+ as.factor(site)+as.factor(diagnosis), data, na.action = na.exclude)
summary(model1) 

data_updated <- cbind(data, fitted_los=fitted(model1))
data_updated <- cbind(data_updated, residuals_model1=residuals(model1))

model2 <- glm(formula = preventable ~ fitted_los+ageyrs+as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model2)  

install.packages("stargazer")
library(stargazer)
stargazer(model1, title="Model1", type="text", df=FALSE, digits=6)
stargazer(model2, title="Model2", type="text", df=FALSE, digits=6)

threshold <- 189  #Obtained the result
data_updated$u_fitted_los <- data_updated$fitted_los-threshold
data_updated$u_fitted_los[data_updated$u_fitted_los<0] <- 0
data_updated$v_fitted_los <- 0
data_updated$v_fitted_los[data_updated$fitted_los>threshold] <- -1
model <- glm(formula = preventable ~ fitted_los + u_fitted_los+ v_fitted_los + ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

#Estimating multiple thresholds for los------------------

threshold1 <- 125   #Did not get perfect results
threshold2 <- 279
data_updated$u_1_fitted_los <- data_updated$fitted_los-threshold1
data_updated$u_1_fitted_los[data_updated$u_1_fitted_los<0] <- 0
data_updated$u_2_fitted_los <- data_updated$fitted_los-threshold2
data_updated$u_2_fitted_los[data_updated$u_2_fitted_los<0] <- 0
data_updated$v_1_fitted_los <- 0
data_updated$v_2_fitted_los <- 0
data_updated$v_1_fitted_los[data_updated$fitted_los>threshold1] <- -1
data_updated$v_2_fitted_los[data_updated$fitted_los>threshold2] <- -1
model <- glm(formula = preventable ~ fitted_los + u_1_fitted_los+u_2_fitted_los+ v_1_fitted_los + v_2_fitted_los +ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model) 
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

###using los itself, not the fitted values: (AE, not preventable)
threshold1 <- 116     #(We obtained some results for this)
threshold2 <- 261
data_updated$u_1_los <- data_updated$los-threshold1
data_updated$u_1_los[data_updated$u_1_los<0] <- 0
data_updated$u_2_los <- data_updated$los-threshold2
data_updated$u_2_los[data_updated$u_2_los<0] <- 0
data_updated$v_1_los <- 0
data_updated$v_2_los <- 0
data_updated$v_1_los[data_updated$los>threshold1] <- -1
data_updated$v_2_los[data_updated$los>threshold2] <- -1
model <- glm(formula = AEoutcome ~ los + u_1_los+u_2_los+ v_1_los + v_2_los +ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model) 
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])

#threshold <- 20
#data_updated$u_avg_wtbs_shift <- data_updated$avg_wtbs_shift-threshold
#data_updated$u_avg_wtbs_shift[data_updated$avg_wtbs_shift<0] <- 0
#data_updated$v_avg_wtbs_shift <- 0
#data_updated$v_avg_wtbs_shift[data_updated$avg_wtbs_shift>threshold] <- -1
#model <- glm(formula = preventable ~ fitted_los + avg_wtbs_shift+ u_avg_wtbs_shift + v_avg_wtbs_shift + ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
#summary(model) 
#new_threshold <- threshold+(model$coefficients[5]/model$coefficients[4])


##Preventable AEs related to ED 

model3 <- lm(formula = los ~ preventable_EDrelated + ageyrs + wtbshour+ number_staff+ as.factor(shift_block)+ as.factor(ctas)+ as.factor(site)+as.factor(diagnosis), data, na.action = na.exclude)
summary(model1) 
#model3 <- lm(formula = los ~ preventable_EDrelated+ wtbshour*crowded_wtbs+ ageyrs + number_staff+ as.factor(shift_block)+ as.factor(ctas)+ as.factor(site)+as.factor(diagnosis), data, na.action = na.exclude)
#summary(model3) 

data_updated <- cbind(data_updated, fitted_los=fitted(model3))

model4 <- glm(formula = preventable_EDrelated ~ fitted_los +ageyrs + as.factor(shift_block)+ as.factor(ctas)+ as.factor(site)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model4)  

stargazer(model3, title="Model3", type="text", df=FALSE, digits=6)
stargazer(model4, title="Model4", type="text", df=FALSE, digits=6)

###Discountinuity of wtbshour in the first stage
###Discountinuity of los/ageyrs in the first stage (maybe separate it for lwbs group or ....)

#Effect of waiting to be seen on preventable AEs:----------------

model4_1 <- glm(formula = preventable_EDrelated ~ waiting_to_be_seen +ageyrs+ as.factor(data$chronic_condition)+as.factor(shift_block)+as.factor(ctas)+ as.factor(site)+ as.factor(pt_disposition), family = "binomial", data = data)
summary(model4_1) 

threshold <- 97.6
data$u_waiting_to_be_seen <- data$waiting_to_be_seen-threshold
data$u_waiting_to_be_seen[data$u_waiting_to_be_seen<0] <- 0
data$v_waiting_to_be_seen <- 0
data$v_waiting_to_be_seen[data$waiting_to_be_seen>threshold] <- -1
model <- glm(formula = preventable_EDrelated  ~ waiting_to_be_seen + u_waiting_to_be_seen+ v_waiting_to_be_seen + ageyrs+ as.factor(shift_block)+as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

#multiple threshold

threshold1 <- 120
threshold2 <- 315
data$u_1_waiting_to_be_seen <- data$waiting_to_be_seen-threshold1
data$u_1_waiting_to_be_seen[data$u_1_waiting_to_be_seen<0] <- 0
data$u_2_waiting_to_be_seen <- data$waiting_to_be_seen-threshold2
data$u_2_waiting_to_be_seen[data$u_2_waiting_to_be_seen<0] <- 0
data$v_1_waiting_to_be_seen <- 0
data$v_2_waiting_to_be_seen <- 0
data$v_1_waiting_to_be_seen[data$waiting_to_be_seen>threshold1] <- -1
data$v_2_waiting_to_be_seen[data$waiting_to_be_seen>threshold2] <- -1
model <- glm(formula = preventable_EDrelated ~ waiting_to_be_seen + u_1_waiting_to_be_seen+u_2_waiting_to_be_seen+ v_1_waiting_to_be_seen + v_2_waiting_to_be_seen +ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data)
summary(model) 
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])

#Multiple & single thresholds (wtbshour)------------------------------------------

model <- glm(formula = preventable_EDrelated ~ wtbshour+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(ctas)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = data)
summary(model) 

stargazer(model, title="Model 1", type="text", df=FALSE, digits=6)

threshold <- 16 #(Got some results)
data$u_wtbshour <- data$wtbshour-threshold
data$u_wtbshour[data$u_wtbshour<0] <- 0
data$v_wtbshour <- 0
data$v_wtbshour[data$wtbshour>threshold] <- -1
model <- glm(formula = preventable_EDrelated ~ wtbshour + u_wtbshour+ v_wtbshour + number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = data)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])

#multiple threshold

threshold1 <- 2.55
threshold2 <- 40.8
data$u_1_wtbshour <- data$wtbshour-threshold1
data$u_1_wtbshour[data$u_1_wtbshour<0] <- 0
data$u_2_wtbshour <- data$wtbshour-threshold2
data$u_2_wtbshour[data$u_2_wtbshour<0] <- 0
data$v_1_wtbshour <- 0
data$v_2_wtbshour <- 0
data$v_1_wtbshour[data$wtbshour>threshold1] <- -1
data$v_2_wtbshour[data$wtbshour>threshold2] <- -1
model <- glm(formula = preventable_EDrelated ~ wtbshour + u_1_wtbshour+u_2_wtbshour+ v_1_wtbshour + v_2_wtbshour +ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data)
summary(model) 
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])


######Shift level analysis-------------------------
data_shift <- data %>% select(shift_date, shift_block, site, ctas, num_ae_shift, num_preventable_ae_shift, num_preventable_ae_ed_shift, num_chronic_shift, avg_los_shift, avg_staff_shift, num_lwbs_shift, avg_census_shift, num_discharged_shift, num_admitted_shift, avg_wtbs_shift, avg_wtbs_time_shift )
data_shift <- unique(data_shift)

model5 <- glm(formula= num_preventable_ae_shift ~ avg_wtbs_shift+ avg_staff_shift+as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model5)

model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_time_shift+ avg_staff_shift +num_lwbs_shift +num_admitted_shift+num_chronic_shift+as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)

#Algorithm to compute avg_wtbs_shift threshold:
threshold <- 16.3 #(Obtained result)
data_shift$u_avg_wtbs_shift <- data_shift$avg_wtbs_shift-threshold
data_shift$u_avg_wtbs_shift[data_shift$u_avg_wtbs_shift<0] <- 0
data_shift$v_avg_wtbs_shift <- 0
data_shift$v_avg_wtbs_shift[data_shift$avg_wtbs_shift>threshold] <- -1
model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_shift+ u_avg_wtbs_shift+v_avg_wtbs_shift+avg_staff_shift + as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

#Multiple thresholds
threshold1 <- 2.46
threshold2 <- 11.7
data_shift$u_1_avg_wtbs_shift <- data_shift$avg_wtbs_shift-threshold1
data_shift$u_2_avg_wtbs_shift <- data_shift$avg_wtbs_shift-threshold2
data_shift$u_1_avg_wtbs_shift[data_shift$u_1_avg_wtbs_shift<0] <- 0
data_shift$u_2_avg_wtbs_shift[data_shift$u_2_avg_wtbs_shift<0] <- 0
data_shift$v_1_avg_wtbs_shift <- 0
data_shift$v_2_avg_wtbs_shift <- 0
data_shift$v_1_avg_wtbs_shift[data_shift$avg_wtbs_shift>threshold1] <- -1
data_shift$v_2_avg_wtbs_shift[data_shift$avg_wtbs_shift>threshold2] <- -1
model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_shift+ u_1_avg_wtbs_shift+u_2_avg_wtbs_shift+v_1_avg_wtbs_shift+v_2_avg_wtbs_shift+avg_staff_shift + as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

#threshold for wtbs_time-----
threshold <- 197  #(got result)
data_shift$u_avg_wtbs_time_shift <- data_shift$avg_wtbs_time_shift-threshold
data_shift$u_avg_wtbs_time_shift[data_shift$u_avg_wtbs_time_shift<0] <- 0
data_shift$v_avg_wtbs_time_shift <- 0
data_shift$v_avg_wtbs_time_shift[data_shift$avg_wtbs_time_shift>threshold] <- -1
model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_time_shift+ u_avg_wtbs_time_shift+v_avg_wtbs_time_shift+avg_staff_shift+num_lwbs_shift +num_admitted_shift+num_chronic_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

#multiple 
threshold1 <- 20   
threshold2 <- 129
data_shift$u_1_avg_wtbs_time_shift <- data_shift$avg_wtbs_time_shift-threshold1
data_shift$u_2_avg_wtbs_time_shift <- data_shift$avg_wtbs_time_shift-threshold2
data_shift$u_1_avg_wtbs_time_shift[data_shift$u_1_avg_wtbs_time_shift<0] <- 0
data_shift$u_2_avg_wtbs_time_shift[data_shift$u_2_avg_wtbs_time_shift<0] <- 0
data_shift$v_1_avg_wtbs_time_shift <- 0
data_shift$v_2_avg_wtbs_time_shift <- 0
data_shift$v_1_avg_wtbs_time_shift[data_shift$avg_wtbs_time_shift>threshold1] <- -1
data_shift$v_2_avg_wtbs_time_shift[data_shift$avg_wtbs_time_shift>threshold2] <- -1
model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_time_shift+ u_1_avg_wtbs_time_shift+u_2_avg_wtbs_time_shift+v_1_avg_wtbs_time_shift+v_2_avg_wtbs_time_shift+avg_staff_shift + as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)


#Some other type of threshodls:----------------------------------------
model1 <- glm(formula = preventable ~ los+ los*crowded_wtbs + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model1)  #got result for this with threshold 0.8 for crowded_wtbs

wtbs_threshold <- quantile(data$wtbshour, probs=0.8, na.rm=TRUE)
data <- data %>% mutate(crowded_wtbs=ifelse((!is.na(wtbshour) & wtbshour>wtbs_threshold),1,0))

model2 <- glm(formula = preventable ~ los+ ageyrs+ crowded_wtbs + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model2) #got result for this with threshold of 0.45 for crowded_wtbs (10)

model2 <- glm(formula = preventable~ los+ ageyrs + crowded_wtbs_shift + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model2) #got similar results

model3 <- glm(formula = ae_index ~ los+ crowded_wtbs_time_shift + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model3)  #got result for this with threshold of 0.45 for wtbs_time_threshold (58 min)

model4 <- glm(formula = ae_index ~ los+ avg_wtbs_time_shift*crowded_wtbs_time_shift + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model4)

#Analysis on lwbs-------------------------------------------

wtbs_threshold_1 <- quantile(data$wtbshour, probs=0.40, na.rm=TRUE)
wtbs_threshold_2 <- quantile(data$wtbshour, probs=0.80, na.rm=TRUE)
data$crowded_wtbs_group <- 0
data$crowded_wtbs_group <- ifelse(!is.na(data$wtbshour) & data$wtbshour>wtbs_threshold_2,2,0)
data$crowded_wtbs_group[!is.na(data$wtbshour) & data$wtbshour<wtbs_threshold_2 & data$wtbshour>wtbs_threshold_1] <-1

model <- glm(formula=lwbs~ wtbshour+ageyrs+as.factor(shift_block)+as.factor(ctas)+as.factor(site), family = "binomial", data = data) 
summary(model)

threshold <- 16.9 #(Got some results)
data$u_wtbshour <- data$wtbshour-threshold
data$u_wtbshour[data$u_wtbshour<0] <- 0
data$v_wtbshour <- 0
data$v_wtbshour[data$wtbshour>threshold] <- -1
model <- glm(formula = lwbs ~ wtbshour + u_wtbshour+ v_wtbshour + ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas), family = "binomial", data = data)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])

#multiple threshold (Didn't get result)

threshold1 <- 6.29
threshold2 <- 13.7
data$u_1_wtbshour <- data$wtbshour-threshold1
data$u_1_wtbshour[data$u_1_wtbshour<0] <- 0
data$u_2_wtbshour <- data$wtbshour-threshold2
data$u_2_wtbshour[data$u_2_wtbshour<0] <- 0
data$v_1_wtbshour <- 0
data$v_2_wtbshour <- 0
data$v_1_wtbshour[data$wtbshour>threshold1] <- -1
data$v_2_wtbshour[data$wtbshour>threshold2] <- -1
model <- glm(formula = lwbs ~ wtbshour + u_1_wtbshour+u_2_wtbshour+ v_1_wtbshour + v_2_wtbshour +ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(ctas), family = "binomial", data = data)
summary(model) 
new_threshold1 <- threshold1+(model$coefficients[5]/model$coefficients[3])
new_threshold2 <- threshold2+(model$coefficients[6]/model$coefficients[4])

####Other type of threshold for wtbs and wtbs time in shift level---------

model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_shift+ avg_staff_shift+as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)

summary(data_shift$avg_wtbs_shift)

avg_wtbs_t1 <- 3
avg_wtbs_t2 <- 6
avg_wtbs_t3 <- 9
avg_wtbs_t4 <- 12
avg_wtbs_t5 <- 15
data_shift$crowded_wtbs_group <- 0
data_shift$crowded_wtbs_group[data_shift$avg_wtbs_shift>avg_wtbs_t1 & data_shift$avg_wtbs_shift<=avg_wtbs_t2] <-1
data_shift$crowded_wtbs_group[data_shift$avg_wtbs_shift>avg_wtbs_t2 & data_shift$avg_wtbs_shift<=avg_wtbs_t3] <-2
data_shift$crowded_wtbs_group[data_shift$avg_wtbs_shift>avg_wtbs_t3 & data_shift$avg_wtbs_shift<=avg_wtbs_t4] <-3
data_shift$crowded_wtbs_group[data_shift$avg_wtbs_shift>avg_wtbs_t4] <-4

model <- glm(formula= num_preventable_ae_ed_shift ~ avg_staff_shift+as.factor(crowded_wtbs_group)+as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)  # threshold ~ 9

#Wtbs time

model <- glm(formula= num_preventable_ae_ed_shift ~ avg_wtbs_time_shift+ avg_staff_shift +num_lwbs_shift +num_admitted_shift+num_chronic_shift+as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model)

avg_wtbs_time_t1 <- 25
avg_wtbs_time_t2 <- 50
avg_wtbs_time_t3 <- 75
avg_wtbs_time_t4 <- 100
avg_wtbs_time_t5 <- 125
avg_wtbs_time_t6 <- 150
avg_wtbs_time_t7 <- 175
data_shift$crowded_wtbs_time_group <- 0
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t1 & data_shift$avg_wtbs_time_shift<=avg_wtbs_time_t2] <-1
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t2 & data_shift$avg_wtbs_time_shift<=avg_wtbs_time_t3] <-2
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t3 & data_shift$avg_wtbs_time_shift<=avg_wtbs_time_t4] <-3
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t4 & data_shift$avg_wtbs_time_shift<=avg_wtbs_time_t5] <-4
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t5 & data_shift$avg_wtbs_time_shift<=avg_wtbs_time_t6] <-5
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t6 & data_shift$avg_wtbs_time_shift<=avg_wtbs_time_t7] <-6
data_shift$crowded_wtbs_time_group[data_shift$avg_wtbs_time_shift>avg_wtbs_time_t7] <-7

model <- glm(formula= num_preventable_ae_ed_shift ~ as.factor(crowded_wtbs_time_group)+ avg_staff_shift +num_lwbs_shift +num_admitted_shift+as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model) #Threshold for avg_wtbs_time=150 min

#New analysis for the effects of los on AE:
model1 <- lm(formula = los ~ preventable_EDrelated + ageyrs + wtbshour+ number_staff+as.factor(shift_block)+ as.factor(ctas)+ as.factor(site)+as.factor(diagnosis), data, na.action = na.exclude)
summary(model1) 

data_updated <- cbind(data, fitted_los=fitted(model1))
data_updated <- cbind(data_updated, residuals_model1=residuals(model1))

model2 <- glm(formula = preventable_EDrelated ~ fitted_los+ageyrs+as.factor(chronic_condition)+as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model2)  

threshold <- 184  #Obtained the result
data_updated$u_fitted_los <- data_updated$fitted_los-threshold
data_updated$u_fitted_los[data_updated$u_fitted_los<0] <- 0
data_updated$v_fitted_los <- 0
data_updated$v_fitted_los[data_updated$fitted_los>threshold] <- -1
model <- glm(formula = preventable_EDrelated ~ fitted_los + u_fitted_los+ v_fitted_los + ageyrs+ as.factor(chronic_condition)+as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])
stargazer(model, title="Model", type="text", df=FALSE, digits=6)

#Other type of threshold for crowded shift

wtbs_t1 <- 3
wtbs_t2 <- 6
wtbs_t3 <- 9
wtbs_t4 <- 12
wtbs_t5 <- 15
data_updated$crowded_wtbs_group <- 0
data_updated$crowded_wtbs_group[(!is.na(data_updated$wtbshour)) & data_updated$wtbshour>wtbs_t1 & data_updated$wtbshour<=wtbs_t2] <-1
data_updated$crowded_wtbs_group[(!is.na(data_updated$wtbshour)) & data_updated$wtbshour>wtbs_t2 & data_updated$wtbshour<=wtbs_t3] <-2
data_updated$crowded_wtbs_group[(!is.na(data_updated$wtbshour)) & data_updated$wtbshour>wtbs_t3 & data_updated$wtbshour<=wtbs_t4] <-3
data_updated$crowded_wtbs_group[(!is.na(data_updated$wtbshour)) & data_updated$wtbshour>wtbs_t4 & data_updated$wtbshour<=wtbs_t5] <-4
data_updated$crowded_wtbs_group[(!is.na(data_updated$wtbshour)) & data_updated$wtbshour>wtbs_t5] <-5

model2 <- glm(formula = preventable_EDrelated ~ as.factor(crowded_wtbs_group)+ageyrs+number_staff+as.factor(chronic_condition)+as.factor(shift_block)+ as.factor(site)+ as.factor(ctas)+ as.factor(pt_disposition), family = "binomial", data = data_updated)
summary(model2)  

stargazer(model2, title="Model", type="text", df=FALSE, digits=6)

#Analysing plots using break points--------------------------------------------------------

data4 <- data[!is.na(data$preventable) & !is.na(data$ageyrs),]
#data4_1<- data4[data4$lwbs==1,]
#data4_2<- data4[data4$lwbs==0,]
attach(data4)
fit <- glm(formula=preventable ~ poly(ageyrs, 6) ,family ="binomial", data=data4 )
agelims =range(ageyrs)
age.grid=seq (from=agelims[1], to=agelims[2])
preds=predict (fit ,newdata =list(ageyrs=age.grid),se=T)
pfit=exp(preds$fit )/(1+ exp( preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))
preds=predict(fit,newdata =list(ageyrs=age.grid),type="response", se=T)
plot(ageyrs ,preventable ,xlim=agelims ,type ="n",ylim=c(0 ,.1) )
lines(age.grid ,pfit ,lwd =2, col =" blue")
matlines(age.grid ,se.bands ,lwd =1, col =" blue",lty =3)

data5 <- data[!is.na(data$preventable) & !is.na(data$los),]
attach(data5)
fit <- glm(formula=preventable ~ poly(los, 7) ,family ="binomial",data=data5 )
loslims =range(los)
los.grid=seq (from=loslims[1], to=loslims[2])
preds=predict (fit ,newdata =list(los=los.grid),se=T)
pfit=exp(preds$fit )/(1+ exp( preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))
preds=predict(fit,newdata =list(los=los.grid),type="response", se=T)
plot(los ,preventable ,xlim=loslims ,type ="n",ylim=c(0 ,.2) )
lines(los.grid ,pfit ,lwd =2, col =" blue")
matlines(los.grid ,se.bands ,lwd =1, col =" blue",lty =3)

data6 <- data[!is.na(data$preventable) & !is.na(data$wtbshour),]
attach(data6)
fit <- glm(formula=preventable ~ poly(wtbshour, 5) ,family ="binomial",data=data6 )
wtbslims =range(wtbshour)
wtbs.grid=seq (from=wtbslims[1], to=wtbslims[2])
preds=predict (fit ,newdata =list(wtbshour=wtbs.grid),se=T)
pfit=exp(preds$fit )/(1+ exp( preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))
preds=predict(fit,newdata =list(wtbshour=wtbs.grid),type="response", se=T)
plot(wtbshour ,preventable ,xlim=wtbslims ,type ="n",ylim=c(0 ,.2) )
lines(wtbs.grid ,pfit ,lwd =2, col =" blue")
matlines(wtbs.grid ,se.bands ,lwd =1, col =" blue",lty =3)

data$log_los <- log(data$los+1)
data7 <- data[!is.na(data$preventable) & !is.na(data$los),]
attach(data7)
fit <- glm(formula=preventable ~ poly(log_los, 5) ,family ="binomial",data=data5 )
loslims =range(log_los)
los.grid=seq (from=loslims[1], to=loslims[2])
preds=predict (fit ,newdata =list(log_los=los.grid),se=T)
pfit=exp(preds$fit )/(1+ exp( preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))
preds=predict(fit,newdata =list(log_los=los.grid),type="response", se=T)
plot(log_los ,preventable ,xlim=loslims ,type ="n",ylim=c(0 ,.2) )
lines(los.grid ,pfit ,lwd =2, col =" blue")
matlines(los.grid ,se.bands ,lwd =1, col =" blue",lty =3)






#############Some other regression models (e.x. the ones for Nov 27's meeting) and analysis (e.x. plotting residuals)----------------------------------------------------------
model <- glm(formula = preventable ~ waiting_to_be_seen+ ageyrs + as.factor(shift_block)+ as.factor(site)+as.factor(ctas)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model)

#Regression Models (Individual Level)

model1 <- glm(formula = AEoutcome ~ los + los*as.factor(shift_block) +los*as.factor(primary_dx_grouping) +ageyrs + number_staff + as.factor(shift_block)+as.factor(ctas)+ as.factor(site)+as.factor(pt_disposition)+as.factor(primary_dx_grouping), family = "binomial", data = data)
summary(model1)  

model1_2 <- glm(formula = AEoutcome ~ avg_los_shift + ageyrs + total_num_staff + as.factor(shift_block)+as.factor(ctas)+ as.factor(site)+as.factor(pt_disposition) , family = "binomial", data = data)
summary(model1_2)

model1_3 <- glm(formula = preventable ~los+ los*crowded_shift+ los*as.factor(shift_block)+crowded_shift + ageyrs + number_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model1_3)

model1_4 <- glm(formula = AEoutcome ~los+ los*los_discontinuity+ los_discontinuity+ageyrs + total_num_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas) , family = "binomial", data = data)
summary(model1_4)

model1_5 <- glm(formula = preventable ~ waiting_to_be_seen + ageyrs + total_num_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas) , family = "binomial", data = data)
summary(model1_5)

#model1_6 <- glm(formula = preventable ~ waiting_to_be_seen + waiting_to_be_seen*wtbs_discontinuity+ wtbs_discontinuity+ageyrs + total_num_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas) , family = "binomial", data = data)
#summary(model1_6)

model2 <- glm(formula = AEoutcome ~ los + los*as.factor(shift_block) + los*as.factor(ctas) + ageyrs + as.factor(shift_block)+as.factor(ctas)+ as.factor(site) , family = "binomial", data = data)
summary(model2)

model3 <- glm(formula = preventable ~ los + los*as.factor(shift_block) + ageyrs + as.factor(shift_block)+as.factor(ctas)+ as.factor(site) , family = "binomial", data = data)
summary(model3)

model3_3 <- glm(formula = preventable ~los+ los*crowded_shift+ crowded_shift + ageyrs + total_num_staff + as.factor(shift_block)+as.factor(ctas)+ as.factor(site) , family = "binomial", data = data)
summary(model3_3)


#Regression Models (Shift Level)

data_shift <- data %>% select(shift_date, shift_block, site, ctas, num_ae_shift, num_preventable_ae_shift, avg_los_shift, avg_staff_shift, num_lwbs_shift, avg_census_shift, num_discharged_shift, num_admitted_shift )
data_shift <- unique(data_shift)

model4 <- glm(formula= num_ae_shift ~ avg_los_shift + avg_staff_shift+ num_lwbs_shift + num_discharged_shift + num_admitted_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model4)

model4_1 <- glm(formula= num_preventable_ae_shift ~ avg_los_shift+ avg_wtbs_shift* + avg_staff_shift+ num_lwbs_shift + num_admitted_shift+num_discharged_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data)
summary(model4_1)

model5 <- glm(formula= num_ae_shift ~ avg_los_shift + avg_los_shift* as.factor(ctas) + avg_staff_shift+ num_lwbs_shift + avg_census_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model5)

model5_1 <- glm(formula= num_preventable_ae_shift ~ avg_los_shift + avg_los_shift* as.factor(ctas) + avg_staff_shift+ num_lwbs_shift + avg_census_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model5_1)

# Running on the data with AE

data_AE <- subset (data, data$AEoutcome==TRUE)

#model6 <- glm(formula = preventable ~ waiting_to_be_seen+ ageyrs + as.factor(shift_block)+as.factor(ctas)+ as.factor(site) , family = "binomial", data = data_AE)
#summary(model6)

# plot residual

res<- residuals(model1_3,type="response")

data_res <- data

data_res$residual=NA
data_res$residual2=NA
View(head(data_res))
model1_3 <- glm(formula = AEoutcome ~los+ los*crowded_shift+ crowded_shift + ageyrs + total_num_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas) , family = "binomial", data = data_res)
data_res[names(residuals(model1_3,type="response")),"residual"] <- residuals(model1_3,type="response")
data_res[names(model1_3$residuals),"residual2"] <- model1_3$residuals
plot(data_res$los, data_res$residual)
cor.test(data_res$los, data_res$residual2)

#Regression Models for Meeting with Quynh (Nov 27, 2018)

model1 <- glm(formula = preventable ~ los + los*as.factor(shift_block)+los*crowded_shift+crowded_shift+los*as.factor(diagnosis)+ageyrs + total_num_staff + as.factor(shift_block)+as.factor(ctas)+ as.factor(site)+as.factor(pt_disposition)+as.factor(diagnosis), family = "binomial", data = data)
summary(model1)  

model1_2 <- glm(formula = AEoutcome ~ los + los*as.factor(shift_block) +los*as.factor(primary_dx_grouping) +ageyrs + total_num_staff + as.factor(shift_block)+as.factor(ctas)+ as.factor(site)+as.factor(pt_disposition)+as.factor(primary_dx_grouping), family = "binomial", data = data)
summary(model1_2)  

model2 <- glm(formula = AEoutcome ~los+ los*crowded_shift+ los*as.factor(shift_block)+los*as.factor(diagnosis)+crowded_shift + ageyrs + total_num_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas)+as.factor(pt_disposition)+as.factor(diagnosis), family = "binomial", data = data)
summary(model2)

#Change threshold to 70th perecntile
attach(data)
model3 <- glm(formula = preventable ~los+ los*crowded_shift+los*as.factor(shift_block)+crowded_shift + Los*as.factor(diagnosis)+ ageyrs + total_num_staff + as.factor(shift_block)+ as.factor(site)+as.factor(ctas)+as.factor(pt_disposition)+as.factor(diagnosis), family = "binomial", data = data)
summary(model3)

data_shift <- data %>% select(shift_date, shift_block, site, ctas, num_ae_shift, num_preventable_ae_shift, avg_los_shift, avg_staff_shift, num_lwbs_shift, avg_census_shift, num_discharged_shift, num_admitted_shift )
data_shift <- unique(data_shift)

model4 <- glm(formula= num_ae_shift ~ avg_los_shift + avg_staff_shift+ num_lwbs_shift + num_discharged_shift + num_admitted_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model4)

model5 <- glm(formula= num_preventable_ae_shift ~ avg_los_shift + avg_staff_shift+ num_lwbs_shift + num_admitted_shift+num_discharged_shift+ as.factor(site)+ as.factor(shift_block)+as.factor(ctas), family = "poisson", data = data_shift)
summary(model5)

### Defining a new threshold for waiting to be seen and wtbs

model1 <- glm(formula = preventable ~ los+ los*crowded_wtbs + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model1)  #got result for this with threshold 0.8 for crowded_wtbs

model2 <- glm(formula = preventable ~ los+ crowded_wtbs + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model2)  #got result for this with threshold of 0.45 for crowded_wtbs (10)

model3 <- glm(formula = ae_index ~ los+ crowded_wtbs_time_shift + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model3)  #got result for this with threshold of 0.45 for wtbs_time_threshold (58 min)

model4 <- glm(formula = ae_index ~ los+ avg_wtbs_time_shift*crowded_wtbs_time_shift + as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model4)  #....Try more models

model1 <- glm(formula = preventable_num ~ los + wtbshour*crowded_wtbs + as.factor(shift_block)+as.factor(ctas)+ as.factor(site)+as.factor(pt_disposition) , family = "poisson", data = data)
summary(model1) 

##########################search about smooth.spline

#data8 <- data[!is.na(data$los) & !is.na(data$wtbshour),]
#fit <-  smooth.spline(data8$wtbshour, data8$los, df=10)
#xx <- unique(sort(c(seq(0, 60, by=0.5), kn<- unique(data8$wtbshour))))
#lines(pp <- predict(fit,xx), col="red")

#lines(ss, lty = 2, col = "red")
#data8 <- data[!is.na(data$los) & !is.na(data$wtbshour),]
#attach(data8)
#fit <-  smooth.spline(data8$wtbshour, data8$los, df=3)
#wtbslims =range(wtbshour)
#wtbs.grid=seq (from=wtbslims[1], to=wtbslims[2])
#preds=predict (fit ,newdata =list(wtbshour=wtbs.grid),se=T)
#se.bands= cbind(preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit)
#plot(wtbshour, los ,xlim=wtbslims ,type ="n",ylim=c(0 ,500) )
#lines(wtbs.grid , preds$fit ,lwd =2, col =" blue")
#matlines(wtbs.grid ,se.bands ,lwd =1, col =" blue",lty =3)

####Running individual model of los on AE with shift level data

data <- data %>% group_by(site, shift_block, shift_date, ctas) %>% mutate(sum_los_others_shift=sum(los,na.rm = TRUE))
data <- data %>% group_by(site, shift_block, shift_date, ctas) %>% mutate(number_los_others_shift=n_distinct(record_id[los>0]))
data$mean_los_others_shift <- (data$sum_los_others_shift-data$los)/(data$number_los_others_shift-1)

model3 <- glm(formula = preventable_EDrelated ~ mean_los_others_shift+ ageyrs + as.factor(chronic_condition)+as.factor(ctas)+ as.factor(site) + as.factor(shift_block)+as.factor(pt_disposition), family = "binomial", data = data)
summary(model3)  


########After the meeting 
model <- glm(formula = preventable_EDrelated ~ wtbshour+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition)*as.factor(acuity), family = "binomial", data = data)
summary(model)   #Patients with chronic condition in acuity 2 has lower AE compared to chronic condition in acuity 1!

#Releveling acuity: #Effect of chronic condition for acuity 2 on AE is not significant.
data2 <- data   
data2$acuity <- as.factor(data2$acuity)
data2 <- within(data2, acuity <- relevel(acuity, ref ="2"))
model <- glm(formula = preventable_EDrelated ~ wtbshour+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition)*as.factor(acuity), family = "binomial", data = data2)
summary(model)

model <- glm(formula = preventable_EDrelated ~ wtbshour*as.factor(acuity)+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = data2)
summary(model)    #Statistically, we cannot tell effects of wtbshour for diffrent acuity levels are different from each other!

model <- glm(formula = preventable_EDrelated ~ wtbshour*as.factor(chronic_condition)+ number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(acuity), family = "binomial", data = data)
summary(model)

data3 <- data2
data3$chronic_condition <- as.factor(data3$chronic_condition)
data3 <- within(data3, chronic_condition <- relevel(chronic_condition, ref ="1"))
model <- glm(formula = preventable_EDrelated ~ wtbshour*as.factor(chronic_condition)*as.factor(acuity)+ number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition), family = "binomial", data = data)
summary(model)

temp1 <- data[data$acuity==1,] #-------------------- 
model1 <- glm(formula = preventable_EDrelated ~ los+ wtbshour+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp1)
summary(model1)   #The effect of wtbs on preventable_EDrelated AE for acuity 1 is not significant (even with considering piece-wise linear models)

threshold <- 13 #Did not get any result
temp1$u_wtbshour <- temp1$wtbshour-threshold
temp1$u_wtbshour[temp1$u_wtbshour<0] <- 0
temp1$v_wtbshour <- 0
temp1$v_wtbshour[temp1$wtbshour>threshold] <- -1
model <- glm(formula = preventable_EDrelated ~ los+wtbshour + u_wtbshour +ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp1)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])


temp2 <- data[data$acuity==2,] #-----------------------------------
model2 <- glm(formula = preventable_EDrelated ~ wtbshour+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp2)
summary(model2)  #If we add los, we see significance for both of los and wtbshour!

threshold <- 17 #(Got some results)
temp2$u_wtbshour <- temp2$wtbshour-threshold
temp2$u_wtbshour[temp2$u_wtbshour<0] <- 0
temp2$v_wtbshour <- 0
temp2$v_wtbshour[temp2$wtbshour>threshold] <- -1
model <- glm(formula = preventable_EDrelated ~ wtbshour + u_wtbshour +v_wtbshour + number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp2)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])


temp3 <- data[data$acuity==3,] #------------------------------------
model3 <- glm(formula = preventable_EDrelated ~ wtbshour+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp3)
summary(model3)

threshold <- 6 #(Got some results)
temp3$u_wtbshour <- temp3$wtbshour-threshold
temp3$u_wtbshour[temp3$u_wtbshour<0] <- 0
temp3$v_wtbshour <- 0
temp3$v_wtbshour[temp3$wtbshour>threshold] <- -1
model <- glm(formula = preventable_EDrelated ~ wtbshour + u_wtbshour+ v_wtbshour + number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp3)
summary(model) 
new_threshold <- threshold+(model$coefficients[4]/model$coefficients[3])

##################################
temp4 <- rbind(temp2,temp3)
model4 <- glm(formula = preventable_EDrelated ~ wtbshour*as.factor(acuity)+number_staff+ageyrs+ as.factor(shift_block)+ as.factor(site)+ as.factor(pt_disposition)+as.factor(chronic_condition), family = "binomial", data = temp4)
summary(model4) 

##############################
plot(temp4$preventable_EDrelated, temp4$wtbshour)

