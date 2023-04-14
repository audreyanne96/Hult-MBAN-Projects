# A2: Hospital Readmissions EDA & Modeling
# Visualizing & Analyzing Data with R: Methods & Tools - DAT-5323 - BMBAN1

# Audrey Anne Arocha
# April 8, 2023

############### ENVIRONMENT ###############

# Setting working directory
setwd("~/Desktop/MBAN R/hult-git-R/personalFiles/A2")

# Setting libraries
library(tidyverse)
library(dplyr)
library(mice)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(caret)
library(vtreat)
library(forcats)
library(tidytext)
library(tm)
library(textdata)
library(stringr)
library(SnowballC)
library(pROC)
library(ROCR)
library(MLmetrics)
library(fastDummies)
library(ROSE)
library(randomForest)
library(ranger)
library(rpart)

# Accessing databases (r_ for raw)
# Hospital Data
r_hos_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv')
r_hos_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv')
# Medicine Data
r_med_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv')
r_med_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv')
# Patients Data
r_pat_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv')
r_pat_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv')

# Merge raw files
summary(r_hos_train)
r_train <- left_join(r_hos_train,r_med_train, by = 'tmpID') %>%
           left_join(r_pat_train, by = 'tmpID')
r_test <- left_join(r_hos_test, r_med_test, by = 'tmpID') %>%
          left_join(r_pat_test, by = 'tmpID')

############### FUNCTION CREATION ###############

# ANALYSIS
# Show relevant summary statistics and frequency table
analyze_column <- function(df, col_name) {
  # Count missing and blank values
  n_na <- sum(is.na(df[[col_name]]))
  n_blank <- sum(df[[col_name]] == "")
  # Count unique values
  n_unique <- length(unique(df[[col_name]]))
  # Create frequency table
  freq_table <- data.frame(table(df[[col_name]]))
  # Return summary statistics and frequency table
  if (length(freq_table$Var1) < 10) {
    return(list(n_na = n_na, n_blank = n_blank, n_unique = n_unique, freq_table = freq_table))
  } else {
    return(list(n_na = n_na, n_blank = n_blank, n_unique = n_unique, freq_table = head(freq_table,10)))
  }
}

# FOR CLEANING
# Blank to NAs, Not Available to NAs, ? to NAs
cl_toNAFactor <- function(df, col_name) {
  # Values to NA
  df[[col_name]] <- ifelse(df[[col_name]] == "", NA, df[[col_name]])
  df[[col_name]] <- ifelse(df[[col_name]] == "Not Available", NA, df[[col_name]])
  df[[col_name]] <- ifelse(df[[col_name]] == "?", NA, df[[col_name]])
  # To Factor
  df[[col_name]] <- factor(df[[col_name]], ordered = FALSE)
  # Print column information
  str(df[[col_name]])
  cat("Number of missing values:", sum(is.na(df[[col_name]])), "\n")
  # Return the cleaned dataframe
  return(df)
}
# Clean all medications
cl_medication <- function(df, columns) {
  for (col in columns) {
    df[,col] <- factor(df[,col], ordered = TRUE, levels = c("No", "Down", "Steady", "Up"))
  }
  return(df)
}

# FOR PLOTTING
# Plot to bar graph with frequencies per column
pl_bar_freq <- function(df,col_name, x_label) {
  col_table <- data.frame(table(df[,col_name]))
  ggplot(col_table, aes(x=Var1, y=Freq)) +
    geom_bar(stat = "identity", fill = "darkslategray") +
    labs(x = x_label, y="") + 
    geom_text(aes(label = Freq), position = position_stack(), vjust = 2, size = 4, color = "white") +
    theme_minimal()
}
# Plot points with frequencies
pl_point_freq <- function(df, col_name,x_label){
  tmp_df <- data.frame(table(df[,col_name]))
  ggplot(tmp_df, aes(x=Var1, y=Freq)) +
    geom_point(color = "darkslategray") +
    geom_smooth(method = "loess") +
    labs(x = x_label, y = "") +
    theme_minimal()
}

############### DATA UNDERSTANDING AND DATA PREPARATION (CLEANING AND TRANSFORMATION) ###############

# No manual subsetting between training and test since test is already set, exploration will be done on full train set
# No manual subsetting for validation, will later use cross-validation in modeling

colnames(r_train)
dim(r_train) # 7500,44
train <- r_train # creates a separate df for cleaned training dataset
test <- r_test # creates a separate df for cleaned test dataset

# PER COLUMN

# tmpID - FOR REMOVAL
analyze_column(r_train,"tmpID") # 0 NA, 0 Blank, 7500 unique

# admission_type_id
analyze_column(r_train,"admission_type_id") # 0 NA, 532 Blank, 7 unique
# cleaning in train
train <- cl_toNAFactor(train,"admission_type_id") # 1000 nulls
# cleaning in test
test <- cl_toNAFactor(test,"admission_type_id") # 335 nulls
# checking
analyze_column(train,"admission_type_id") # 1000 NA, NA Blank, 6 unique
# plotting train
pl_bar_freq(train,"admission_type_id","Admission Types")
# lumping infrequent categories to others
train$admission_type_id_c <- fct_lump(train$admission_type_id, n=3, other_level = "Other")
analyze_column(train,"admission_type_id_c") # 1000 NA, NA blank, 5 unique
pl_bar_freq(train,"admission_type_id_c","Admission Types")
# for test
test$admission_type_id_c <- fct_lump(test$admission_type_id, n=3, other_level = "Other")
# next steps - null imputation, dummify

# discharge_disposition_id
analyze_column(r_train,"discharge_disposition_id") # 0 NA, 353 Blank, 20 unique
# cleaning in train
train <- cl_toNAFactor(train,"discharge_disposition_id") # 353 nulls
# cleaning in test
test <- cl_toNAFactor(test,"discharge_disposition_id") # 116 nulls
# checking
analyze_column(train,"discharge_disposition_id") # 353 NA, NA blank, 20 unique
# plotting train
pl_bar_freq(train,"discharge_disposition_id","Discharge Type")
# reducing categories to To Home, To Hospice, Expired, Transferred, Others
unique(train$discharge_disposition_id)
train$discharge_disposition_id_c <- tolower(train$discharge_disposition_id)
train$discharge_disposition_id_c <- ifelse(is.na(train$discharge_disposition_id_c),NA,
                                           ifelse(grepl("to home", train$discharge_disposition_id_c), "ToHome",
                                                  ifelse(grepl("expired", train$discharge_disposition_id_c), "Expired",
                                                         ifelse(grepl("transferred", train$discharge_disposition_id_c), "Transferred", "Others"))))
train$discharge_disposition_id_c <- factor(train$discharge_disposition_id_c)
# for test
test$discharge_disposition_id_c <- tolower(test$discharge_disposition_id)
test$discharge_disposition_id_c <- ifelse(is.na(test$discharge_disposition_id_c),NA,
                                          ifelse(grepl("to home", test$discharge_disposition_id_c), "ToHome",
                                                 ifelse(grepl("expired", test$discharge_disposition_id_c), "Expired",
                                                        ifelse(grepl("transferred", test$discharge_disposition_id_c), "Transferred", "Others"))))
test$discharge_disposition_id_c <- factor(test$discharge_disposition_id_c)
# checking
analyze_column(train, "discharge_disposition_id_c") # 353 NA, NA blank, 6 unique
tmp <- data.frame(table(train$discharge_disposition_id,train$discharge_disposition_id_c))
tmp <- tmp[tmp$Freq != 0,]
pl_bar_freq(train,"discharge_disposition_id_c","Discharge Type")
# next steps - null imputation

# admission_source_id
analyze_column(r_train,"admission_source_id") # 0 NA, 699 Blank, 11 unique
# cleaning in train
train <- cl_toNAFactor(train,"admission_source_id") # 708 nulls
# cleaning in test
test <- cl_toNAFactor(test,"admission_source_id") # 244 nulls
# checking
analyze_column(train,"admission_source_id") # 708 NA, NA blank, 10 unique
# plotting train
pl_bar_freq(train,"admission_source_id","Admission Source")
# reducing categories to Emergency Room, Referral, Transfer, Others
unique(train$admission_source_id)
train$admission_source_id_c <- tolower(train$admission_source_id)
train$admission_source_id_c <- ifelse(is.na(train$admission_source_id_c),NA,
                                      ifelse(grepl("emergency", train$admission_source_id_c), "ER",
                                                  ifelse(grepl("referral",train$admission_source_id_c), "Referral",
                                                         ifelse(grepl("transfer", train$admission_source_id_c), "Transfer","Others"))))
train$admission_source_id_c <- factor(train$admission_source_id_c)
# for test
test$admission_source_id_c <- tolower(test$admission_source_id)
test$admission_source_id_c <- ifelse(is.na(test$admission_source_id_c),NA,
                                      ifelse(grepl("emergency", test$admission_source_id_c), "ER",
                                             ifelse(grepl("referral",test$admission_source_id_c), "Referral",
                                                    ifelse(grepl("transfer", test$admission_source_id_c), "Transfer","Others"))))
test$admission_source_id_c <- factor(test$admission_source_id_c)
# checking
analyze_column(train, "admission_source_id_c") # 353 NA, NA blank, 6 unique
tmp <- data.frame(table(train$admission_source_id,train$admission_source_id_c))
tmp <- tmp[tmp$Freq != 0,]
pl_bar_freq(train,"admission_source_id_c","Admission Source")
# next steps - null imputation

# time_in_hospital
analyze_column(r_train,"time_in_hospital") # 0 NA, 0 Blank, 14 unique, continuous
# cleaning train
str(train$time_in_hospital) # already int
sum(is.na(train$time_in_hospital)) # 0 nulls
# cleaning test
str(test$time_in_hospital) # already int
sum(is.na(test$time_in_hospital)) # 0 nulls
# plotting train
pl_bar_freq(train,"time_in_hospital","Number of Days")
# next steps - none

# medical_specialty - FOR REMOVAL
analyze_column(r_train,"medical_specialty") # 0 NA, 0 Blank, 50 unique
sum(r_train$medical_specialty == "?") # 3099 ?s
# cleaning train
train <- cl_toNAFactor(train, "medical_specialty") # 3099 NAs
# cleaning test
test <- cl_toNAFactor(test, "medical_specialty") # 1001 NAs
# checking
analyze_column(train,"medical_specialty") # 3099 NA, NA blank, 50 unique
# plotting train
pl_bar_freq(train,"medical_specialty","Medical Specialty")
# lumping infrequent categories to others
train$medical_specialty_c <- fct_lump(train$medical_specialty, n = 5, other_level = "Other")
analyze_column(train,"medical_specialty_c") # 3099 NA, NA blank, 70 unique
pl_bar_freq(train,"medical_specialty_c","Medical Specialty")
# for test
test$medical_specialty_c <- fct_lump(test$medical_specialty, n = 5, other_level = "Other")
# remove, even with lumping still difficult to decode and more than 40% are ? or NA

# num_lab_procedures
analyze_column(r_train,"num_lab_procedures") # 0 NA, 0 Blank, 107 unique, continuous
# cleaning train
str(train$num_lab_procedures) # already integer
sum(is.na(train$num_lab_procedures)) # 0 nulls
# cleaning test
str(test$num_lab_procedures) # already integer
sum(is.na(test$num_lab_procedures)) # 0 nulls
# plotting train
pl_point_freq(train,"num_lab_procedures","Number of Lab Procedures")
# looks to be normally distributed
# next steps - none, retain continuous

# num_procedures
analyze_column(r_train,"num_procedures") # 0 NA, 0 Blank, 7 unique, continuous
# cleaning train
str(train$num_procedures) # already integer
sum(is.na(train$num_procedures)) # 0 nulls
# cleaning test
str(test$num_procedures) # already integer
sum(is.na(test$num_procedures)) # 0 nulls
# plotting train
pl_bar_freq(train,"num_procedures","Number of Procedures")
# next steps - none, retain continuous

# num_medications
analyze_column(r_train,"num_medications") # 0 NA, 0 Blank, 67 unique, continuous
# cleaning train
str(train$num_medications) # already integer
sum(is.na(train$num_medications)) # 0 nulls
# cleaning test
str(test$num_procedures) # already integer
sum(is.na(test$num_medications)) # 0 nulls
# plotting train
pl_point_freq(train,"num_medications","Number of Medications")
# next steps - none, retain continuous

# number_outpatient
analyze_column(r_train,"number_outpatient") # 0 NA, 0 Blank, 19 unique
# cleaning train
str(train$number_outpatient) # already integer
sum(is.na(train$number_outpatient)) # 0 nulls
# cleaning test
str(test$number_outpatient) # already integer
sum(is.na(test$number_outpatient)) # 0 nulls
# plotting train
pl_point_freq(train,"number_outpatient","Number of Outpatients")
# next steps - none, retain continuous

# number_emergency
analyze_column(r_train,"number_emergency") # 0 NA, 0 Blank, 12 unique
# cleaning train
str(train$number_emergency) # already integer
sum(is.na(train$number_emergency)) # 0 nulls
# cleaning test
str(test$number_emergency) # already integer
sum(is.na(test$number_emergency)) # 0 nulls
# plotting train
pl_point_freq(train,"number_emergency","Number of Emergency")
# next steps - none, retain continuous

# number_inpatient
analyze_column(r_train,"number_inpatient") # 0 NA, 0 Blank, 11 unique
# cleaning train
str(train$number_inpatient) # already integer
sum(is.na(train$number_inpatient)) # 0 nulls
# cleaning test
str(test$number_inpatient) # already integer
sum(is.na(test$number_inpatient)) # 0 nulls
# plotting train
pl_point_freq(train,"number_inpatient","Number of Inpatients")
# next steps - none, retain continuous

# number_diagnoses
analyze_column(r_train,"number_diagnoses") # 0 NA, 0 blank, 9 unique
# cleaning train
str(train$number_diagnoses) # already integer
sum(is.na(train$number_diagnoses)) # 0 nulls
# cleaning test
str(test$number_diagnoses) # already integer
sum(is.na(test$number_diagnoses)) # 0 nulls
# plotting train
pl_point_freq(train,"number_diagnoses","Number of Diagnoses")
# next steps - none, retain continuous

# diag_1_desc
analyze_column(r_train,"diag_1_desc") # 0 NA, 2 blank, 430 unique, descriptive
# cleaning train
train <- cl_toNAFactor(train,"diag_1_desc") # 2 nulls
# cleaning test
test <- cl_toNAFactor(test,"diag_1_desc") # 0 nulls
# next steps - text analysis

# diag_2_desc
analyze_column(r_train,"diag_2_desc") # 0 NA, 42 blank, 383 unique, descriptive
# cleaning train
train <- cl_toNAFactor(train, "diag_2_desc") # 42 nulls
# cleaning test
test <- cl_toNAFactor(test, "diag_2_desc") # 17 nulls
# next steps - text analysis

# diag_3_desc
analyze_column(r_train,"diag_3_desc") # 0 NA, 144 blank, 423 unique
# cleaning train
train <- cl_toNAFactor(train,"diag_3_desc") # 144 nulls
# cleaning test
test <- cl_toNAFactor(test,"diag_3_desc") # 64 nulls
# next steps - text analysis

# max_glu_serum
analyze_column(r_train,"max_glu_serum") # 0 na, 0 blank, 4 unique
# cleaning train
#train$max_glu_serum <- factor(train$max_glu_serum, ordered = T, levels = c("None","Norm",">200",">300"))
str(train$max_glu_serum_c)
sum(is.na(train$max_glu_serum_c)) # 0 nulls
# cleaning test
#test$max_glu_serum <- factor(test$max_glu_serum, ordered = T, levels = c("None","Norm",">200",">300"))
sum(is.na(test$max_glu_serum)) # 0 nulls
table(test$max_glu_serum) 
# plotting train
pl_bar_freq(train,"max_glu_serum","Glucose Serum Test")
# Transform to None, Norm, Abnorm
train$max_glu_serum_c <- tolower(train$max_glu_serum)
train$max_glu_serum_c <- ifelse(grepl("none",train$max_glu_serum_c),"None",
                                ifelse(grepl("norm",train$max_glu_serum_c), "Norm", "Abnorm"))
train$max_glu_serum_c <- factor(train$max_glu_serum_c, ordered = T, levels = c("None","Norm","Abnorm"))
table(train$max_glu_serum_c)
sum(is.na(train$max_glu_serum_c))
# For test
test$max_glu_serum_c <- tolower(test$max_glu_serum)
test$max_glu_serum_c <- ifelse(grepl("none",test$max_glu_serum_c),"None",
                                ifelse(grepl("norm",test$max_glu_serum_c), "Norm", "Abnorm"))
test$max_glu_serum_c <- factor(test$max_glu_serum_c, ordered = T, levels = c("None","Norm","Abnorm"))
table(test$max_glu_serum_c)
# plotting train
pl_bar_freq(train,"max_glu_serum_c","Glucose Serum Test")
# next steps - dummify norm and abnorm

# A1Cresult
analyze_column(r_train,"A1Cresult") # 0 na, 0 blank, 4 unique
# cleaning train
#train$A1Cresult <- factor(train$A1Cresult, ordered = T, levels = c("None","Norm",">7",">8"))
str(train$A1Cresult)
sum(is.na(train$A1Cresult)) # 0 nulls
# cleaning test
#test$A1Cresult <- factor(test$A1Cresult, ordered = T, levels = c("None","Norm",">7",">8"))
sum(is.na(test$A1Cresult)) # 0 nulls
# plotting train
pl_bar_freq(train,"A1Cresult","A1C Results")
# Transform to None, Norm, Abnorm
train$A1Cresult_c <- tolower(train$A1Cresult)
train$A1Cresult_c <- ifelse(grepl("none",train$A1Cresult_c),"None",
                                ifelse(grepl("norm",train$A1Cresult_c), "Norm", "Abnorm"))
train$A1Cresult_c <- factor(train$A1Cresult_c, ordered = T, levels = c("None","Norm","Abnorm"))
table(train$A1Cresult_c)
sum(is.na(train$A1Cresult_c))
# For test
test$A1Cresult_c <- tolower(test$A1Cresult)
test$A1Cresult_c <- ifelse(grepl("none",test$A1Cresult_c),"None",
                               ifelse(grepl("norm",test$A1Cresult_c), "Norm", "Abnorm"))
test$A1Cresult_c <- factor(test$A1Cresult_c, ordered = T, levels = c("None","Norm","Abnorm"))
table(test$A1Cresult_c)
# next steps - dummify norm and abnorm

# various medications
analyze_column(r_train,"troglitazone") # usually 0 na, 0 blank, 4 unique for all medications
# cleaning train
colnames(r_med_train)
str(r_med_train)
sum(is.na(r_med_train)) # 0 nulls
sum(r_med_train == "") # 0 blanks
cl_col_meds <- c("metformin","repaglinide","nateglinide","chlorpropamide",
                 "glimepiride","acetohexamide","glipizide","glyburide",
                 "tolbutamide","pioglitazone","rosiglitazone","acarbose",
                 "miglitol","troglitazone","tolazamide","examide",
                 "citoglipton","insulin")
train <- cl_medication(train,cl_col_meds)
# cleaning test
test <- cl_medication(test,cl_col_meds)
# next steps - remove medications that are all No

# change
analyze_column(r_train,"change") # 0 na, 0 blank, 2 unique
# cleaning train
train <- cl_toNAFactor(train, "change") # 0 nulls
# cleaning test
test <- cl_toNAFactor(test, "change") # 0 nulls
# plotting train
pl_bar_freq(train,"change","Change")
# 1/0
train$change_c <- tolower(train$change)
train$change_c <- ifelse(train$change_c == "ch",1,0)
# for test
test$change_c <- tolower(test$change)
test$change_c <- ifelse(test$change_c == "ch",1,0)
# checking
analyze_column(train,"change_c") # 0, 0, 2
pl_bar_freq(train,"change_c","Change")

# diabetesMed
analyze_column(r_train,"diabetesMed") # 0 na, 0 blank, 2 unique
# cleaning train
train <- cl_toNAFactor(train, "diabetesMed")
# cleaning test
test <- cl_toNAFactor(test, "diabetesMed")
# plotting train
pl_bar_freq(train,"diabetesMed","Diabetes Medication")
# 1/0
train$diabetesMed_c <- tolower(train$diabetesMed)
train$diabetesMed_c <- ifelse(train$diabetesMed_c == "yes",1,0)
# for test
test$diabetesMed_c <- tolower(test$diabetesMed)
test$diabetesMed_c <- ifelse(test$diabetesMed_c == "yes",1,0)
# checking
analyze_column(train,"diabetesMed_c") # 0, 0, 2
pl_bar_freq(train,"diabetesMed_c","Diabetes Medication")

# race
analyze_column(r_train,"race") # 0 na, 0 blank, 6 unique
sum(r_train$race == "?") # 172 ?s
# cleaning train
train <- cl_toNAFactor(train, "race") # 172 nulls
# cleaning test
test <- cl_toNAFactor(test, "race") # 49 nulls
# plotting train
pl_bar_freq(train,"race","Ethnicity")
# lumping top categories
train$race_c <- fct_lump(train$race, n=2, other_level = "Other")
analyze_column(train,"race_c") # 1000 NA, NA blank, 5 unique
pl_bar_freq(train,"race_c","Ethnicity")
# for test
test$race_c <- fct_lump(test$race, n=2, other_level = "Other")

# gender
analyze_column(r_train,"gender") # 0 na, 0 blank, 2 unique
# cleaning train
train <- cl_toNAFactor(train,"gender") # 0 nulls
# cleaning test
test <- cl_toNAFactor(test,"gender") # 0 nulls
# plotting train
pl_bar_freq(train,"gender","Gender")
# is_female
train$isfemale <- tolower(train$gender)
train$isfemale <- ifelse(train$isfemale == "female",1,0)
analyze_column(train,"isfemale")
pl_bar_freq(train,"isfemale","Female")
# for test
test$isfemale <- tolower(test$gender)
test$isfemale <- ifelse(test$isfemale == "female",1,0)

# age
analyze_column(r_train,"age") # 0 na, 0 blank, 101 unique, continuous
# cleaning train
str(train$age) # already integer
sum(is.na(train$age)) # 0 nulls
# cleaning test
str(test$age) # already integer
sum(is.na(test$age)) # 0 nulls
# plotting train
pl_point_freq(train,"age","Age")

# wgt
analyze_column(r_train, "wgt") # 0 na, 0 blank, 196 unique, continuous
# cleaning train
str(train$wgt) # already integer
sum(is.na(train$wgt)) # 0 nulls
# cleaning test
str(test$wgt) # already integer
sum(is.na(test$wgt)) # 0 nulls
# plotting train
pl_point_freq(train,"wgt","Weight")

# payer_code - FOR REMOVAL
analyze_column(r_train,"payer_code") # 0 na, 0 blank, 16 unique
sum(r_train$payer_code == "?") # 3981 ?s
# drop, irrelevant and code is difficult to interpret or assume meanings from

# readmitted_y (DEPENDENT VARIABLE)
analyze_column(r_train,"readmitted_y") # 0 na, 0 blank, 2 unique
str(train$readmitted_y) # logical, 7500
str(test$readmitted_y) # logical, 2500
# plot train
pl_bar_freq(train,"readmitted_y","Readmission (Target Variable)")
# next steps - remove from test, balance in training and modeling 

############### DATA PREPARATION - TEXT ANALYSIS FOR DIAGNOSES ###############

# TEXT ANALYSIS FOR DIAGNOSIS
train$diag_1_desc_c <- tolower(train$diag_1_desc)
train$diag_1_desc_c <- gsub("[^[:alnum:]\\s]+", " ", train$diag_1_desc_c)
train$diag_2_desc_c <- tolower(train$diag_2_desc)
train$diag_2_desc_c <- gsub("[^[:alnum:]\\s]+", " ", train$diag_2_desc_c)
train$diag_3_desc_c <- tolower(train$diag_3_desc)
train$diag_3_desc_c <- gsub("[^[:alnum:]\\s]+", " ", train$diag_3_desc_c)
train$diag_combined <- paste(train$diag_1_desc_c, train$diag_2_desc_c, train$diag_3_desc_c)
# remove stop words - other words were added based on iterations of checking top words
train$diag_combined <- removeWords(train$diag_combined, c(stopwords("english"),"type","without","ani","mention","cor","state","stated","specified","unspecified","control","uncontrolled"))
# create corpus
corpus <- Corpus(VectorSource(train$diag_combined))
# create document-term matrix
dtm <- DocumentTermMatrix(corpus)
# find frequency of terms
freq_main <- findFreqTerms(dtm)
freq_main <- sapply(freq_main, function(x) sum(grepl(x, train$diag_combined)))
freq_main <- data.frame(words = names(freq_main), values = freq_main)
freq_main <- freq_main[order(freq_main$values, decreasing = TRUE), ]
head(freq_main,10)
# combine similar words
freq_main$stems <- wordStem(freq_main$words, language = "en")
freq_stems <- aggregate(values ~ stems, freq_main, sum)
freq_stems <- freq_stems[order(freq_stems$values, decreasing = TRUE), ]
head(freq_stems,10)
# select top words
top_words <- head(freq_main[order(-freq_main$values), "words"], 10)
top_wordstems <- head(freq_stems[order(-freq_stems$values), "stems"], 10)
# count of top words
top_words_matrix <- sapply(top_words, function(word) str_count(train$diag_combined, word))
top_wordstems_matrix <- sapply(top_wordstems, function(word) str_count(train$diag_combined, word))
head(top_words_matrix)
head(top_wordstems_matrix) 
# values between words and stems are the same where it should be
# will use stems because of "hypertens"
# cbind to main database but words will have prefix "diag_" to indicate it was found in their diagnosis
colnames(top_wordstems_matrix) <- paste0("diag_", colnames(top_wordstems_matrix))
train <- cbind(train, top_wordstems_matrix)
str(train)

# APPLYING TO TEST
test$diag_1_desc_c <- tolower(test$diag_1_desc)
test$diag_1_desc_c <- gsub("[^[:alnum:]\\s]+", " ", test$diag_1_desc_c)
test$diag_2_desc_c <- tolower(test$diag_2_desc)
test$diag_2_desc_c <- gsub("[^[:alnum:]\\s]+", " ", test$diag_2_desc_c)
test$diag_3_desc_c <- tolower(test$diag_3_desc)
test$diag_3_desc_c <- gsub("[^[:alnum:]\\s]+", " ", test$diag_3_desc_c)
test$diag_combined <- paste(test$diag_1_desc_c, test$diag_2_desc_c, test$diag_3_desc_c)
# remove stop words - same stop words from train
test$diag_combined <- removeWords(test$diag_combined, c(stopwords("english"),"type","without","ani","mention","cor","state","stated","specified","unspecified","control","uncontrolled"))
# applying same list of words from train
top_wordstems_matrix_t <- sapply(top_wordstems, function(word) str_count(test$diag_combined, word))
head(top_wordstems_matrix_t)
colnames(top_wordstems_matrix_t) <- paste0("diag_", colnames(top_wordstems_matrix_t))
test <- cbind(test, top_wordstems_matrix_t)
str(test)

############### DATA PREPARATION - SELECTION OF DATA ###############

# Checking column names of train and test
if(all.equal(names(train), names(test))) {
  print("same")
} else {
  print("different")
}

# Determining variance of num_ or number_ columns to see if they are varied enough to have value to the model
tmp1 <- apply(train[,c("num_lab_procedures","num_procedures", "num_medications",
                       "number_outpatient", "number_emergency", "number_inpatient",
                       "number_diagnoses")], 2, var)
print(tmp1)
# lowest is 0.5 - number_emergency, may still have value

# Finding medication columns that has "No" >= 98% and would likely have little to no impact in the training
tmp1 <- colMeans(train == "No")
tmp2 <- which(tmp1 >= 0.98)
colnames(train[tmp2])
# columns are for removal

# Selecting the features
colnames(train)
features <- c(
              #"tmpID", - irrelevant
              #"admission_type_id", "discharge_disposition_id","admission_source_id", - will use cleaned
              "time_in_hospital",
              #"medical_specialty", - will use cleaned
              "num_lab_procedures","num_procedures", "num_medications",
              "number_outpatient", "number_emergency", "number_inpatient",
              "number_diagnoses",
              # "diag_1_desc","diag_2_desc","diag_3_desc", - will use cleaned and findings from text analysis
              # "max_glu_serum","A1Cresult", - will use cleaned
              "metformin",
              #"repaglinide","nateglinide","chlorpropamide", - No >= 98%
              "glimepiride",
              #"acetohexamide", - No >= 98%
              "glipizide","glyburide",
              #"tolbutamide", - No >= 98%
              "pioglitazone","rosiglitazone",
              #"acarbose","miglitol","troglitazone","tolazamide","examide","citoglipton", - No >= 98%
              "insulin",
              # "change","diabetesMed", - will use cleaned
              # "race","gender", - will use cleaned
              "age","wgt",
              # "payer_code", - irrelevant, difficult to interpret
              "readmitted_y",
              "admission_type_id_c","discharge_disposition_id_c","admission_source_id_c",
              # "medical_specialty_c", - difficult to interpret
              "max_glu_serum_c","A1Cresult_c",
              "change_c","diabetesMed_c",
              "race_c","isfemale",
              # "diag_1_desc_c","diag_2_desc_c","diag_3_desc_c", - will use findings from text analysis
              # "diag_combined", - will use findings from text analysis
              "diag_diabet","diag_complic","diag_mellitus","diag_cardia","diag_malign",
              "diag_hypertens","diag_failur","diag_chronic","diag_heart","diag_acut"   
              )
final_train <- train[,features]
final_test <- test[,features]

############### DATA PREPARATION - NULL IMPUTATION ###############

# Check for missing values
colnames(final_train)[apply(is.na(final_train), 2, any)]
colnames(final_test)[apply(is.na(final_test), 2, any)]
# all null values are categorical
pl_bar_freq(final_train,"admission_type_id_c", "Admission Types") # highly skewed to emergency
pl_bar_freq(final_train,"discharge_disposition_id_c", "Discharge Disposition") # highly skewed to To Home and Transferred
pl_bar_freq(final_train,"admission_source_id_c", "Admission Source") # highly skewed to ER and Referral
pl_bar_freq(final_train,"race_c", "Ethnicity") # highly skewed to caucasian

# MICE Null Imputation Method - PMM
# Train
set.seed(123)
final_train <- mice(final_train, method = "pmm")
final_train <- complete(final_train)
sum(is.na(final_train))
# Test
set.seed(123)
final_test <- mice(final_test, method = "pmm")
final_test <- complete(final_test)
sum(is.na(final_test))

############### DATA PREPARATION - DUMMIFY ###############

# Checking column names of train and test
if(all.equal(names(final_train), names(final_test))) {
  print("same")
} else {
  print("different")
}

# Select non-numeric features
tmp_fordummy <- sapply(final_train, function(x) !is.numeric(x))
tmp_fordummy <- names(final_train)[tmp_fordummy]
tmp_fordummy <- tmp_fordummy[tmp_fordummy != "readmitted_y"]
tmp_pattern <- c("_No$", "_None$", "_Other$", "_Others$","_Expired$")
# Expired is removed since it will always lead to not being re-admitted. Removed to allow the model learn from all other features other than being expired.

# Dummifying function
dummify <- function(data, dummy_cols, pattern, target_var) {
  treated_data <- dummy_cols(data, select_columns = dummy_cols,
                             remove_first_dummy = FALSE,
                             remove_most_frequent_dummy = FALSE,
                             ignore_na = FALSE,
                             split = NULL,
                             remove_selected_columns = FALSE)
  treated_data <- treated_data[, !names(treated_data) %in% dummy_cols]
  treated_data <- treated_data[, -grep(paste(pattern, collapse = "|"), names(treated_data))]
  treated_data <- treated_data %>% select(-{{target_var}}, everything(), {{target_var}})
  return(treated_data)
}

treated_train <- dummify(final_train, tmp_fordummy, tmp_pattern, readmitted_y)
treated_test <- dummify(final_test, tmp_fordummy, tmp_pattern, readmitted_y)

# Rename long feature names
treated_train <- treated_train %>%
  rename(admission_type_Elective = admission_type_id_c_Elective, 
         admission_type_Emergency = admission_type_id_c_Emergency,
         admission_type_Urgent = admission_type_id_c_Urgent,
         discharge_ToHome = discharge_disposition_id_c_ToHome,
         discharge_Transferred = discharge_disposition_id_c_Transferred,
         admission_source_ER = admission_source_id_c_ER,
         admission_source_Referral = admission_source_id_c_Referral,
         admission_source_Transfer = admission_source_id_c_Transfer)
treated_test <- treated_test %>%
  rename(admission_type_Elective = admission_type_id_c_Elective, 
         admission_type_Emergency = admission_type_id_c_Emergency,
         admission_type_Urgent = admission_type_id_c_Urgent,
         discharge_ToHome = discharge_disposition_id_c_ToHome,
         discharge_Transferred = discharge_disposition_id_c_Transferred,
         admission_source_ER = admission_source_id_c_ER,
         admission_source_Referral = admission_source_id_c_Referral,
         admission_source_Transfer = admission_source_id_c_Transfer)

# Changing Y to 1/0
treated_train$readmitted_y <- ifelse(treated_train$readmitted_y == "TRUE",1,0)
treated_test$readmitted_y <- ifelse(treated_test$readmitted_y == "TRUE",1,0)

head(treated_train)
str(treated_train)
head(treated_test)
str(treated_test)

############### MODELING - EVALUATION FUNCTIONS ###############

# See results
eval_results <- function(pred_probs,actual_outcomes,cutoff=0.5) {
  # Apply cutoff to predicted probabilities
  pred_labels <- ifelse(pred_probs >= cutoff, 1, 0)
  # Create dataframe with results
  results <- data.frame(actual = actual_outcomes,
                        predicted = pred_labels,
                        probs = round(pred_probs, 4))
  # Return results
  return(results)
}
# See metrics - cm, acc, auc
eval_metrics <- function(pred_probs, actual_outcomes, cutoff = 0.5) {
  # Apply cutoff to predicted probabilities
  pred_labels <- ifelse(pred_probs >= cutoff, 1, 0)
  # Compute evaluation metrics
  acc <- MLmetrics::Accuracy(pred_labels, actual_outcomes)
  auc <- MLmetrics::AUC(pred_labels, actual_outcomes)
  f1 <- MLmetrics::F1_Score(actual_outcomes, pred_labels)
  mae <- MLmetrics::MAE(pred_labels, actual_outcomes)
  rmse <- MLmetrics::RMSE(pred_labels, actual_outcomes)
  # Create dataframe of evaluation metrics
  metrics_df <- data.frame(
    Metric = c("Accuracy", "AUC", "F1-score", "MAE", "RMSE"),
    Value = c(round(acc, 4), round(auc, 4), round(f1, 4), round(mae, 4), round(rmse, 4))
  )
  return(metrics_df)
}

# See density plot
eval_plotdense <- function(results,cutoff=0.5) {
  # Create density plot
  plotdense <- ggplot(results, aes(x = probs, color = as.factor(actual))) +
    geom_density() + 
    geom_vline(aes(xintercept = cutoff), color = 'darkgreen') +
    theme_minimal()
  return(plotdense)
}
# See roc plot
eval_plotroc <- function(pred_probs, actual_outcomes,cutoff=0.5) {
  # Apply cutoff to predicted probabilities
  pred_labels <- ifelse(pred_probs >= cutoff, 1, 0)
  # Compute ROC
  roc_curve <- roc(pred_labels, as.logical(actual_outcomes)*1)
  # Create ROC plot
  plotroc <- plot(roc_curve,
                  main = "ROC Curve",
                  col="darkslategray",
                  identity=T,
                  identity.col="darkslategray",
                  print.thres=T,
                  print.thres.col="darkslategray",
                  print.auc = T,
                  print.auc.col="darkslategray",
                  auc.polygon = T,
                  auc.polygon.col="seashell2",
                  max.auc.polygon = T,
                  max.auc.polygon.col="white")
  # Return results
  return(plotroc)
}

# Plot CM
eval_plotcm <- function(pred_probs, actual_outcomes,cutoff=0.5) {
  # Apply cutoff to predicted probabilities
  pred_labels <- ifelse(pred_probs >= cutoff, 1, 0)
  n <- length(pred_labels)
  # Calculate initial metrics
  acc_calc <- MLmetrics::Accuracy(pred_labels, actual_outcomes)
  auc <- MLmetrics::AUC(pred_labels, actual_outcomes)
  f1 <- MLmetrics::F1_Score(actual_outcomes, pred_labels)
  mae <- MLmetrics::MAE(pred_labels, actual_outcomes)
  rmse <- MLmetrics::RMSE(pred_labels, actual_outcomes)
  # Generate the confusion matrix
  pred_labels <- factor(pred_labels)
  actual_outcomes <- factor(actual_outcomes)
  tmp_cm <- MLmetrics::ConfusionMatrix(pred_labels, actual_outcomes)
  tmp_cmdf <- data.frame(tmp_cm) %>% arrange(desc(y_true)) %>% arrange(desc(y_pred))
  # Extract the confusion matrix components
  TP <- tmp_cmdf$Freq[1]
  FP <- tmp_cmdf$Freq[2]
  FN <- tmp_cmdf$Freq[3]
  TN <- tmp_cmdf$Freq[4]
  # Calculate additional metrics
  acc_cm <- round((TP + TN) / n, 4)
  misclass <- round((FP + FN) / n, 4)
  # Create a data frame with the confusion matrix components and labels
  tmp_cmdf <- data.frame(Label = c("True Positive", "False Positive", "False Negative", "True Negative"),
                         Value = c(TP, FP, FN, TN)
  )
  # Define the data for the confusion matrix
  tmp_plotcm <- matrix(c(TN, FN, FP, TP), 
                       nrow = 2, 
                       dimnames = list(Actual = c("Actual Negative", "Actual Positive"), 
                                       Predicted = c("Predicted Negative", "Predicted Positive")))
  # Convert the data to a data frame for plotting
  tmp_plotcmdf <- data.frame(expand.grid(Actual = rownames(tmp_plotcm), 
                                         Predicted = colnames(tmp_plotcm)),
                             Value = as.vector(tmp_plotcm))
  # Define the plot theme
  theme_confusion_matrix <- function(base_size = 12) {
    theme(
      text = element_text(size = base_size),
      axis.line = element_blank(),
      axis.text.x = element_text(hjust = 0),
      axis.text.y = element_text(vjust = 0.5),
      axis.title = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(1, 4), "cm")
    )
  }
  # Plot the confusion matrix
  plotcm <- ggplot(tmp_plotcmdf, aes(x = Predicted, y = Actual, fill = Value)) +
    geom_tile() +
    scale_fill_gradient(low = "lightgray", high = "darkslategray") +
    geom_text(aes(label = Value), color = "black") +
    labs(title = "Confusion Matrix", x = "Predicted", y = "Actual", fill = "") +
    theme_confusion_matrix()
  # Plot with metrics
  final_plotcm <- grid.arrange(plotcm,
                               bottom = textGrob(paste0("Accuracy CM: ", round(acc_cm,4), "   Accuracy Calc: ", round(acc_calc,4),
                                                        "\nAUC: ", round(auc,4),"   F1: ", round(f1,4),
                                                        "\nMisclass Rate: ", round(misclass,4),"   MAE: ", round(mae,4), "   RMSE: ", round(rmse,4)), 
                                                 gp = gpar(fontsize = 10), 
                                                 hjust = 1, 
                                                 vjust = -.025,
                                                 x = unit(1, "npc") - unit(2, "cm"), 
                                                 y = unit(0.5, "npc")))
  return(final_plotcm)
}

############### MODELING - LOGISTIC REGRESSION ###############

# GLM 1 - DEFAULT
# Fit Model
glm_model <- glm(readmitted_y ~ ., data = treated_train, family = "binomial")
summary(glm_model)
# Predictions
glm_pred <- predict(glm_model, treated_test, type="response")
# Evaluation
glm_results <- eval_results(glm_pred,treated_test$readmitted_y)
head(glm_results)
glm1_results <- glm_results
glm_metrics <- eval_metrics(glm1_results$probs,glm1_results$actual) 
# acc 0.6580, auc 0.6003, f1 0.7545, mae 0.3420, rmse 0.5848
glm1_metrics <- glm_metrics # Save results
glm_density <- eval_plotdense(glm_results)
glm_roc <- eval_plotroc(glm1_results$probs,glm1_results$actual) # 0.640
glm1_roc <- glm_roc # Save roc
glm_cm <- eval_plotcm(glm1_results$probs,glm1_results$actual)
glm1_cm <- eval_plotcm(glm1_results$probs,glm1_results$actual) # Save cm
# Most important
glm_coeffs <- coef(glm_model)
glm_coeffs_df <- as.data.frame(glm_coeffs)
glm_coeffs_df <- glm_coeffs_df %>% arrange(desc(abs(glm_coeffs_df[,1])))
tmp_top10 <- data.frame(name = rownames(head(glm_coeffs_df,11)),
                        values = head(glm_coeffs_df,11))
tmp_top10 <- tmp_top10[tmp_top10$name != "(Intercept)",]
tmp_top10 <- data.frame(tmp_top10) 
tmp_top10 <- tmp_top10[order(-abs(tmp_top10$glm_coeffs)),]
glm1_top10feat <- tmp_top10 # Save top 10
# Plot most important
ggplot(glm1_top10feat, aes(x=name, y=round(glm_coeffs,4))) +
  geom_bar(stat="identity", fill="darkslategrey") +
  coord_flip() +
  labs(title="Top 10 Features (GLM)", x="Feature", y="Coefficient") +
  geom_text(aes(label=round(glm_coeffs,4)), vjust=.5, hjust = .5, color="black", size=4) +
  theme_minimal()

# GLM 2 - BALANCED
pl_bar_freq(treated_train, "readmitted_y", "Dependent Var" ) # 4508 F, 2992 T
treated_train_bal <- ROSE(readmitted_y ~ ., data = treated_train, seed = 123)$data
pl_bar_freq(treated_train_bal, "readmitted_y", "Dependent Var" ) # 3792 0, 3708 1
# Fit Model
glm_model <- glm(readmitted_y ~ ., data = treated_train_bal, family = "binomial")
summary(glm_model)
# Predictions
glm_pred <- predict(glm_model, treated_test, type="response")
# Evaluation
glm_results <- eval_results(glm_pred,treated_test$readmitted_y)
head(glm_results)
glm2_results <- glm_results # Save
glm_metrics <- eval_metrics(glm2_results$predicted,glm2_results$actual) 
# acc 0.6580, auc 0.6003, f1 0.7545, mae 0.3420, rmse 0.5848
# acc 0.6212, auc 0.6114, f1 0.6789, mae 0.3788, rmse 0.6155 - balanced - higher error, lower acc and f1
glm2_metrics <- glm_metrics # Save
glm_density <- eval_plotdense(glm2_results)
glm_roc <- eval_plotroc(glm2_results$predicted,glm2_results$actual)
glm2_roc <- glm_roc # Save
glm_cm <- eval_plotcm(glm2_results$predicted,glm2_results$actual) # Better in TN, so bad in TP
glm2_cm <- eval_plotcm(glm2_results$predicted,glm2_results$actual) # Save
# Most important
glm_coeffs <- coef(glm_model)
glm_coeffs_df <- as.data.frame(glm_coeffs)
glm_coeffs_df <- glm_coeffs_df %>% arrange(desc(abs(glm_coeffs_df[,1])))
tmp_top10 <- data.frame(name = rownames(head(glm_coeffs_df,11)),
                        values = head(glm_coeffs_df,11))
tmp_top10 <- tmp_top10[tmp_top10$name != "(Intercept)",]
tmp_top10 <- data.frame(tmp_top10)
glm2_top10feat <- tmp_top10 # save
ggplot(glm2_top10feat, aes(x=name, y=round(glm_coeffs,4))) +
  geom_bar(stat="identity", fill="darkslategrey") +
  coord_flip() +
  labs(title="Top 10 Features (GLM)", x="Feature", y="Coefficient") +
  geom_text(aes(label=round(glm_coeffs,4)), vjust=.5, hjust = .5, color="darkgray", size=4) +
  theme_minimal() 
# rosiglitazone_Up more likely, rosiglitazone_Down less likely
# other medications - pioglitazone, metformine, glyburide, glimepride

# best is default not balanced

############### MODELING - DECISION TREE ###############

# DT 1 - DEFAULT
# Create a decision tree using the rpart function
dt_model <- rpart(factor(readmitted_y) ~ ., 
                  data = treated_train, 
                  method = "class")
# Make predictions on the test dataset
dt_pred <- predict(dt_model, treated_test, type = "prob")
head(dt_pred)
# Evaluations
dt_results <- eval_results(dt_pred,treated_test$readmitted_y)
head(dt_results)
dt1_results <- dt_results # Save
dt_metrics <- eval_metrics(dt1_results$probs.1,dt1_results$actual)
# acc 0.6404, auc 0.5623, f1 0.7566, mae 0.3596, rmse 0.5997
dt1_metrics <- dt_metrics # Save
dt_plotroc <- eval_plotroc(dt1_results$probs.1,dt1_results$actual) # 0.628
dt1_roc <- dt_plotroc
dt_plotcm <- eval_plotcm(dt1_results$probs.1,dt1_results$actual) # bad in TP
dt1_cm <- eval_plotcm(dt1_results$probs.1,dt1_results$actual) # Save
# Calculate the feature importance using the Gini importance measure
dt_imp <- varImp(dt_model, type = 2)
head(dt_imp)
dt_imp <- data.frame(feature = rownames(dt_imp),
                     values = dt_imp)
dt_imp <- dt_imp[order(dt_imp[,2], decreasing = TRUE),]
tmp_top10 <- data.frame(name = rownames(head(dt_imp, 10)),
                        values = head(dt_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
dt1_top10feat <- tmp_top10
ggplot(dt1_top10feat, aes(x=round(values,4), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (IncNodePurity)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 

# DT 2 - BALANCED
# Balancing using ROSE
pl_bar_freq(treated_train, "readmitted_y", "Dependent Var" ) # 4508 F, 2992 T
treated_train_bal <- ROSE(readmitted_y ~ ., data = treated_train, seed = 123)$data
pl_bar_freq(treated_train_bal, "readmitted_y", "Dependent Var" ) # 3792 0, 3708 1
# Create a decision tree using the rpart function
dt_model <- rpart(factor(readmitted_y) ~ ., 
                  data = treated_train_bal, 
                  method = "class")
# Make predictions on the test dataset
dt_pred <- predict(dt_model, treated_test, type = "prob")
head(dt_pred)
# Evaluations
dt_results <- eval_results(dt_pred,treated_test$readmitted_y)
head(dt_results)
dt2_results <- dt_results # Save
dt_metrics <- eval_metrics(dt2_results$probs.1,dt2_results$actual)
# acc 0.6288, auc 0.5522, f1 0.7471, mae 0.3712, rmse 0.6093 - worse
dt2_metrics <- dt_metrics # Save
dt_plotroc <- eval_plotroc(dt2_results$probs.1,dt2_results$actual) # 0.601
dt2_roc <- dt_plotroc
dt_plotcm <- eval_plotcm(dt2_results$probs.1,dt2_results$actual) # bad in TP
dt2_cm <- eval_plotcm(dt2_results$probs.1,dt2_results$actual) # Save
# Calculate the feature importance using the Gini importance measure
dt_imp <- varImp(dt_model, type = 2)
head(dt_imp)
dt_imp <- data.frame(feature = rownames(dt_imp),
                     values = dt_imp)
dt_imp <- dt_imp[order(dt_imp[,2], decreasing = TRUE),]
tmp_top10 <- data.frame(name = rownames(head(dt_imp, 10)),
                        values = head(dt_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
dt2_top10feat <- tmp_top10
ggplot(dt2_top10feat, aes(x=round(values,4), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (IncNodePurity)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 

# DT 3 - GRID TUNING
# grid set up
dt_grid <- expand.grid(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1))
dt_control <- trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE)
dt_model <- train(as.factor(readmitted_y) ~ .,
                  data = treated_train, 
                  method = "rpart",
                  trControl = dt_control, 
                  tuneGrid = dt_grid,
                  control = rpart.control(minsplit = 4, 
                                          minbucket = 4,
                                          maxdepth = 4)
                  )
dt_model$finalModel
# Make predictions on the test dataset
dt_pred <- predict(dt_model, treated_test, type = "prob")
head(dt_pred)
# Evaluations
dt_results <- eval_results(dt_pred,treated_test$readmitted_y)
head(dt_results)
dt3_results <- dt_results # Save
dt_metrics <- eval_metrics(dt3_results$probs.1,dt3_results$actual)
dt3_metrics <- dt_metrics # Save
dt_plotroc <- eval_plotroc(dt3_results$probs.1,dt3_results$actual) # 0.628
dt3_roc <- dt_plotroc
dt_plotcm <- eval_plotcm(dt3_results$probs.1,dt3_results$actual) # bad in TP
dt3_cm <- eval_plotcm(dt3_results$probs.1,dt3_results$actual) # Save
# Calculate the feature importance using the Gini importance measure
dt_imp <- varImp(dt_model, type = 2)
head(dt_imp)
dt_imp <- data.frame(feature = rownames(dt_imp$importance),
                     values = dt_imp$importance)
dt_imp <- dt_imp[order(dt_imp[,2], decreasing = TRUE),]
tmp_top10 <- data.frame(name = rownames(head(dt_imp, 10)),
                        values = head(dt_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
dt3_top10feat <- tmp_top10
ggplot(dt3_top10feat, aes(x=round(values,4), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (IncNodePurity)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 
# makes sense

# DT 4 - GRID TUNING + BALANCED
# Balancing using ROSE
pl_bar_freq(treated_train, "readmitted_y", "Dependent Var" ) # 4508 F, 2992 T
treated_train_bal <- ROSE(readmitted_y ~ ., data = treated_train, seed = 123)$data
pl_bar_freq(treated_train_bal, "readmitted_y", "Dependent Var" ) # 3792 0, 3708 1
# grid tuning
dt_grid <- expand.grid(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1))
dt_control <- trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE)
dt_model <- train(as.factor(readmitted_y) ~ .,
                  data = treated_train_bal, 
                  method = "rpart",
                  trControl = dt_control, 
                  tuneGrid = dt_grid,
                  control = rpart.control(minsplit = 3, 
                                          minbucket = 3,
                                          maxdepth = 3)
)
dt_model$finalModel
# Make predictions on the test dataset
dt_pred <- predict(dt_model, treated_test, type = "prob")
head(dt_pred)
# Evaluations
dt_results <- eval_results(dt_pred,treated_test$readmitted_y)
head(dt_results)
dt4_results <- dt_results # Save
dt_metrics <- eval_metrics(dt4_results$probs.1,dt4_results$actual)
# acc 0.6288, auc 0.5522, f1 0.7471, mae 0.3712, rmse 0.6093 - so much worse, plateaus after 3
dt4_metrics <- dt_metrics # Save
dt_plotroc <- eval_plotroc(dt4_results$probs.1,dt4_results$actual) # 0.628 > 0.601
dt4_roc <- dt_plotroc
dt_plotcm <- eval_plotcm(dt4_results$probs.1,dt4_results$actual) # bad in TP
dt4_cm <- eval_plotcm(dt4_results$probs.1,dt4_results$actual) # Save
# Calculate the feature importance using the Gini importance measure
dt_imp <- varImp(dt_model, type = 2)
head(dt_imp)
dt_imp <- data.frame(feature = rownames(dt_imp$importance),
                     values = dt_imp$importance)
dt_imp <- dt_imp[order(dt_imp[,2], decreasing = TRUE),]
tmp_top10 <- data.frame(name = rownames(head(dt_imp, 10)),
                        values = head(dt_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
dt4_top10feat <- tmp_top10
ggplot(dt4_top10feat, aes(x=round(values,4), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (IncNodePurity)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 
# makes sense??

# Best decision tree is tuned, not balanced

############### MODELING - RANDOM FOREST ###############

# RF 1 - DEFAULT
# Separation of data
X_train <- treated_train[, -ncol(treated_train)]
y_train <- treated_train$readmitted_y
X_test <- treated_test[, -ncol(treated_test)]
y_test <- treated_test$readmitted_y
# Modeling
rf_model <- randomForest(x = X_train, y = factor(y_train), ntree = 200, importance = TRUE)
# Predict
rf_pred <- predict(rf_model, X_test,type="prob")
# Evaluations
rf_results <- eval_results(rf_pred,y_test)
str(rf_results)
rf1_results <- rf_results # Save
rf_metrics <- eval_metrics(rf1_results$probs.1,rf1_results$actual)
# acc 0.6508, auc 0.5965, f1 0.7464, mae 0.3492, rmse 0.5909
rf1_metrics <- rf_metrics
rf_plotroc <- eval_plotroc(rf1_results$probs.1,rf1_results$actual) # 0.628
rf1_roc <- rf_plotroc
rf_plotcm <- eval_plotcm(rf1_results$probs.1,rf1_results$actual)
rf1_cm <- eval_plotcm(rf1_results$probs.1,rf1_results$actual)
# Most important
rf_imp <- randomForest::importance(rf_model)
rf_imp <- rf_imp[order(rf_imp[, 2], decreasing = TRUE), ]
# Using IncNodePurity
tmp_top10 <- data.frame(name = rownames(head(rf_imp, 10)),
                        values = head(rf_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
rf1_top10feat <- tmp_top10
ggplot(rf1_top10feat, aes(x=round(values,4), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (IncNodePurity)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 

# RF 2 - BALANCED
# Balancing using ROSE
pl_bar_freq(treated_train, "readmitted_y", "Dependent Var" ) # 4508 F, 2992 T
treated_train_bal <- ROSE(readmitted_y ~ ., data = treated_train, seed = 123)$data
pl_bar_freq(treated_train_bal, "readmitted_y", "Dependent Var" ) # 3792 0, 3708 1
# Separation of data
X_train <- treated_train_bal[, -ncol(treated_train_bal)]
y_train <- treated_train_bal$readmitted_y
X_test <- treated_test[, -ncol(treated_test)]
y_test <- treated_test$readmitted_y
# Modeling
rf_model <- randomForest(x = X_train, y = factor(y_train), ntree = 200, importance = TRUE)
# Predict
rf_pred <- predict(rf_model, X_test,type="prob")
# Evaluations
rf_results <- eval_results(rf_pred,y_test)
head(rf_results)
rf2_results <- rf_results
rf_metrics <- eval_metrics(rf2_results$probs.1,rf2_results$actual) 
rf2_metrics <- rf_metrics
# acc 0.6272, auc 0.5438, f1 0.7509, mae 0.3728, rmse 0.6106
rf_plotroc <- eval_plotroc(rf2_results$probs.1,rf2_results$actual)
rf2_roc <- rf_plotroc
rf_plotcm <- eval_plotcm(rf2_results$probs.1,rf2_results$actual) # terrible
rf2_cm <- eval_plotcm(rf2_results$probs.1,rf2_results$actual)
# Most important
rf_imp <- randomForest::importance(rf_model)
rf_imp <- rf_imp[order(rf_imp[, 2], decreasing = TRUE), ]
# Using IncNodePurity
tmp_top10 <- data.frame(name = rownames(head(rf_imp, 10)),
                        values = head(rf_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
rf2_top10feat <- tmp_top10
ggplot(rf2_top10feat, aes(x=round(values,4), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (IncNodePurity)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 

# RF 3 - GRID TUNING, NOT BALANCED
# Grid tuning
# mtry, splitrule, min.node.size
rf_grid <- expand.grid(mtry = c(6,7,8,9,10),
                       splitrule = c("extratrees"),
                       min.node.size = c(30,40,50,60,70)
)
# CV
rf_control <- trainControl(method = "CV",
                           number = 5,
                           verboseIter = TRUE)
# ntree, oob
rf_ntree <- vector()
rf_oob  <- vector()
rf_ntreesearch <- seq(from = 100, to = 1000, by=100)
for(i in 1:length(rf_ntreesearch)){
  print(i)
  rf_fit <- train(as.factor(readmitted_y) ~ ., 
                  data = treated_train,
                  method = 'ranger',
                  num.trees = rf_ntreesearch[i], 
                  tuneGrid = rf_grid,
                  trControl = rf_control)
  rf_ntree[i] <- rf_fit$finalModel$num.trees
  rf_oob[i] <- rf_fit$finalModel$prediction.error
}
# initial results
rf_fitresults <- data.frame(ntrees = rf_ntree,
                            oobError = rf_oob)
head(rf_fitresults)
ggplot(rf_fitresults, aes(x=ntrees,y=oobError)) + 
  geom_line(color = 'red') +
  geom_smooth(method = "loess")
# optimal looks to be around 500 - changes in iterations?
# grid search for ntree = 300
rf_fit <- train(as.factor(readmitted_y) ~ ., 
                data = treated_train,
                method = 'ranger',
                num.trees = 500,
                tuneGrid = rf_grid,
                trControl = rf_control)
rf_fit$finalModel
# ntree = 500, mtry = 7, node = 50
# Modeling - using tuned
rf_model <- ranger(as.factor(readmitted_y) ~ .,
                   data  = treated_train, 
                   num.trees = 500,
                   importance = 'impurity', #corresponds to gini
                   mtry  = 7,
                   splitrule = 'extratrees',
                   min.node.size = 50,
                   probability = T)
# Predict
rf_pred <- predict(rf_model, treated_test)
head(rf_pred$predictions)
# Evaluations
rf_results <- eval_results(rf_pred$predictions[,2],treated_test$readmitted_y)
head(rf_results)
rf3_results <- rf_results
rf_metrics <- eval_metrics(rf3_results$probs,rf3_results$actual) 
rf3_metrics <- rf_metrics
# grid search tuning - not balanced
# acc 0.6616, auc 0.5917, f1 0.7660, mae 0.3384, rmse 0.5817
rf_plotroc <- eval_plotroc(rf3_results$probs,rf3_results$actual) # auc 0.659
rf3_roc <- rf_plotroc
rf_plotcm <- eval_plotcm(rf3_results$probs,rf3_results$actual) # true positives are bad
rf3_cm <- eval_plotcm(rf3_results$probs,rf3_results$actual)
# Most important
rf_imp <- importance(rf_model)
rf_imp <- data.frame(feature = names(rf_imp),
                     value = round(rf_imp,6))
rf_imp <- rf_imp[order(rf_imp[,2], decreasing = T),]
head(rf_imp)
tmp_top10 <- data.frame(name = rownames(head(rf_imp, 10)),
                        values = head(rf_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
rf3_top10feat <- tmp_top10
ggplot(rf3_top10feat, aes(x=round(values,6), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (Gini)") +
  geom_text(aes(label=round(values,4)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 
# important features makes sense

# RF 4 - CARET + RANGER TUNING, BALANCED
# Balancing using ROSE
pl_bar_freq(treated_train, "readmitted_y", "Dependent Var" ) # 4508 F, 2992 T
treated_train_bal <- ROSE(readmitted_y ~ ., data = treated_train, seed = 123)$data
pl_bar_freq(treated_train_bal, "readmitted_y", "Dependent Var" ) # 3792 0, 3708 1
# Grid tuning
rf_grid <- expand.grid(mtry = c(6,7,8,9,10),
                       splitrule = c("extratrees"),
                       min.node.size = c(30,40,50,60,70)
)
# CV
rf_control <- trainControl(method = "CV",
                           number = 5,
                           verboseIter = TRUE)
# ntree, oob
rf_ntree <- vector()
rf_oob  <- vector()
rf_ntreesearch <- seq(from = 100, to = 1000, by=100)
for(i in 1:length(rf_ntreesearch)){
  print(i)
  rf_fit <- train(as.factor(readmitted_y) ~ ., 
                  data = treated_train,
                  method = 'ranger',
                  num.trees = rf_ntreesearch[i], 
                  tuneGrid = rf_grid,
                  trControl = rf_control)
  rf_ntree[i] <- rf_fit$finalModel$num.trees
  rf_oob[i] <- rf_fit$finalModel$prediction.error
}
# initial results
rf_fitresults <- data.frame(ntrees = rf_ntree,
                            oobError = rf_oob)
head(rf_fitresults)
ggplot(rf_fitresults, aes(x=ntrees,y=oobError)) + 
  geom_line(color = 'red') +
  geom_smooth(method = "loess")
# optimal looks to be around 500 - changes in iterations?
# grid search for ntree = 300
rf_fit <- train(as.factor(readmitted_y) ~ ., 
                data = treated_train,
                method = 'ranger',
                num.trees = 500,
                tuneGrid = rf_grid,
                trControl = rf_control)
rf_fit$finalModel
# ntree = 500, mtry = 7, node = 50
# Modeling - using tuned
rf_model <- ranger(as.factor(readmitted_y) ~ .,
                   data  = treated_train_bal, 
                   num.trees = 500,
                   importance = 'impurity', #corresponds to gini
                   mtry  = 7,
                   splitrule = 'gini',
                   min.node.size = 50,
                   probability = T)
# Predict
rf_pred <- predict(rf_model, treated_test)
head(rf_pred$predictions)
# Evaluations
rf_results <- eval_results(rf_pred$predictions[,2],treated_test$readmitted_y)
head(rf_results)
rf4_results <- rf_results
rf_metrics <- eval_metrics(rf4_results$probs,rf4_results$actual) 
rf4_metrics <- rf_metrics
# grid search tuning, balanced
# acc 0.6288, auc 0.5459, f1 0.7517, mae 0.3712, rmse 0.6093 - worse
rf_plotroc <- eval_plotroc(rf4_results$probs,rf4_results$actual) # auc 0.607
rf4_roc <- rf_plotroc
rf_plotcm <- eval_plotcm(rf4_results$probs,rf4_results$actual) # true positives are still so bad
rf4_cm <- eval_plotcm(rf4_results$probs,rf4_results$actual)
# Most important
rf_imp <- importance(rf_model)
rf_imp <- data.frame(feature = names(rf_imp),
                     value = round(rf_imp,6))
rf_imp <- rf_imp[order(rf_imp[,2], decreasing = T),]
head(rf_imp)
tmp_top10 <- data.frame(name = rownames(head(rf_imp, 10)),
                        values = head(rf_imp[,2], 10))
tmp_top10$name <- fct_reorder(tmp_top10$name, tmp_top10$values)
rf4_top10feat <- tmp_top10
ggplot(rf4_top10feat, aes(x=round(values,6), y=name)) +
  geom_bar(stat="identity", fill="darkslategrey") +
  labs(title="Top 10 Features (RF)", x="Feature", y="Importance (Gini)") +
  geom_text(aes(label=round(values,2)), vjust=.5, hjust = 1.25, color="white", size=3.5) +
  theme_minimal() 
# important features makes sense

glm1_metrics # BEST
glm2_metrics
dt1_metrics
dt2_metrics
dt3_metrics # BEST
dt4_metrics
rf1_metrics
rf2_metrics
rf3_metrics # BEST
rf4_metrics

############### ANALYSIS AND VISUALIZATIONS ###############

# Top 100
head(rf3_results)
top100 <- data.frame(tmpID = r_test$tmpID,
                     actual = treated_test$readmitted_y,
                     probability = rf3_results$probs)
top100$predicted <- ifelse(top100$probability >= 0.5,1,0)
top100$correct <- ifelse(top100$predicted==top100$actual,1,0)
top100 <- top100[order(top100$probability,decreasing = T),]
top100 <- head(top100,100)
sum(top100$correct)/length(top100$predicted) #73% for top100
# TO CSV
#write.csv(top100, "A2 - Top 100 Patients.csv", row.names = FALSE)

# Random Forest Feature Importance
ggplot(rf3_top10feat, aes(x = round(values, 6), y = name)) +
  geom_bar(stat = "identity", fill = "darkslategrey") +
  labs(title = "", x = "", y = "") +
  geom_text(aes(label = round(values, 4)), vjust = .5, hjust = 1.25, color = "white", size = 3.5) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10))
rf3_top10feat

# Merge Train and Test
merged_final <- rbind(final_train,final_test)
merged_treated <- rbind(treated_train,treated_test)
results_final <- cbind(final_test,rf3_results)
results_treated <- cbind(treated_test,rf3_results)
str(results_treated)

# Number_X vs Readmission
# merged - inpatient and diagnoses
tmp_freq <- merged_final[, c("number_inpatient", "number_diagnoses", "readmitted_y")]
tmp_freq <- tmp_freq %>%
  group_by(number_inpatient, number_diagnoses, readmitted_y) %>%
  summarise(freq = n())
ggplot(tmp_freq, aes(x = number_inpatient, y = number_diagnoses, color=as.factor(readmitted_y), size = freq)) +
  geom_point(alpha=0.9, stroke=0) +
  labs(title = "", x = "Inpatient", y = "Diagnoses", size = "Frequency", color="Actual Readmission") +
  scale_color_manual(values = c("darkgray", "darkslategray")) +
  scale_size(range = c(4, 15)) +
  theme_minimal()
# merged - medication and diagnoses
tmp_freq2 <- merged_final[, c("number_diagnoses", "num_medications", "readmitted_y")]
tmp_freq2 <- tmp_freq2 %>%
  group_by(number_diagnoses, num_medications, readmitted_y) %>%
  summarise(freq = n())
ggplot(tmp_freq2, aes(x = number_diagnoses, y = num_medications, color=as.factor(readmitted_y), size = freq)) +
  geom_point(alpha=0.9, stroke=0) +
  labs(title = "", x = "Diagnoses", y = "Medications", size = "Frequency", color="Actual Readmissions") +
  scale_color_manual(values = c("gray", "darkslategray")) +
  scale_size(range = c(4, 15)) +
  theme_minimal()
# merged - lab and diagnoses
tmp_freq2 <- merged_final[, c("num_lab_procedures", "number_diagnoses", "readmitted_y")]
tmp_freq2 <- tmp_freq2 %>%
  group_by(num_lab_procedures, number_diagnoses, readmitted_y) %>%
  summarise(freq = n())
ggplot(tmp_freq2, aes(x = number_diagnoses, y = num_lab_procedures, color=as.factor(readmitted_y), size = freq)) +
  geom_point(alpha=0.9, stroke=0) +
  labs(title = "", x = "Diagnoses", y = "Lab Procedures", size = "Frequency", color="Actual Readmissions") +
  scale_color_manual(values = c("gray", "darkslategray")) +
  scale_size(range = c(4, 15)) +
  theme_minimal()
# merged - lab and inpatient
tmp_freq2 <- merged_final[, c("num_lab_procedures", "number_inpatient", "readmitted_y")]
tmp_freq2 <- tmp_freq2 %>%
  group_by(num_lab_procedures, number_inpatient, readmitted_y) %>%
  summarise(freq = n())
ggplot(tmp_freq2, aes(x = number_inpatient, y = num_lab_procedures, color=as.factor(readmitted_y), size = freq)) +
  geom_point(alpha=0.9, stroke=0) +
  labs(title = "", x = "Inpatient", y = "Lab Procedures", size = "Frequency", color="Actual Readmissions") +
  scale_color_manual(values = c("gray", "darkslategray")) +
  scale_size(range = c(4, 15)) +
  theme_minimal()
# merged - meds and inpatient
tmp_freq2 <- merged_final[, c("num_medications", "number_inpatient", "readmitted_y")]
tmp_freq2 <- tmp_freq2 %>%
  group_by(num_medications, number_inpatient, readmitted_y) %>%
  summarise(freq = n())
ggplot(tmp_freq2, aes(x = number_inpatient, y = num_medications, color=as.factor(readmitted_y), size = freq)) +
  geom_point(alpha=0.9, stroke=0) +
  labs(title = "", x = "Inpatient", y = "Medications", size = "Frequency", color="Actual Readmissions") +
  scale_color_manual(values = c("gray", "darkslategray")) +
  scale_size(range = c(4, 15)) +
  theme_minimal()

# probabilities - inpatient
tmp_prob1 <- results_final[,c("number_inpatient","probs","actual")]
tmp_prob1$color <- ifelse(tmp_prob1$actual == 1, "darkslategray", "gray")
ggplot(tmp_prob1, aes(x = number_inpatient, y = probs, color = color)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red4") +
  scale_color_identity(guide = "legend", labels = c("Yes", "No")) +
  labs(title = "", x = "Number of Inpatient", y = "Probability of Readmission",color="Actual Readmission") +
  theme_minimal() +
  theme(legend.position = "top")
# probabilities - diagnoses
tmp_prob2 <- results_final[,c("number_diagnoses","probs","actual")]
tmp_prob2$color <- ifelse(tmp_prob2$actual == 1, "darkslategray", "gray")
ggplot(tmp_prob2, aes(x = number_diagnoses, y = probs, color = color)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red4") +
  scale_color_identity(guide = "legend", labels = c("Yes", "No")) +
  labs(title = "", x = "Number of Diagnoses", y = "Probability of Readmission",color="Actual Readmission") +
  theme_minimal() +
  theme(legend.position = "top")
# probabilities - lab procedures
tmp_prob3 <- results_final[,c("num_lab_procedures","probs","actual")]
tmp_prob3$color <- ifelse(tmp_prob3$actual == 1, "darkslategray", "gray")
ggplot(tmp_prob3, aes(x = num_lab_procedures, y = probs, color = color)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red4") +
  scale_color_identity(guide = "legend", labels = c("Yes", "No")) +
  labs(title = "", x = "Number of Lab Procedures", y = "Probability of Readmission", color="Actual Readmission") +
  theme_minimal() +
  theme(legend.position = "top")

# Diag_Chronic
# probabilities - lab procedures
tmp_probdiag <- results_final[,c("diag_chronic","probs","actual")]
tmp_probdiag$color <- ifelse(tmp_probdiag$actual == 1, "darkslategray", "gray")
ggplot(tmp_probdiag, aes(x = diag_chronic, y = probs, color = color)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red4") +
  scale_color_identity(guide = "legend", labels = c("Yes", "No")) +
  labs(title = "", x = "Chronic in Diagnoses", y = "Probability of Readmission", color="Actual Readmission") +
  theme_minimal() +
  theme(legend.position = "top")

# Age
merged_age <- rbind(r_train,r_test)
merged_age <- merged_age[,c("age","readmitted_y")]
tmp_freqage <- data.frame(table(merged_age))
tmp_freqage$age <- as.integer(tmp_freqage$age)
head(tmp_freqage)
ggplot(tmp_freqage, aes(x = age, y = Freq, color=as.factor(readmitted_y))) +
  geom_point(size=2.5) +
  labs(title = "", x = "Age", y = "Freq",color="Actual Readmission") +
  scale_color_manual(values = c("darkgray", "darkslategray")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(tmp_freqage$age), 10)) +
  theme(legend.position = "top")
tmp_probage <- results_final[,c("age","probs","actual")]
tmp_probage$color <- ifelse(tmp_probage$actual == 1, "darkslategray", "gray")
ggplot(tmp_probage, aes(x = age, y = probs, color = color)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red4") +
  scale_color_identity(guide = "legend", labels = c("Yes", "No")) +
  labs(title = "", x = "Age", y = "Probability of Readmission", color="Actual Readmission") +
  theme_minimal() +
  theme(legend.position = "top")

# Time
tmp_time <- data.frame(table(merged_final[,c("time_in_hospital","readmitted_y")]))
tmp_time$time_in_hospital <- as.integer(tmp_time$time_in_hospital)
str(tmp_time$time_in_hospital)
ggplot(tmp_time, aes(x = time_in_hospital, y = Freq, color=as.factor(readmitted_y))) +
  geom_point(size=3) +
  labs(title = "", x = "Time in Hospital", y = "Freq",color="Actual Readmission") +
  scale_color_manual(values = c("darkgray", "darkslategray")) +
  theme_minimal() +
  theme(legend.position = "top")