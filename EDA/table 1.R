library(readr)
library(gtsummary)
library(readxl)
library(gt)
library(tidyverse)

#-------------------------------------------------------load data
blood<-  read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")
urine<-  read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")


## covariates

### blood
blood$ethnicity_specified_recat<- factor(blood$ethnicity_specified_recat,
                                            levels = c("Chinese","Malay","Indian"))

blood$pcv1_highest_education_completed_recat<- factor(blood$pcv1_highest_education_completed_recat,
                                                         levels=c("University","Primary/Secondary/Post_secondary"))

blood$pcv1_household_income_recat<- factor(blood$pcv1_household_income_recat,
                                              levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))

blood$pcv1_parity_recat<- factor(blood$pcv1_parity_recat,
                                    levels=c("0",">= 1"))

blood$age_at_recruitment_cat <- factor(blood$age_at_recruitment_cat,
                                          levels=c("Age first tertile","Age second tertile","Age third tertile"))


blood$Styrofoam_boxes<- factor(blood$Styrofoam_boxes,
                                  levels = c("No", "Yes"))
blood$Plastic_boxes<- factor(blood$Plastic_boxes,
                                levels = c("No", "Yes"))
blood$Canned<- factor(blood$Canned,
                         levels = c("No", "Yes"))
### urine
urine$ethnicity_specified_recat<- factor(urine$ethnicity_specified_recat,
                                         levels = c("Chinese","Indian/Malay"))

urine$pcv1_highest_education_completed_recat<- factor(urine$pcv1_highest_education_completed_recat,
                                                      levels=c("University","Primary/Secondary/Post_secondary"))

urine$pcv1_household_income_recat<- factor(urine$pcv1_household_income_recat,
                                           levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))

urine$pcv1_parity_recat<- factor(urine$pcv1_parity_recat,
                                 levels=c("0",">= 1"))

urine$age_at_recruitment_cat <- factor(urine$age_at_recruitment_cat,
                                       levels=c("Age first tertile","Age second tertile","Age third tertile"))

urine$Styrofoam_boxes<- factor(urine$Styrofoam_boxes,
                               levels = c("No", "Yes"))
urine$Plastic_boxes<- factor(urine$Plastic_boxes,
                             levels = c("No", "Yes"))
urine$Canned<- factor(urine$Canned,
                      levels = c("No", "Yes"))


## table
blood_table <- blood %>%
  select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
         TotalFish, Fast_food, fruit, vegetable, vegetable_boil, vegetable_SF, Styrofoam_boxes, Styrofoam_boxes_freq_mean,Plastic_boxes, Plastic_boxes_freq_mean, Canned, Canned_freq_mean) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical", Canned ~ "categorical",
                           Styrofoam_boxes_freq_mean ~ "continuous", Plastic_boxes_freq_mean ~ "continuous", Canned_freq_mean ~ "continuous"),
              missing_text = "Missing", 
              digits = everything() ~ 2
  ) %>%
  bold_labels()


urine_table <- urine %>%
  select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
         TotalFish, Fast_food, fruit, vegetable, vegetable_boil, vegetable_SF, Styrofoam_boxes, Styrofoam_boxes_freq_mean,Plastic_boxes, Plastic_boxes_freq_mean, Canned, Canned_freq_mean) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical", Canned ~ "categorical",
                           Styrofoam_boxes_freq_mean ~ "continuous", Plastic_boxes_freq_mean ~ "continuous", Canned_freq_mean ~ "continuous"),
              missing_text = "Missing", 
              digits = everything() ~ 2
  ) %>%
  bold_labels()

urine_table

table<- tbl_merge(tbls =list(blood_table, urine_table), 
                  tab_spanner= c("**Blood**", "**Urine**") )

table


















