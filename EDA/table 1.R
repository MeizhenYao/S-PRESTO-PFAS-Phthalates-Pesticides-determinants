library(readr)
library(gtsummary)
library(readxl)
library(gt)
library(tidyverse)

#-------------------------------------------------------load data
blood<-  read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")
urine<-  read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")
FFQ<- read_csv("~/Projects/S-PRESTO/input/for paper/chemical_paper/FFQ.csv")
whole_cohort<-  read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/for paper/whole_cohort_20230316.csv")


## covariates

### whole
blood_nonimpute<- whole_cohort %>% 
               left_join(FFQ, by="Subject_ID")%>%
               filter(participation == 1) %>% 
               mutate(
                 Fast_food=Meatpatties+Nuggets+Opizza+Potatofried,
                 TotalFish=Cannedfish+NonOily_FB+NonOily_FC+NonOily_FDF+NonOily_FR+NonOily_FSF+Oily_FB+Oily_FC+Oily_FDF+Oily_FR+Oily_FSF+Otherseafood,
                 fruit=Citrusfruits+ApplesPears+Grapesberries+Stonefruits+Tropicalfruits,
                 vegetable=Beansprouts_Boil+Beansprouts_SF+Broccoli_Boil+Broccoli_SF+Cabbage_Boil+
                   Cabbage_SF+Carrots_Boil+Carrots_SF+DarkGleaf_Boil+DarkGleaf_SF+Mushrm_Boil+
                   Mushrm_SF+Okra_Boil+Okra_SF+Peas_Boil+Peas_SF+Pumpkin_Boil+Pumpkin_SF+
                   Tomatoes_Boil+Tomatoes_SF,
                 vegetable_boil=Beansprouts_Boil+Broccoli_Boil+Cabbage_Boil+Carrots_Boil+DarkGleaf_Boil+
                   Mushrm_Boil+Okra_Boil+Peas_Boil+Pumpkin_Boil+Tomatoes_Boil,
                 vegetable_SF=Beansprouts_SF+Broccoli_SF+Cabbage_SF+Carrots_SF+DarkGleaf_SF+
                   Mushrm_SF+Okra_SF+Peas_SF+Pumpkin_SF+Tomatoes_SF,
                 occupation=case_when(is.na(broad_occupation_my) == TRUE ~ NA,
                                      broad_occupation_my == "Office worker" ~ "Office worker",
                                      broad_occupation_my == "Health care worker" ~ "Health care worker",
                                      broad_occupation_my %in% c("Service worker", "Outdoor worker") ~ "Service worker & Outdoor worker",
                                      broad_occupation_my == "Not working" ~ "Not working",
                                      .default = NA))

blood_nonimpute$ethnicity_specified_recat<- factor(blood_nonimpute$ethnicity_specified_recat,
                                         levels = c("Chinese","Malay","Indian"))

blood_nonimpute$pcv1_highest_education_completed_recat<- factor(blood_nonimpute$pcv1_highest_education_completed_recat,
                                                      levels=c("University","Primary/Secondary/Post_secondary"))

blood_nonimpute$pcv1_household_income_recat<- factor(blood_nonimpute$pcv1_household_income_recat,
                                           levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))

blood_nonimpute$pcv1_parity_recat<- factor(blood_nonimpute$pcv1_parity_recat,
                                 levels=c("0",">= 1"))

blood_nonimpute$age_at_recruitment_cat <- blood_nonimpute$age_at_recruitment
for(i in 1:nrow(blood_nonimpute)){
  if(is.na(blood_nonimpute$age_at_recruitment[i])==TRUE) {blood_nonimpute$age_at_recruitment_cat[i]<- NA}
  else if(blood_nonimpute$age_at_recruitment[i] < 28.76){blood_nonimpute$age_at_recruitment_cat[i] <- "Age first tertile"}
  else if ((blood_nonimpute$age_at_recruitment[i] >= 28.76) & (blood_nonimpute$age_at_recruitment[i] < 32.1)){blood_nonimpute$age_at_recruitment_cat[i] <- "Age second tertile"}
  else if (blood_nonimpute$age_at_recruitment[i] >= 32.09){blood_nonimpute$age_at_recruitment_cat[i] <- "Age third tertile"}
}

blood_nonimpute$age_at_recruitment_cat <- factor(blood_nonimpute$age_at_recruitment_cat,
                                       levels=c("Age first tertile","Age second tertile","Age third tertile"))


blood_nonimpute$Styrofoam_boxes<- factor(blood_nonimpute$Styrofoam_boxes,
                               levels = c("No", "Yes"))
blood_nonimpute$Plastic_boxes<- factor(blood_nonimpute$Plastic_boxes,
                             levels = c("No", "Yes"))

blood_nonimpute$packaging<- blood_nonimpute$mca_dim2
q<- quantile(blood_nonimpute$mca_dim2, probs = c(2/3), type = 2)
for(i in 1:nrow(blood_nonimpute)){
  if (blood_nonimpute$mca_dim2[i] <= q[1]){blood_nonimpute$packaging[i] <- "Low-tendency packaging"}
  else if (blood_nonimpute$mca_dim2[i] > q[1]){blood_nonimpute$packaging[i] <- "High-tendency packaging"}
}


blood_nonimpute$packaging<- factor(blood_nonimpute$packaging,
                            levels = c("Low-tendency packaging", "High-tendency packaging"))

blood_nonimpute$occupation<- factor(blood_nonimpute$occupation,
                             levels = c("Not working", "Office worker", "Health care worker", "Service worker & Outdoor worker"))


urine_nonimpute<- blood_nonimpute %>%
                 filter(urine_participation == 1)

### blood
# blood$ethnicity_specified_recat<- factor(blood$ethnicity_specified_recat,
#                                             levels = c("Chinese","Malay","Indian"))
# 
# blood$pcv1_highest_education_completed_recat<- factor(blood$pcv1_highest_education_completed_recat,
#                                                          levels=c("University","Primary/Secondary/Post_secondary"))
# 
# blood$pcv1_household_income_recat<- factor(blood$pcv1_household_income_recat,
#                                               levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))
# 
# blood$pcv1_parity_recat<- factor(blood$pcv1_parity_recat,
#                                     levels=c("0",">= 1"))
# 
# blood$age_at_recruitment_cat <- factor(blood$age_at_recruitment_cat,
#                                           levels=c("Age first tertile","Age second tertile","Age third tertile"))
# 
# 
# blood$Styrofoam_boxes<- factor(blood$Styrofoam_boxes,
#                                   levels = c("No", "Yes"))
# blood$Plastic_boxes<- factor(blood$Plastic_boxes,
#                                 levels = c("No", "Yes"))
# blood$Canned<- factor(blood$Canned,
#                          levels = c("No", "Yes"))
# ### urine
# urine$ethnicity_specified_recat<- factor(urine$ethnicity_specified_recat,
#                                          levels = c("Chinese","Indian/Malay"))
# 
# urine$pcv1_highest_education_completed_recat<- factor(urine$pcv1_highest_education_completed_recat,
#                                                       levels=c("University","Primary/Secondary/Post_secondary"))
# 
# urine$pcv1_household_income_recat<- factor(urine$pcv1_household_income_recat,
#                                            levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))
# 
# urine$pcv1_parity_recat<- factor(urine$pcv1_parity_recat,
#                                  levels=c("0",">= 1"))
# 
# urine$age_at_recruitment_cat <- factor(urine$age_at_recruitment_cat,
#                                        levels=c("Age first tertile","Age second tertile","Age third tertile"))
# 
# urine$Styrofoam_boxes<- factor(urine$Styrofoam_boxes,
#                                levels = c("No", "Yes"))
# urine$Plastic_boxes<- factor(urine$Plastic_boxes,
#                              levels = c("No", "Yes"))
# urine$Canned<- factor(urine$Canned,
#                       levels = c("No", "Yes"))


## table - after imputation
# blood_table <- blood %>%
#   select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
#          TotalFish, Fast_food, fruit, vegetable, vegetable_boil, vegetable_SF, Styrofoam_boxes, Styrofoam_boxes_freq_mean,Plastic_boxes, Plastic_boxes_freq_mean, Canned, Canned_freq_mean) %>%
#   tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical", Canned ~ "categorical",
#                            Styrofoam_boxes_freq_mean ~ "continuous", Plastic_boxes_freq_mean ~ "continuous", Canned_freq_mean ~ "continuous"),
#               missing_text = "Missing", 
#               digits = everything() ~ 2
#   ) %>%
#   bold_labels()
# 
# 
# urine_table <- urine %>%
#   select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
#          TotalFish, Fast_food, fruit, vegetable, vegetable_boil, vegetable_SF, Styrofoam_boxes, Styrofoam_boxes_freq_mean,Plastic_boxes, Plastic_boxes_freq_mean, Canned, Canned_freq_mean) %>%
#   tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical", Canned ~ "categorical",
#                            Styrofoam_boxes_freq_mean ~ "continuous", Plastic_boxes_freq_mean ~ "continuous", Canned_freq_mean ~ "continuous"),
#               missing_text = "Missing", 
#               digits = everything() ~ 2
#   ) %>%
#   bold_labels()
# 
# urine_table
# 
# table<- tbl_merge(tbls =list(blood_table, urine_table), 
#                   tab_spanner= c("**Blood**", "**Urine**") )
# 
# table
# 

# TotalFish, Fast_food, fruit, vegetable, vegetable_boil, vegetable_SF

## table - no imputation
blood_table <- blood_nonimpute %>%
  dplyr::select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
         Styrofoam_boxes, Styrofoam_boxes_freq_mean,Plastic_boxes, Plastic_boxes_freq_mean,
         occupation, packaging) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical",
                           Styrofoam_boxes_freq_mean ~ "continuous", Plastic_boxes_freq_mean ~ "continuous"),
              missing_text = "Missing", 
              digits = everything() ~ c(0, 1)
  ) %>%
  bold_labels()



urine_table <- urine_nonimpute %>%
  dplyr::select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
         Styrofoam_boxes, Styrofoam_boxes_freq_mean,Plastic_boxes, Plastic_boxes_freq_mean,
         occupation, packaging) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical",
                           Styrofoam_boxes_freq_mean ~ "continuous", Plastic_boxes_freq_mean ~ "continuous"),
              missing_text = "Missing", 
              digits = everything() ~ c(0, 1)
  ) %>%
  bold_labels()



table<- tbl_merge(tbls =list(blood_table, urine_table), 
                  tab_spanner= c("**Blood**", "**Urine**") )

table















