library(readr)
library(gtsummary)
library(readxl)
library(gt)
library(tidyverse)

#-------------------------------------------------------load data
blood<-  read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")
urine<-  read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")
FFQ<- read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/FFQ.csv")
whole_cohort<-  read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/whole_cohort_20230316.csv")


## covariates

### whole
whole_cohort<- whole_cohort %>% 
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

#-------------------------------------------------------data preparation
# -------------------- FFQ qx
whole_cohort<- whole_cohort %>% 
  mutate(GQ1_Place_BF_adjust = case_when(is.na(GQ1_Place_BF) == TRUE ~ NA,
                                         GQ1_Place_BF == "0 and 1" ~ "0, 1",
                                         GQ1_Place_BF == "0,1" ~ "0, 1",
                                         GQ1_Place_BF == "3,5" ~ "3, 5",
                                         GQ1_Place_BF == "6,5" ~ "5,6",
                                         .default = GQ1_Place_BF))

whole_cohort<- whole_cohort %>% 
  mutate(GQ2_Place_Lunch_adjust = case_when(is.na(GQ2_Place_Lunch) == TRUE ~ NA,
                                            GQ2_Place_Lunch == "0,5" ~ "0, 5",
                                            GQ2_Place_Lunch == "1,3" ~ "1, 3",
                                            GQ2_Place_Lunch == "1,5" ~ "1, 5",
                                            GQ2_Place_Lunch == "1,3,5" ~ "1, 3, 5",
                                            GQ2_Place_Lunch == "2,5" ~ "2, 5",
                                            GQ2_Place_Lunch == "3 and 5" ~ "3,5",
                                            GQ2_Place_Lunch == "8 (fasting mo)nth" ~ "8, fasting month",
                                            .default = GQ2_Place_Lunch))
whole_cohort<- whole_cohort %>% 
  mutate(GQ3_Place_Dinner_adjust = case_when(is.na(GQ3_Place_Dinner) == TRUE ~ NA,
                                             GQ3_Place_Dinner == "0 and 5" ~ "0, 5",
                                             GQ3_Place_Dinner == "0,5" ~ "0, 5",
                                             GQ3_Place_Dinner == "0,2" ~ "0, 2",
                                             GQ3_Place_Dinner == "0,2,5" ~ "0, 2, 5",
                                             GQ3_Place_Dinner == "0,5,2" ~ "0, 2, 5",
                                             GQ3_Place_Dinner == "0,,5,6" ~ "0, 5, 6",
                                             GQ3_Place_Dinner == "0,5,6" ~ "0, 5, 6",
                                             GQ3_Place_Dinner == "2,5," ~ "2,5",
                                             GQ3_Place_Dinner == "2,5,6" ~ "2, 5, 6",
                                             GQ3_Place_Dinner == "2,5,6,0" ~ "0,2,5,6",
                                             GQ3_Place_Dinner == "5 and 2" ~ "2,5",
                                             GQ3_Place_Dinner == "5,6" ~ "5, 6",
                                             .default = GQ3_Place_Dinner))


whole_cohort<- whole_cohort %>% 
  mutate(GQ1_Place_BF_new_v4 = case_when(is.na(GQ1_Place_BF_adjust) == TRUE ~ NA,
                                         GQ1_Place_BF_adjust %in% c("0", "0, 8", "8", "8 (fasting month)") ~ 1,
                                         GQ1_Place_BF_adjust %in% c("2", "3", "3, 5", "4", "5", "7, brunch-outside", "7, hotel", "0,3", "0,5") ~ 2,
                                         .default = 3))


whole_cohort<- whole_cohort %>% 
  mutate(GQ2_Place_Lunch_new_v4 = case_when(is.na(GQ2_Place_Lunch_adjust) == TRUE ~ NA,
                                            GQ2_Place_Lunch_adjust %in% c("0", "8", "8, fasting month") ~ 1,
                                            GQ2_Place_Lunch_adjust %in% c("2", "3", "4", "5", "0,2", "0 and 3", "0, 3, 7, brunch (11am meal)", "0, 5", "2, 5", "2,3,5", "2,4,5", "3,5", "5,2", "7, brunch-outside","7: cook self since work has cooking facilities") ~ 2,
                                            .default = 3))


whole_cohort<- whole_cohort %>% 
  mutate(GQ3_Place_Dinner_new_v4 = case_when(is.na(GQ3_Place_Dinner_adjust) == TRUE ~ NA,
                                             GQ3_Place_Dinner_adjust %in% c("0", "7,Mother's place", "8") ~ 1,
                                             GQ3_Place_Dinner_adjust %in% c("2", "3", "4", "5", "0, 2", "0, 2, 5", "0, 5", "0,3", "2,5", "7, food stall", "7,office pantry") ~ 2,
                                             .default = 3))

# 
# 1:3,
#                                        labels = 

whole_cohort$GQ1_Place_BF_new_v4<- factor(whole_cohort$GQ1_Place_BF_new_v4,
                                          levels = 1:3,
                                          labels = c("Eat at home", "Eat in outside", "Packed from home/ Take out from outside"))

whole_cohort$GQ2_Place_Lunch_new_v4<- factor(whole_cohort$GQ2_Place_Lunch_new_v4,
                                             levels = 1:3,
                                             labels = c("Eat at home", "Eat in outside", "Packed from home/ Take out from outside"))

whole_cohort$GQ3_Place_Dinner_new_v4<- factor(whole_cohort$GQ3_Place_Dinner_new_v4,
                                              levels = 1:3,
                                              labels = c("Eat at home", "Eat in outside", "Packed from home/ Take out from outside"))



whole_cohort$ethnicity_specified_recat<- factor(whole_cohort$ethnicity_specified_recat,
                                         levels = c("Chinese","Malay","Indian"))

whole_cohort$pcv1_highest_education_completed_recat<- factor(whole_cohort$pcv1_highest_education_completed_recat,
                                                      levels=c("University","Primary/Secondary/Post_secondary"))

whole_cohort$pcv1_household_income_recat<- factor(whole_cohort$pcv1_household_income_recat,
                                           levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))

whole_cohort$pcv1_parity_recat<- factor(whole_cohort$pcv1_parity_recat,
                                 levels=c("0",">= 1"))

whole_cohort$age_at_recruitment_cat <- whole_cohort$age_at_recruitment
for(i in 1:nrow(whole_cohort)){
  if(is.na(whole_cohort$age_at_recruitment[i])==TRUE) {whole_cohort$age_at_recruitment_cat[i]<- NA}
  else if(whole_cohort$age_at_recruitment[i] < 28.76){whole_cohort$age_at_recruitment_cat[i] <- "Age first tertile"}
  else if ((whole_cohort$age_at_recruitment[i] >= 28.76) & (whole_cohort$age_at_recruitment[i] < 32.1)){whole_cohort$age_at_recruitment_cat[i] <- "Age second tertile"}
  else if (whole_cohort$age_at_recruitment[i] >= 32.09){whole_cohort$age_at_recruitment_cat[i] <- "Age third tertile"}
}

whole_cohort$age_at_recruitment_cat <- factor(whole_cohort$age_at_recruitment_cat,
                                       levels=c("Age first tertile","Age second tertile","Age third tertile"))


whole_cohort$Styrofoam_boxes<- factor(whole_cohort$Styrofoam_boxes,
                               levels = c("No", "Yes"))
whole_cohort$Plastic_boxes<- factor(whole_cohort$Plastic_boxes,
                             levels = c("No", "Yes"))


whole_cohort$occupation<- factor(whole_cohort$occupation,
                             levels = c("Not working", "Office worker", "Health care worker", "Service worker & Outdoor worker"))




blood_nonimpute<- whole_cohort %>% 
                  left_join(FFQ, by="Subject_ID")%>%
                  filter(participation == 1) 
  
  
  
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
whole_table <- whole_cohort %>%
  dplyr::select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, occupation, 
                pcv1_parity_recat, TotalFish,Fast_food, fruit, vegetable,
                Styrofoam_boxes,Plastic_boxes,
                GQ1_Place_BF_new_v4, GQ2_Place_Lunch_new_v4, GQ3_Place_Dinner_new_v4) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical"),
              missing_text = "Missing", 
              digits = everything() ~ c(0, 1)
  ) %>%
  bold_labels()



blood_table <- blood_nonimpute %>%
  dplyr::select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, occupation, 
                pcv1_parity_recat, TotalFish,Fast_food, fruit, vegetable,
                Styrofoam_boxes,Plastic_boxes,
                GQ1_Place_BF_new_v4, GQ2_Place_Lunch_new_v4, GQ3_Place_Dinner_new_v4) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical"),
              missing_text = "Missing", 
              digits = everything() ~ c(0, 1)
  ) %>%
  bold_labels()



urine_table <- urine_nonimpute %>%
  dplyr::select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, occupation, 
                pcv1_parity_recat, TotalFish,Fast_food, fruit, vegetable,
                Styrofoam_boxes,Plastic_boxes,
                GQ1_Place_BF_new_v4, GQ2_Place_Lunch_new_v4, GQ3_Place_Dinner_new_v4) %>%
  tbl_summary(type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical"),
              missing_text = "Missing", 
              digits = everything() ~ c(0, 1)
  ) %>%
  bold_labels()


table<- tbl_merge(tbls =list(whole_table, blood_table, urine_table), 
                  tab_spanner= c("**Whole**", "**Blood**", "**Urine**") )

table



#################### compare uring samples with all live birth

compare_urine_table <- blood_nonimpute %>%
  filter(live_birth_recat == 1) %>% 
  dplyr::select(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, occupation, 
                pcv1_parity_recat, TotalFish,Fast_food, fruit, vegetable,
                Styrofoam_boxes,Plastic_boxes,
                GQ1_Place_BF_new_v4, GQ2_Place_Lunch_new_v4, GQ3_Place_Dinner_new_v4,urine_participation) %>%
  tbl_summary(by = urine_participation,
              type =  list(Styrofoam_boxes ~ "categorical", Plastic_boxes ~ "categorical"),
              missing_text = "Missing", 
              digits = everything() ~ c(0, 1)
  ) %>%
  bold_labels() %>% 
  add_p() %>% 
  add_overall()

compare_urine_table










