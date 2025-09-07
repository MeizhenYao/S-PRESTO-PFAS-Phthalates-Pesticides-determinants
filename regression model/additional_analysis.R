# load packages
library(broom)
library(gtsummary)
library(gt)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(readxl)
library(boot)
library(table1)
library(flextable)
library(plyr)
library(Rcpp)
library(modelr)
library(readr)
library(gWQS)
library(broom.mixed)
library(yaml)
library(rmarkdown)
library(officer)
library(scales)
library(lme4)

# import data (change the path to the location where you save the dataset)
PFAS<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")

# rosnerTest(PFAS$PFHxS, k=10)
# rosnerTest(PFAS$PFOS_Total, k=10)
# rosnerTest(PFAS$PFOA_Linear, k=10)

## log transformation
PFAS_log <- PFAS %>%  
  mutate(PFHxS_log=log2(PFHxS), 
         PFOS_Total_log=log2(PFOS_Total), 
         PFOA_Linear_log=log2(PFOA_Linear), 
         PFNA_log=log2(PFNA),
         TotalFish_sc = (TotalFish - mean(TotalFish))/sd(TotalFish),
         Fast_food_sc = (Fast_food - mean(Fast_food))/sd(Fast_food),
         fruit_sc = (fruit - mean(fruit))/sd(fruit),
         vegetable_sc = (vegetable - mean(vegetable))/sd(vegetable),
         vegetable_boil_sc = (vegetable_boil - mean(vegetable_boil))/sd(vegetable_boil),
         vegetable_SF_sc = (vegetable_SF - mean(vegetable_SF))/sd(vegetable_SF),
         Styrofoam_boxes_freq_mean_sc = (Styrofoam_boxes_freq_mean - mean(Styrofoam_boxes_freq_mean))/sd(Styrofoam_boxes_freq_mean),
         Plastic_boxes_freq_mean_sc = (Plastic_boxes_freq_mean - mean(Plastic_boxes_freq_mean))/sd(Plastic_boxes_freq_mean))

## numeric
PFAS_log$PFHpA_bi<- PFAS_log$PFHpA_Comment
for(i in 1:nrow(PFAS_log)){
  if ( PFAS_log$PFHpA_Comment[i]==37){PFAS_log$PFHpA_bi[i]<-0}
  else if (PFAS_log$PFHpA_Comment[i]==0){PFAS_log$PFHpA_bi[i]<-1}
}

PFAS_log$PFHpA_bi<- factor(PFAS_log$PFHpA_bi,
                           levels = c(0,1))

PFAS_log$PFDA_bi<- PFAS_log$PFDA_Comment
for(i in 1:nrow(PFAS_log)){
  if ( PFAS_log$PFDA_Comment[i]==37){PFAS_log$PFDA_bi[i]<-0}
  else if (PFAS_log$PFDA_Comment[i]==0){PFAS_log$PFDA_bi[i]<-1}
}

PFAS_log$PFDA_bi<- factor(PFAS_log$PFDA_bi,
                          levels = c(0,1))

PFAS_log$PFHpS_bi<- PFAS_log$PFHpS_Comment
for(i in 1:nrow(PFAS_log)){
  if ( PFAS_log$PFHpS_Comment[i]==37){PFAS_log$PFHpS_bi[i]<-0}
  else if (PFAS_log$PFHpS_Comment[i]==0){PFAS_log$PFHpS_bi[i]<-1}
}

PFAS_log$PFHpS_bi<- factor(PFAS_log$PFHpS_bi,
                           levels = c(0,1))

## covariates
PFAS_log$ethnicity_specified_recat<- factor(PFAS_log$ethnicity_specified_recat,
                                            levels = c("Chinese","Malay","Indian"))

PFAS_log$pcv1_highest_education_completed_recat<- factor(PFAS_log$pcv1_highest_education_completed_recat,
                                                         levels=c("University","Primary/Secondary/Post_secondary"))

PFAS_log$pcv1_household_income_recat<- factor(PFAS_log$pcv1_household_income_recat,
                                              levels=c("$6,376 and below","$6,377 - $11,293","$11,294 and above"))


PFAS_log$pcv1_parity_recat<- factor(PFAS_log$pcv1_parity_recat,
                                    levels=c("0",">= 1"))



PFAS_log$age_at_recruitment_cat <- factor(PFAS_log$age_at_recruitment_cat,
                                          levels=c("Age first tertile","Age second tertile","Age third tertile"))


PFAS_log$Styrofoam_boxes<- factor(PFAS_log$Styrofoam_boxes,
                                  levels = c("No", "Yes"))
PFAS_log$Plastic_boxes<- factor(PFAS_log$Plastic_boxes,
                                levels = c("No", "Yes"))



PFAS_log$occupation<- factor(PFAS_log$occupation,
                             levels = c("Not working", "Office worker", "Health care worker", "Service worker"))


q_age<- quantile(PFAS_log$mca_dim1, probs = c(1/2), type=2)
PFAS_log$mca_dim1_cat <- PFAS_log$mca_dim1
for(i in 1:nrow(PFAS_log)){
  if(is.na(PFAS_log$mca_dim1[i])==TRUE) {PFAS_log$mca_dim1_cat[i]<- NA}
  else if(PFAS_log$mca_dim1[i] < q_age[1]){PFAS_log$mca_dim1_cat[i] <- "Low-tendency-outside"}
  else if (PFAS_log$mca_dim1[i] >= q_age[1]){PFAS_log$mca_dim1_cat[i] <- "High-tendency-outside"}
}
PFAS_log$mca_dim1_cat<- factor(PFAS_log$mca_dim1_cat,
                               levels=c("Low-tendency-outside","High-tendency-outside"))


q_age<- quantile(PFAS_log$mca_dim2, probs = c(1/2), type=2)
PFAS_log$mca_dim2_cat <- PFAS_log$mca_dim2
for(i in 1:nrow(PFAS_log)){
  if(is.na(PFAS_log$mca_dim2[i])==TRUE) {PFAS_log$mca_dim2_cat[i]<- NA}
  else if(PFAS_log$mca_dim2[i] < q_age[1]){PFAS_log$mca_dim2_cat[i] <- "Low-tendency-packaging"}
  else if (PFAS_log$mca_dim2[i] >= q_age[1]){PFAS_log$mca_dim2_cat[i] <- "High-tendency-packaging"}
}
PFAS_log$mca_dim2_cat<- factor(PFAS_log$mca_dim2_cat,
                               levels=c("Low-tendency-packaging","High-tendency-packaging"))



## PFAS information
PFAS_cont<- c('PFOS_Total_log', 'PFOA_Linear_log', 'PFNA_log', 'PFHxS_log')

PFAS_bi<- c('PFDA_bi', 'PFHpA_bi', 'PFHpS_bi')

PFAS_name<- c('PFOS_Total_log', 'PFOA_Linear_log', 'PFNA_log', 'PFHxS_log', 'PFDA_bi', 'PFHpA_bi', 'PFHpS_bi')

beta<- c('beta', 'beta', 'beta', 'beta', 'log(OR)', 'log(OR)', 'log(OR)')
PFAS_info<- data.frame(PFAS_name, 
                       beta)

tbl_summary(PFAS_log,
            include = c(ethnicity_specified_recat, age_at_recruitment_cat, pcv1_highest_education_completed_recat, pcv1_household_income_recat, pcv1_parity_recat, 
                        Styrofoam_boxes,Plastic_boxes,
                        occupation)) %>% 
  bold_labels()




######--------------------------------- check distribution
ggplot(PFAS_log) + geom_density(aes(x = PFNA_log)) # change PFNA to binary variable?
ggplot(PFAS_log) + geom_histogram(aes(x = PFNA_log))

ggplot(PFAS_log) + geom_histogram(aes(x = Fast_food))
ggplot(PFAS_log) + geom_histogram(aes(x = Fast_food_sc))
ggplot(PFAS_log) + geom_histogram(aes(x = log2(Fast_food+0.1))) # change dietary variable into log2-transformation? (+0.1 or 1?)


ggplot(PFAS_log) + geom_point(aes(x = Fast_food_sc, y=PFOS_Total_log)) + geom_smooth(aes(x = Fast_food_sc, y=PFOS_Total_log), method = "gam") # change intake to low vs high?
ggplot(PFAS_log) + geom_point(aes(x = log2(Fast_food+0.1), y=PFOS_Total_log)) + geom_smooth(aes(x = log2(Fast_food+0.1), y=PFOS_Total_log), method = "gam") # change intake to low vs high?


ggplot(PFAS_log) + geom_point(aes(x = fruit_sc, y=PFOS_Total_log)) + geom_smooth(aes(x = fruit_sc, y=PFOS_Total_log), method = "gam")

ggplot(PFAS_log) + geom_point(aes(x = TotalFish, y=PFOS_Total_log)) + geom_smooth(aes(x = TotalFish, y=PFOS_Total_log), method = "gam")

ggplot(PFAS_log) + geom_point(aes(x = vegetable, y=PFOS_Total_log)) + geom_smooth(aes(x = vegetable, y=PFOS_Total_log), method = "gam")



## for fast food 
### dichotomous
q_age<- quantile(PFAS_log$Fast_food, probs = c(1/2), type=2)
PFAS_log$Fast_food_cat <- PFAS_log$Fast_food
for(i in 1:nrow(PFAS_log)){
  if(is.na(PFAS_log$Fast_food[i])==TRUE) {PFAS_log$Fast_food_cat[i]<- NA}
  else if(PFAS_log$Fast_food[i] < q_age[1]){PFAS_log$Fast_food_cat[i] <- "Low"}
  else if (PFAS_log$Fast_food[i] >= q_age[1]){PFAS_log$Fast_food_cat[i] <- "High"}
}
PFAS_log$Fast_food_cat<- factor(PFAS_log$Fast_food_cat,
                               levels=c("Low","High"))

ggplot(PFAS_log) + geom_boxplot(aes(x = Fast_food_cat, y=PFOS_Total_log))

ggplot(PFAS_log) + geom_boxplot(aes(x = ethnicity_specified_recat, y=PFOS_Total_log, color = Fast_food_cat))

ggplot(PFAS_log) + geom_boxplot(aes(x = mca_dim1_cat, y=PFOS_Total_log, color = Fast_food_cat))

ggplot(PFAS_log) + geom_boxplot(aes(x = Fast_food_cat, y=PFOS_Total_log, color = mca_dim2_cat))


ggplot(PFAS_log) + geom_boxplot(aes(x = mca_dim2_cat, y=PFOS_Total_log))

### tertiles
q_age<- quantile(PFAS_log$Fast_food, probs = c(1/4, 2/4, 3/4), type=2)
PFAS_log$Fast_food_cat2 <- PFAS_log$Fast_food
for(i in 1:nrow(PFAS_log)){
  if(is.na(PFAS_log$Fast_food[i])==TRUE) {PFAS_log$Fast_food_cat2[i]<- NA}
  else if(PFAS_log$Fast_food[i] <= q_age[1]){PFAS_log$Fast_food_cat2[i] <- "Low"}
  else if ((PFAS_log$Fast_food[i] > q_age[1]) & (PFAS_log$Fast_food[i] <= q_age[2])){PFAS_log$Fast_food_cat2[i] <- "Mid"}
  else if ((PFAS_log$Fast_food[i] > q_age[2]) & (PFAS_log$Fast_food[i] <= q_age[3])){PFAS_log$Fast_food_cat2[i] <- "Mid2"}
  else if (PFAS_log$Fast_food[i] > q_age[3]){PFAS_log$Fast_food_cat2[i] <- "High"}
}
PFAS_log$Fast_food_cat2<- factor(PFAS_log$Fast_food_cat2,
                               levels=c("Low","Mid","Mid2","High"))


ggplot(PFAS_log) + geom_boxplot(aes(x = Fast_food_cat2, y=PFOS_Total_log))
ggplot(PFAS_log) + geom_boxplot(aes(x = ethnicity_specified_recat, y=PFOS_Total_log, color = Fast_food_cat2))


ggplot(PFAS_log) + geom_point(aes(x = mca_dim1, y=PFNA_log)) + geom_smooth(aes(x = mca_dim1, y=PFNA_log), method = "gam")

ggplot(PFAS_log) + geom_point(aes(x = mca_dim2, y=PFNA_log)) + geom_smooth(aes(x = mca_dim2, y=PFNA_log), method = "gam")

ggplot(PFAS_log) + geom_boxplot(aes(x = mca_dim1_cat, y=PFNA_log))

ggplot(PFAS_log) + geom_boxplot(aes(x = mca_dim2_cat, y=PFNA_log))


