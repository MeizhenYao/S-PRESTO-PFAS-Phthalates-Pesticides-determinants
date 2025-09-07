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
library(blme)
library(glmnet)
library(ggh4x)
library(grid)
library(officer)
#------------------------------------------------------------import dataset
figure3_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/phtha_model1.csv")

#------------------------------------------------------------reformat variables
figure3_data$phtha_parent<- factor(figure3_data$phtha_parent,
                                   levels = c("DEP", "DiBP", "DBP", "DEHTP", "DiNP", "DEHP", "BBzP", "DiDP"))


figure3_data<- figure3_data %>% 
               mutate(parent = case_when(phtha_parent == "DEP" ~ "Diethyl\nphthalate",
                                         phtha_parent == "DiBP" ~ "Di-iso-butyl\nphthalate",
                                         phtha_parent == "DBP" ~ "Di-n-butyl\nphthalate",
                                         phtha_parent == "DEHTP" ~ "Di-2-ethylhexyl\nterephthalate",
                                         phtha_parent == "DiNP" ~ "Di-iso-nonyl\nphthalate",
                                         phtha_parent == "DEHP" ~ "Di-2-ethylhexyl\nphthalate",
                                         phtha_parent == "BBzP" ~ "Butylbenzyl\nphthalate",
                                         phtha_parent == "DiDP" ~ "Di-isodecyl\nphthalate"
               ))

figure3_data$parent<- factor(figure3_data$parent,
                             levels = c("Diethyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-iso-nonyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Butylbenzyl\nphthalate", "Di-isodecyl\nphthalate"))


figure3_data$phtha<- factor(figure3_data$phtha,
                            levels = c('adjusted_MEP_log','adjusted_MIBP_log','adjusted_MBP_log',
                                       'adjusted_MCPP_log','adjusted_MECPTP_log','adjusted_MEHHTP_log',
                                       'adjusted_MEOHTP_log','adjusted_MCIOP_log','adjusted_MECPP_log',
                                       'adjusted_MEHHP_log','adjusted_MEOHP_log','adjusted_MEHP_log',
                                       'adjusted_MCINP_log','adjusted_MBZP_log'),
                            labels = c('MEP','MIBP','MBP','MCPP','MECPTP','MEHHTP','MEOHTP','MCIOP','MECPP','MEHHP','MEOHP','MEHP','MCINP','MBZP'))

figure3_data$covariates<- factor(figure3_data$covariates,
                                 levels = c(
                                   "Ethnicity", 
                                   "Age",
                                   "Education",
                                   "Income",
                                   "Occupation",
                                   "Parity"))

figure3_data$term<- factor(figure3_data$term,
                           levels = c("ethnicity_specified_recatIndian/Malay",
                                      "age_at_recruitment_catAge third tertile",
                                      "age_at_recruitment_catAge second tertile",
                                      "pcv1_highest_education_completed_recatPrimary/Secondary/Post_secondary",
                                      "pcv1_household_income_recat$11,294 and above",
                                      "pcv1_household_income_recat$6,377 - $11,293",
                                      "occupationService worker",
                                      "occupationHealth care worker",
                                      "occupationOffice worker",
                                      "pcv1_parity_recat>= 1"
                           ),
                           labels = c('Indian/Malay vs.Chinese',
                                      'above 31 years vs. below 29 years',
                                      '29 - 31 years vs. below 29 years',
                                      'Primary/Secondary/Post_secondary vs. University',
                                      '$11,294 and above vs. $6,376 and below',
                                      '$6,377 - $11,293 vs.$6,376 and below',
                                      'Service worker vs. Not working',
                                      'Health care worker vs. Not working',
                                      'Office worker vs. Not working',
                                      'Multiparous vs. Nulliparous'))

figure3_data$Group<- factor(figure3_data$phtha_group,
                            levels = c("LMWPs", "HMWPs"))


figure3_data$conf.low<- as.numeric(figure3_data$conf.low)
figure3_data$conf.high<- as.numeric(figure3_data$conf.high)

figure3_1<- ggplot(figure3_data,aes(y=term)) +
            geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = Group), width=0.1,size=1)+
            geom_point(size=1.8,aes(x=Estimate)) +
            geom_vline(aes(xintercept=0),linetype="dashed",size=0.3)+ 
            scale_color_manual(drop = FALSE,
                              values = c( "#E3882F", "#1B7C3D"),
                              labels = c("LMWPs", "HMWPs"))+
            facet_nested(covariates~parent+phtha, scale="free", space = "free_y", nest_line = element_line(colour = "blue"))+
            xlab(expression(beta ~ (`95% CI`)))+
            ylab("Covariates")+
            theme_bw()+
            theme(panel.spacing.y=unit(0, "line"),
                  axis.text.x = element_text(colour = "black", size=10),
                  axis.text.y = element_text(colour = "black", face = "bold",size=16),
                  axis.title=element_text(face="bold", size = 18),
                  legend.text = element_text(size = 15,face = "bold"),
                  legend.title = element_text(size = 15,face = "bold"),
                  legend.position = "bottom",
                  strip.background = element_blank(),
                  strip.text.x = element_text(colour = "black", face = "bold",size=14),
                  strip.text.y = element_blank())   


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure6.jpeg",
     units="in", width=22, height=10, res=600)


figure3_1

dev.off()

