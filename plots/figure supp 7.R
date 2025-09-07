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
#------------------------------------------------------------import dataset
figures7_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/phtha_model2_ind.csv")

#------------------------------------------------------------reformat variables

figures7_data$term<- factor(figures7_data$term,
                           levels = c("TotalFish_sc",
                                      "Fast_food_sc",
                                      "fruit_sc",
                                      "vegetable_sc",
                                      "Styrofoam_boxesYes",
                                      "Plastic_boxesYes",
                                      "mca_dim1_catHigh-tendency-outside",
                                      "mca_dim2_catHigh-tendency-packaging"
                           ),
                           labels = c('Total Fish Intake',
                                      'Fast Food Intake',
                                      'Fruit Intake',
                                      'Total Vegetable Intake',
                                      'Styrofoam food container use: yes vs. no',
                                      'Plastic food container use: yes vs. no',
                                      'Eat out: high vs. low tendency',
                                      'Consume packaged food: high vs. low tendency'))

figures7_data$covariates<- factor(figures7_data$term)


figures7_data$phtha_parent<- factor(figures7_data$phtha_parent,
                                   levels = c("DEP", "DiBP", "DBP", "DEHTP", "DiNP", "DEHP", "BBzP", "DiDP"))


figures7_data<- figures7_data %>% 
  mutate(parent = case_when(phtha_parent == "DEP" ~ "Diethyl\nphthalate",
                            phtha_parent == "DiBP" ~ "Di-iso-butyl\nphthalate",
                            phtha_parent == "DBP" ~ "Di-n-butyl\nphthalate",
                            phtha_parent == "DEHTP" ~ "Di-2-ethylhexyl\nterephthalate",
                            phtha_parent == "DiNP" ~ "Di-iso-nonyl\nphthalate",
                            phtha_parent == "DEHP" ~ "Di-2-ethylhexyl\nphthalate",
                            phtha_parent == "BBzP" ~ "Butylbenzyl\nphthalate",
                            phtha_parent == "DiDP" ~ "Di-isodecyl\nphthalate"
  ))

figures7_data$parent<- factor(figures7_data$parent,
                             levels = c("Diethyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-iso-nonyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Butylbenzyl\nphthalate", "Di-isodecyl\nphthalate"))


figures7_data$phtha<- factor(figures7_data$phtha,
                            levels = c('adjusted_MEP_log','adjusted_MIBP_log','adjusted_MBP_log',
                                       'adjusted_MCPP_log','adjusted_MECPTP_log','adjusted_MEHHTP_log',
                                       'adjusted_MEOHTP_log','adjusted_MCIOP_log','adjusted_MECPP_log',
                                       'adjusted_MEHHP_log','adjusted_MEOHP_log','adjusted_MEHP_log',
                                       'adjusted_MCINP_log','adjusted_MBZP_log'),
                            labels = c('MEP','MIBP','MBP','MCPP','MECPTP','MEHHTP','MEOHTP','MCIOP','MECPP','MEHHP','MEOHP','MEHP','MCINP','MBZP'))


figures7_data$Group<- factor(figures7_data$phtha_group,
                            levels = c("LMWPs", "HMWPs"))


figures7_data$Timepoint<- factor(figures7_data$Timepoint,
                                 levels = c("Preconception", "Pregnancy"))

figures7_data$conf.low<- as.numeric(figures7_data$conf.low)
figures7_data$conf.high<- as.numeric(figures7_data$conf.high)
# 100*(exp(conf.low)-1)


figures7_2<- ggplot(figures7_data,aes(y=term, shape=fct_rev(Timepoint))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = Group), width=0.1,size=1, position = position_dodge(width = 0.5))+
  geom_point(size=2.2,aes(x=Estimate), position = position_dodge(width = 0.5), color = "black") +
  geom_vline(aes(xintercept=0),linetype="dashed",size=0.5) + 
  scale_shape_manual(drop = FALSE,
                     values = c(15, 17),
                     labels = c("Preconception", "Pregnancy"),
                     breaks = c("Preconception", "Pregnancy"))+
  scale_color_manual(drop = FALSE,
                     values = c( "#E3882F", "#1B7C3D"),
                     labels = c("LMWPs", "HMWPs"))+
  facet_nested(covariates~parent + phtha, scale="free", space = "free_y", nest_line = element_line(colour = "blue"))+
  xlab(expression(beta ~ (`95% CI`)))+
  ylab("Covariates")+
  labs(shape = "Timepoint")+
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

       
jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/supp_figure7.jpeg",
     units="in", width=22, height=10, res=600)


figures7_2

dev.off()
