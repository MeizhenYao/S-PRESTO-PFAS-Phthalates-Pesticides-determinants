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
figure3_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/phtha_model2.csv")

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
                            levels = c('MEP','MIBP','MBP','MCPP','MECPTP','MEHHTP','MEOHTP','MCIOP','MECPP','MEHHP','MEOHP','MEHP','MCINP','MBZP'))

figure3_data$covariates<- factor(figure3_data$term,
                                 levels = c(
                                   "Total Fish", 
                                   "Fast food",
                                   "Fruit",
                                   "Total Vegetable Intake",
                                   "Boiled Vegetable Intake",
                                   "Stir Fried Vegetable Intake",
                                   "Styrofoam boxes",
                                   "Styrofoam boxes frequency",
                                   "Plastic boxes",
                                   "Plastic boxes frequency",
                                   "Canned",
                                   "Canned frequency"))

figure3_data$Group<- factor(figure3_data$phtha_group,
                            levels = c("LMWPs", "HMWPs"))



figure3_data$conf.low<- as.numeric(figure3_data$conf.low)
figure3_data$conf.high<- as.numeric(figure3_data$conf.high)

figure3_2<- ggplot(figure3_data,aes(y=covariates)) +
  geom_errorbar(aes(xmin = exp(conf.low), xmax = exp(conf.high), color = Group), width=0.1,size=1)+
  geom_point(size=1.8,aes(x=exp(Estimate))) +
  geom_vline(aes(xintercept=1),linetype="dashed",size=0.3)+ 
  scale_color_manual(drop = FALSE,
                     values = c( "#E3882F", "#1B7C3D"),
                     labels = c("LMWPs", "HMWPs"))+
  facet_nested(covariates~parent+phtha, scale="free", space = "free_y", nest_line = element_line(colour = "blue"))+
  xlab(expression(beta ~ (`95% CI`)))+
  ylab("Covariates")+
  theme_bw()+
  theme(panel.spacing.y=unit(0.2, "line"),
        axis.title=element_text(face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 9,face = "bold"),
        legend.title = element_text(size = 10,face = "bold"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=10),
        strip.text.y = element_text(colour = "black", face = "bold",size=10,angle=0))   


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure3_2.jpeg",
     units="in", width=18, height=12, res=500)


figure3_2

dev.off()
