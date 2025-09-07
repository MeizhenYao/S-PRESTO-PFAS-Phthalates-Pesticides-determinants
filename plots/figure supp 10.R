
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
#------------------------------------------------------------import dataset
figures10_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/PFAS_ethi_strata.csv")

# remove sociodemo from plot


#------------------------------------------------------------reformat variables
figures10_data$PFAS<- factor(figures10_data$PFAS_name,
                           levels = c('PFOS_Total_log', 'PFOA_Linear_log', 'PFNA_log', 'PFHxS_log', 'PFDA_bi', 'PFHpA_bi', 'PFHpS_bi'),
                           labels = c('PFOS', 'PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS'))


figures10_data$ethnicity<- factor(figures10_data$ethnicity,
                                  levels = c("Chinese", "Malay/Indian"),
                                  labels = c("Chinese", "Indian/Malay"))

figures10_data$term<- factor(figures10_data$term,
                             levels = c("Fast_food_sc", "pcv1_highest_education_completed_recatPrimary/Secondary/Post_secondary"),
                             labels = c("Fast Food Intake", "Primary/Secondary/Post_secondary vs. University")
                             )

# figures10_data$term<- factor(figures10_data$term,
#                              levels = c("TotalFish_sc", "fruit_sc", "Fast_food_sc", "vegetable_sc"),
#                              labels = c("Total Fish Intake", "Fruit Intake", "Fast Food  Intake", "Total Vegetable Intake")
# )


figures10_data$Estimate<- as.numeric(figures10_data$Estimate)
figures10_data$conf.low<- as.numeric(figures10_data$conf.low)
figures10_data$conf.high<- as.numeric(figures10_data$conf.high)

# 
# figures10_data<- figures10_data %>% 
#                  mutate(Estimate = ifelse(beta == "beta", 100*(2^Estimate-1), Estimate),
#                         conf.low = ifelse(beta == "beta", 100*(2^conf.low-1), conf.low),
#                         conf.high = ifelse(beta == "beta", 100*(2^conf.high-1), conf.high))


## continuous PFAS
figures10_data_1<- figures10_data %>% 
  filter(PFAS %in% c('PFOS', 'PFOA', 'PFNA', 'PFHxS'))



figure2_1<- ggplot(figures10_data_1,aes(y=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width=0.1,size=1.4, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=Estimate)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  geom_vline(aes(xintercept=0),linetype="dashed",size=0.5)+
  facet_grid(ethnicity~PFAS, scale="free", space = "free_y")+
  xlab(expression(beta ~ ("95% CI")))+
  ylab("Covariates")+
  theme_bw()+
  theme(panel.spacing.y=unit(0.4, "line"),
        axis.title=element_text(face="bold", size=18),
        axis.text.x = element_text(colour = "black", size=15),
        axis.text.y = element_text(colour = "black", face = "bold",size=18),
        strip.background = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=18),
        strip.text.y = element_blank())   

figure2_1

## dichotomous PFAS
figures10_data_2<- figures10_data %>% 
  filter(PFAS %in% c('PFDA', 'PFHpA', 'PFHpS'))

figure2_2<- ggplot(figures10_data_2,aes(y=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width=0.1,size=1.4, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=Estimate))+ 
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 0.1)) +
  geom_vline(aes(xintercept=1),linetype="dashed",size=0.5)+
  facet_grid(ethnicity~PFAS, scale="free", space = "free_y")+
  xlab("OR (95% CI)")+
  ylab("")+
  theme_bw()+
  theme(panel.spacing.y=unit(0.4, "line"),
        axis.title=element_text(face="bold", size=18),
        axis.text.x = element_text(colour = "black", size=15),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=18),
        strip.text.y = element_text(colour = "black", face = "bold",size=18))   

figure2_2




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/supp_figure10.jpeg",
     units="in", width=26, height=12, res=600)


ggarrange(figure2_1, figure2_2,
          widths = c(10,5),
          heights = c(1,0.8))


dev.off()
