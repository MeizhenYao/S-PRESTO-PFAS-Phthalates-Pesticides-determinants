
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
figure2_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/final_model1.csv")


#------------------------------------------------------------reformat variables
figure2_data$PFAS_name<- factor(figure2_data$PFAS_name,
                           levels = c('PFOS_Total_log', 'PFOA_Linear_log', 'PFNA_log', 'PFHxS_log', 'PFDA_bi', 'PFHpA_bi', 'PFHpS_bi'),
                           labels = c('PFOS', 'PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS'))

figure2_data$covariates<- factor(figure2_data$covariates,
                                 levels = c("Ethinicity",
                                            "Income",
                                            "Occupation",
                                            "Parity",
                                            "Fast_food",
                                            "Fruit"
                                 ))

figure2_data$term<- factor(figure2_data$term,
                           levels = c("ethnicity_specified_recatIndian",
                                      "ethnicity_specified_recatMalay",
                                      "pcv1_household_income_recat$11,294 and above",
                                      "pcv1_household_income_recat$6,377 - $11,293",
                                      "occupationService worker",
                                      "occupationHealth care worker",
                                      "occupationOffice worker",
                                      "pcv1_parity_recat>= 1",
                                      "Fast_food_sc",
                                      "fruit_sc"
                                      
                           ))

figure2_data$Estimate<- round(as.numeric(figure2_data$Estimate), 2)
figure2_data$conf.low<- round(as.numeric(figure2_data$conf.low), 2)
figure2_data$conf.high<- round(as.numeric(figure2_data$conf.high), 2)

## figure 2 part 1
figure2_data_1<- figure2_data %>% 
  filter(PFAS %in% c('PFOS', 'PFOA', 'PFNA', 'PFHxS'))

figure2_1<- ggplot(figure2_data_1,aes(y=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width=0.1,size=1, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=Estimate)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_discrete(labels = c("ethnicity_specified_recatMalay" = bquote('Malay vs.'~Chinese^a),
                              "ethnicity_specified_recatIndian"= bquote('Indian vs.'~Chinese^a),
                              "pcv1_household_income_recat$6,377 - $11,293" = bquote('$6,377 - $11,293 vs.$6,376 and '~below^a),
                              "pcv1_household_income_recat$11,294 and above" = bquote('$11,294 and above vs.$6,376 and '~below^a),
                              "occupationOffice worker" = bquote('Office worker vs. Not'~working^a),
                              "occupationHealth care worker" = bquote('Health care worker vs. Not'~working^a),
                              "occupationService worker" = bquote('Service worker vs. Not'~working^a),
                              "pcv1_parity_recat>= 1" = bquote('Multiparous vs.'~Nulliparous^a),
                              "fruit_sc" = bquote('Fruit'~Intake^a),
                              "Fast_food_sc"= bquote('Fast Food'~Intake^a)))+
  geom_vline(aes(xintercept=0),linetype="dashed",size=0.5)+
  facet_grid(covariates~PFAS, scale="free", space = "free_y")+
  xlab(expression(beta ~ ("95% CI")))+
  ylab("Covariates")+
  theme_bw()+
  theme(panel.spacing.y=unit(0, "line"),
        axis.title=element_text(face="bold", size=18),
        axis.text.x = element_text(colour = "black", size=15),
        axis.text.y = element_text(colour = "black", face = "bold",size=18),
        strip.background = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=18),
        strip.text.y = element_blank())   

figure2_1


## figure 2 part 2
figure2_data_2<- figure2_data %>% 
  filter(PFAS %in% c('PFDA', 'PFHpA', 'PFHpS'))

figure2_2<- ggplot(figure2_data_2,aes(y=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width=0.1,size=1, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=Estimate)) +
  geom_vline(aes(xintercept=1),linetype="dashed",size=0.5)+ 
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_discrete(labels = c("ethnicity_specified_recatMalay" = bquote('Malay vs.'~Chinese^a),
                              "ethnicity_specified_recatIndian"= bquote('Indian vs.'~Chinese^a),
                              "pcv1_household_income_recat$6,377 - $11,293" = bquote('$6,377 - $11,293 vs.$6,376 and '~below^a),
                              "pcv1_household_income_recat$11,294 and above" = bquote('$11,294 and above vs.$6,376 and '~below^a),
                              "occupationOffice worker" = bquote('Office worker vs. Not'~working^a),
                              "occupationHealth care worker" = bquote('Health care worker vs. Not'~working^a),
                              "occupationService worker" = bquote('Service worker vs. Not'~working^a),
                              "pcv1_parity_recat>= 1" = bquote('Multiparous vs.'~Nulliparous^a),
                              "fruit_sc" = bquote('Fruit'~Intake^a),
                              "Fast_food_sc"= bquote('Fast Food'~Intake^a)))+
  facet_grid(covariates~PFAS, scale="free", space = "free_y")+
  xlab("OR (95% CI)")+
  ylab("")+
  theme_bw()+
  theme(panel.spacing.y=unit(0, "line"),
        axis.title=element_text(face="bold",size=18),
        axis.text.x = element_text(colour = "black", size=15),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=18),
        strip.text.y = element_blank())   

figure2_2




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/supp_figure6.jpeg",
     units="in", width=20, height=10, res=600)


ggarrange(figure2_1, figure2_2,
          widths = c(6,3),
          heights = c(1,0.8))

dev.off()

