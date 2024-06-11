
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
figure2_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/PFAS_model2.csv")

#------------------------------------------------------------reformat variables
figure2_data$PFAS<- factor(figure2_data$PFAS_name,
                           levels = c('PFOS_Total_log', 'PFOA_Linear_log', 'PFNA_log', 'PFHxS_log', 'PFDA_bi', 'PFHpA_bi', 'PFHpS_bi'),
                           labels = c('PFOS', 'PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS'))

figure2_data$term<- factor(figure2_data$term,
                                 levels = c("TotalFish_sc",
                                            "Fast_food_sc",
                                            "fruit_sc",
                                            "vegetable_sc",
                                            "Styrofoam_boxesYes",
                                            "Plastic_boxesYes",
                                            "mca_dim1_catHigh-tendency-outside",
                                            "mca_dim2_catHigh-tendency-packaging"
                                 ))


figure2_data$covariates<- figure2_data$term


figure2_data$conf.low<- as.numeric(figure2_data$conf.low)
figure2_data$conf.high<- as.numeric(figure2_data$conf.high)

## figure 2 part 1
figure2_data_1<- figure2_data %>% 
  filter(PFAS %in% c('PFOS', 'PFOA', 'PFNA', 'PFHxS'))

figure2_1<- ggplot(figure2_data_1,aes(y=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width=0.1,size=1, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=Estimate)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_discrete(labels = c("TotalFish_sc" = bquote('Total Fish'~Intake^a),
                              "Fast_food_sc"= bquote('Fast Food'~Intake^a),
                              "fruit_sc" = bquote('Fruit'~Intake^a),
                              "vegetable_sc" = bquote('Total Vegetable'~Intake^a),
                              "Styrofoam_boxesYes" = bquote('Styrofoam food container use: yes vs.'~no^a),
                              "Plastic_boxesYes" = bquote('Plastic food container use: yes vs.'~no^a),
                              "mca_dim1_catHigh-tendency-outside" = bquote('Eat out: high vs. low'~tendency^a),
                              "mca_dim2_catHigh-tendency-packaging" = bquote('Consume packaged food: high vs. low'~tendency^a))) +
  geom_vline(aes(xintercept=0),linetype="dashed",size=0.5)+
  facet_grid(covariates~PFAS, scale="free", space = "free_y")+
  xlab(expression(beta ~ (`95% CI`)))+
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
  scale_y_discrete(labels = c("TotalFish_sc" = bquote('Total Fish'~Intake^a),
                              "Fast_food_sc"= bquote('Fast Food'~Intake^a),
                              "fruit_sc" = bquote('Fruit'~Intake^a),
                              "vegetable_sc" = bquote('Total Vegetable'~Intake^a),
                              "Styrofoam_boxesYes" = bquote('Styrofoam food container use: yes vs.'~no^a),
                              "Plastic_boxesYes" = bquote('Plastic food container use: yes vs.'~no^a),
                              "mca_dim1_catHigh-tendency-outside" = bquote('Eat out: high vs. low'~tendency^a),
                              "mca_dim2_catHigh-tendency-packaging" = bquote('Consume packaged food: high vs. low'~tendency^a))) +
  geom_vline(aes(xintercept=1),linetype="dashed",size=0.5)+ 
  facet_grid(covariates~PFAS, scale="free", space = "free_y")+
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 0.1)) +
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




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure3.jpeg",
     units="in", width=24, height=14, res=600)


ggarrange(figure2_1, figure2_2,
          widths = c(6,3),
          heights = c(1,0.8))

dev.off()

