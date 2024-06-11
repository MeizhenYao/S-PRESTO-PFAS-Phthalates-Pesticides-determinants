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
figure4_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/pest_model2.csv")

#------------------------------------------------------------reformat variables
figure4_data$PEST<- factor(figure4_data$PEST,
                           levels = c('adjusted_DEP_log', 'adjusted_DMP_log', 'adjusted_DETP_log', 'adjusted_DMTP_log', 'adjusted_TCP_log', 
                                      'adjusted_PNP_log', 'adjusted_DEDP_log', 'adjusted_DMDP_log', 'adjusted_PBA_log', 'adjusted_TRANS_DCCA_log', 'adjusted_CIS_DCCA_log'),
                           labels = c('DEP', 'DMP', 'DETP', 'DMTP', 'TCP', 'PNP', 'DEDP', 'DMDP', 'PBA', 'TRANS DCCA', 'CIS DCCA'))

figure4_data$term<- factor(figure4_data$term,
                           levels = c("TotalFish_sc",
                                      "Fast_food_sc",
                                      "fruit_sc",
                                      "vegetable_sc",
                                      "Styrofoam_boxesYes",
                                      "Plastic_boxesYes",
                                      "mca_dim1_catHigh-tendency-outside",
                                      "mca_dim2_catHigh-tendency-packaging"
                           ))

figure4_data$covariates<- factor(figure4_data$term)

figure4_data$Group<- factor(figure4_data$Group,
                            levels = c("OP pesticides", "PYR metabolites"),
                            labels = c("OP pesticides", "Pyrethroids"))



figure4_data$conf.low<- as.numeric(figure4_data$conf.low)
figure4_data$conf.high<- as.numeric(figure4_data$conf.high)


figure4_2<- ggplot(figure4_data,aes(y=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = Group), width=0.1,size=1)+
  geom_point(size=1.8,aes(x=Estimate)) +
  scale_y_discrete(labels = c("TotalFish_sc" = bquote('Total Fish'~Intake^a),
                              "Fast_food_sc"= bquote('Fast Food'~Intake^a),
                              "fruit_sc" = bquote('Fruit'~Intake^a),
                              "vegetable_sc" = bquote('Total Vegetable'~Intake^a),
                              "Styrofoam_boxesYes" = bquote('Styrofoam food container use: yes vs.'~no^a),
                              "Plastic_boxesYes" = bquote('Plastic food container use: yes vs.'~no^a),
                              "mca_dim1_catHigh-tendency-outside" = bquote('Eat out: high vs. low'~tendency^a),
                              "mca_dim2_catHigh-tendency-packaging" = bquote('Consume packaged food: high vs. low'~tendency^a)))+ 
  geom_vline(aes(xintercept=0),linetype="dashed",size=0.3)+ 
  scale_color_manual(drop = FALSE,
                     values = c("#C52A20", "#8E549E"),
                     labels = c("OP pesticides", "Pyrethroids"))+
  facet_grid(covariates~PEST, scale="free", space = "free_y")+
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


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure8.jpeg",
     units="in", width=22, height=10, res=600)


figure4_2


dev.off()
