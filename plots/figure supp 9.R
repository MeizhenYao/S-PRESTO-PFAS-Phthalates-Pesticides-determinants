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
figures9_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/pest_model2_ind.csv")

#------------------------------------------------------------reformat variables
figures9_data$pest<- factor(figures9_data$pest,
                           levels = c('adjusted_DEP_log', 'adjusted_DMP_log', 'adjusted_DETP_log', 'adjusted_DMTP_log', 'adjusted_TCP_log', 
                                      'adjusted_PNP_log', 'adjusted_DEDP_log', 'adjusted_DMDP_log', 'adjusted_PBA_log', 'adjusted_TRANS_DCCA_log', 'adjusted_CIS_DCCA_log'),
                           labels = c('DEP', 'DMP', 'DETP', 'DMTP', 'TCP', 'PNP', 'DEDP', 'DMDP', 'PBA', 'TRANS DCCA', 'CIS DCCA'))

figures9_data$term<- factor(figures9_data$term,
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

figures9_data$covariates<- factor(figures9_data$term)

figures9_data$Group<- factor(figures9_data$Group,
                            levels = c("OP pesticides", "PYR metabolites"),
                            labels = c("OP pesticides", "Pyrethroids"))


figures9_data$Timepoint<- factor(figures9_data$Timepoint,
                                 levels = c("Preconception", "Pregnancy"))


figures9_data$conf.low<- as.numeric(figures9_data$conf.low)
figures9_data$conf.high<- as.numeric(figures9_data$conf.high)


figures9_2<- ggplot(figures9_data,aes(y=term, shape=fct_rev(Timepoint))) +
              geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = Group), width=0.1,size=1, position = position_dodge(width = 0.5))+
              geom_point(size=2.2,aes(x=Estimate), position = position_dodge(width = 0.5), color = "black") +
              geom_vline(aes(xintercept=0),linetype="dashed",size=0.5) + 
              scale_shape_manual(drop = FALSE,
                                 values = c(15, 17),
                                 labels = c("Preconception", "Pregnancy"),
                                 breaks = c("Preconception", "Pregnancy"))+
              scale_color_manual(drop = FALSE,
                                 values = c("#C52A20", "#8E549E"),
                                 labels = c("OP pesticides", "Pyrethroids"))+
              facet_grid(covariates~pest, scale="free", space = "free_y")+
              xlab(expression(beta ~ (`95% CI`)))+
              labs(shape = "Timepoint")+
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


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/supp_figures9.jpeg",
     units="in", width=22, height=10, res=600)


figures9_2


dev.off()
