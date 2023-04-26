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
figure4_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/pest_model1.csv")

#------------------------------------------------------------reformat variables
figure4_data$PEST<- factor(figure4_data$PEST,
                            levels = c("DEP", "DMP", "DETP","DMTP", "TCP", "PNP", "TRANS DCCA", "PBA", "DEDP", "DMDP", "CIS DCCA", "PCP", "FPBA"))

figure4_data$covariates<- factor(figure4_data$covariates,
                                 levels = c(
                                   "Ethnicity", 
                                   "Recruitment Age",
                                   "Highest Education ",
                                   "Household Income",
                                   "Parity"))

figure4_data$term<- factor(figure4_data$term,
                           levels = c('Chinese',
                                      'Indian/Malay',
                                      'First tertile',
                                      'Second tertile',
                                      'Third tertile',
                                      'University',
                                      'Primary/Secondary/Post Secondary',
                                      '$6,376 and below',
                                      '$6,377 - $11,293',
                                      '$11,294 and above',
                                      '0 previous births',
                                      '>=1 previous births'
                           ))

figure4_data$Group<- factor(figure4_data$Group,
                            levels = c("PYR metabolites", "OP pesticides"))



figure4_data$conf.low<- as.numeric(figure4_data$conf.low)
figure4_data$conf.high<- as.numeric(figure4_data$conf.high)

figure4_1<- ggplot(figure4_data,aes(y=term)) +
  geom_errorbar(aes(xmin = exp(conf.low), xmax = exp(conf.high), color = Group), width=0.1,size=1)+
  geom_point(size=1.8,aes(x=exp(Estimate))) +
  geom_vline(aes(xintercept=1),linetype="dashed",size=0.3)+ 
  scale_color_manual(drop = FALSE,
                     values = c( "#8E549E", "#C52A20"),
                     labels = c("PYR metabolites", "OP pesticides"))+
  facet_nested(covariates~PEST, scale="free", space = "free_y", nest_line = element_line(colour = "blue"))+
  xlab(expression(beta ~ (`95% CI`)))+
  ylab("Covariates")+
  theme_bw()+
  theme(panel.spacing.y=unit(0, "line"),
        axis.title=element_text(face="bold"),
        legend.text = element_text(size = 9,face = "bold"),
        legend.title = element_text(size = 10,face = "bold"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=10),
        strip.text.y = element_text(colour = "black", face = "bold",size=10,angle=0))   


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure4_1.jpeg",
     units="in", width=18, height=12, res=500)


figure4_1

dev.off()
