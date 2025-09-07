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
figures8_data<-  read.csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/plot_data_input/pest_model1_ind.csv")

#------------------------------------------------------------reformat variables
figures8_data$pest<- factor(figures8_data$pest,
                           levels = c('adjusted_DEP_log', 'adjusted_DMP_log', 'adjusted_DETP_log', 'adjusted_DMTP_log', 'adjusted_TCP_log', 
                                      'adjusted_PNP_log', 'adjusted_DEDP_log', 'adjusted_DMDP_log', 'adjusted_PBA_log', 'adjusted_TRANS_DCCA_log', 'adjusted_CIS_DCCA_log'),
                           labels = c('DEP', 'DMP', 'DETP', 'DMTP', 'TCP', 'PNP', 'DEDP', 'DMDP', 'PBA', 'TRANS DCCA', 'CIS DCCA'))

figures8_data$covariates<- factor(figures8_data$covariates,
                                 levels = c(
                                   "Ethnicity", 
                                   "Age",
                                   "Education",
                                   "Income",
                                   "Occupation",
                                   "Parity"))


figures8_data$term<- factor(figures8_data$term,
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


figures8_data$Group<- factor(figures8_data$Group,
                            levels = c("OP pesticides", "PYR metabolites"),
                            labels = c("OP pesticides", "Pyrethroids"))


figures8_data$Timepoint<- factor(figures8_data$Timepoint,
                                 levels = c("Preconception", "Pregnancy"))


figures8_data$conf.low<- as.numeric(figures8_data$conf.low)
figures8_data$conf.high<- as.numeric(figures8_data$conf.high)

figures8_1<- ggplot(figures8_data,aes(y=term, shape=fct_rev(Timepoint))) +
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
              facet_nested(covariates~pest, scale="free", space = "free_y", nest_line = element_line(colour = "blue"))+
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


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/supp_figure8.jpeg",
     units="in", width=22, height=10, res=500)


figures8_1

dev.off()
