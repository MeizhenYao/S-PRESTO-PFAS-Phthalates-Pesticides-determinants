
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
figure2_data$PFAS<- factor(figure2_data$PFAS,
                           levels = c('PFOS', 'PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS'))

figure2_data$covariates<- factor(figure2_data$term,
                                 levels = c("Parity",
                                            "Total Fish Intake", 
                                            "Fast food Intake",
                                            "Fruit Intake",
                                            "Total Vegetable Intake",
                                            "Styrofoam Boxes Usage",
                                            "Plastic Boxes Usage ",
                                            "Occupation"))


figure2_data$level<- factor(figure2_data$level,
                             levels = c("0",
                                        ">= 1",
                                        'No',
                                        'Yes',
                                        "Not working",
                                        "Office worker",
                                        "Health care worker",
                                        "Service worker & Outdoor worker",
                                        ""
                             ))


figure2_data$conf.low<- as.numeric(figure2_data$conf.low)
figure2_data$conf.high<- as.numeric(figure2_data$conf.high)

## figure 2 part 1
figure2_data_1<- figure2_data %>% 
  filter(PFAS %in% c('PFOS', 'PFOA', 'PFNA', 'PFHxS'))

figure2_1<- ggplot(figure2_data_1,aes(y=level)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width=0.1,size=1, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=Estimate)) +
  geom_vline(aes(xintercept=0),linetype="dashed",size=0.5)+
  facet_grid(covariates~PFAS, scale="free", space = "free_y")+
  xlab(expression(beta ~ (`95% CI`)))+
  ylab("Covariates")+
  theme_bw()+
  theme(panel.spacing.y=unit(0.2, "line"),
        axis.title=element_text(face="bold"),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=10),
        strip.text.y = element_blank())   

figure2_1


## figure 2 part 2
figure2_data_2<- figure2_data %>% 
  filter(PFAS %in% c('PFDA', 'PFHpA', 'PFHpS'))

figure2_2<- ggplot(figure2_data_2,aes(y=level)) +
  geom_errorbar(aes(xmin = exp(conf.low), xmax = exp(conf.high)), width=0.1,size=1, color = "#2E5FA1")+
  geom_point(size=1.8,aes(x=exp(Estimate))) +
  geom_vline(aes(xintercept=1),linetype="dashed",size=0.5)+ 
  facet_grid(covariates~PFAS, scale="free", space = "free_y")+
  scale_x_continuous(trans='log2') +
  xlab("OR (95% CI)")+
  ylab("")+
  theme_bw()+
  theme(panel.spacing.y=unit(0.2, "line"),
        axis.title=element_text(face="bold"),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(colour = "black", face = "bold",size=10),
        strip.text.y = element_text(colour = "black", face = "bold",size=10,angle=0))   

figure2_2




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure3.1.jpeg",
     units="in", width=18, height=12, res=500)


ggarrange(figure2_1, figure2_2,
          widths = c(4,3),
          heights = c(1,1))

dev.off()

