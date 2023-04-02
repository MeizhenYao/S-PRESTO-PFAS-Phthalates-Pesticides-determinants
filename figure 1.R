# load packages
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

#-----------------------------------------PFAS

# import data (change the path to the location where you save the dataset)
PFAS<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")

## info
Chemicals<- c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS')
LOD<- c("NA", "97.4%", "60.7%", "100.0%", "41.4%", "33.9%", "48.4%", "0.3%", "0.0%", "1.6%", "1.3%", "0.5%", "0.0%", "0.0%", "0.0%")

PFAS_inform<- data.frame(LOD, Chemicals)

## data in long format
PFAS_long<- PFAS %>% 
  pivot_longer(cols=c(PFOS_Total, PFOA_Linear, PFNA, PFHxS, PFDA, PFHpA, PFHpS, PFBS, NEtFOSAA, NMeFOSAA, PAP, diPAP, FTS, PFOSA, PFDS),
               names_to = "Chemicals",
               values_to="value")
## calculate mean
PFAS_hjust<- PFAS_long %>% 
  group_by(Chemicals) %>% 
  dplyr::summarise(mean = mean(log2(value)))


## combine into one long format data
PFAS_long<- PFAS_long %>% 
            inner_join(PFAS_inform, by = c("Chemicals")) %>% 
            inner_join(PFAS_hjust, by = c("Chemicals"))



PFAS_long$Chemicals<- factor(PFAS_long$Chemicals,
                            levels=c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'),
                            labels=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'))


# put in plot
p1 <- (ggplot(PFAS_long, aes(x=Chemicals, y=log2(value))) + 
        stat_summary(fun ="mean", geom="bar", width = 0.7, position=position_dodge(), fill="#2E5FA1", size=1)+
        stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width=.2, position=position_dodge(.9), size=0.8)+
        scale_x_discrete(limits = rev(levels(PFAS_long$Chemicals)))+
        geom_text(aes(x=Chemicals,y=0, label = LOD, hjust= ifelse(mean <0, "bottom", "top")), size=3) +
        labs(x ="",y = "Mean (log2 Concentrations) w/ (>LOD)%",
             title = "PFAS") +  
        coord_flip() + 
        theme_bw())

pm1 = p1 + theme(
               plot.title = element_text(size = 10,face = "bold"),
               axis.text.x= element_text(face="bold", angle = 0, size = 9),
               axis.text.y = element_text(size = 9,face = "bold"),
               axis.title=element_text(size=10,face="bold"),plot.margin = unit(c(0,0,0,0), "lines"))
                                                                                                                        


pm1





#-----------------------------------------Phthalates
Phthalates<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_long_20230317.xlsx")
Phthalates_wide<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")

## info
phtha_LOD<- c("100.0%", "100.0%", "90.8%", "88.2%", "81.6%", "86.8%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "98.7%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "71.1%", "61.8%", "0.0%", "3.9%", "100.0%", "92.1%", "100.0%", "97.4%", "100.0%", "100.0%")
phtha_name<- c('adjusted_MEP','adjusted_MEP','adjusted_MCPP','adjusted_MCPP','adjusted_MBZP','adjusted_MBZP','adjusted_MBP','adjusted_MBP','adjusted_MIBP','adjusted_MIBP','adjusted_MEHP','adjusted_MEHP','adjusted_MEOHP','adjusted_MEOHP','adjusted_MEHHP','adjusted_MEHHP','adjusted_MECPP','adjusted_MECPP','adjusted_MCIOP','adjusted_MCIOP','adjusted_MCINP','adjusted_MCINP','adjusted_MEHTP','adjusted_MEHTP','adjusted_MEOHTP','adjusted_MEOHTP','adjusted_MEHHTP','adjusted_MEHHTP','adjusted_MECPTP','adjusted_MECPTP')
phtha_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")
phtha_parent<- c("DEP", "DEP", "DBP", "DBP", "BBzP", "BBzP", "DBP", "DBP", "DiBP", "DiBP", "DEHP", "DEHP", "DEHP", "DEHP", "DEHP", "DEHP", "DEHP", "DEHP", "DiNP", "DiNP", "DiDP", "DiDP", "DEHTP", "DEHTP", "DEHTP", "DEHTP", "DEHTP", "DEHTP","DEHTP", "DEHTP")
phtha_group<- c("LMWPs", "LMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "LMWPs", "LMWPs", "LMWPs", "LMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs")

phtha_inform <- data.frame(Chemicals = phtha_name,
                        LOD = phtha_LOD,
                        Timepoint = phtha_time,
                        Parent = phtha_parent,
                        Group = phtha_group) %>% 
             arrange(by_group=Parent)

## data in long format
phtha_long<- Phthalates %>% 
  pivot_longer(cols=c(adjusted_MEP,adjusted_MCPP,adjusted_MBZP,adjusted_MBP,adjusted_MIBP,adjusted_MEHP,adjusted_MEOHP,adjusted_MEHHP,adjusted_MECPP,adjusted_MCIOP,adjusted_MCINP,adjusted_MEHTP,adjusted_MEOHTP,adjusted_MEHHTP,adjusted_MECPTP),
               names_to = "Chemicals",
               values_to="value")

## calculate mean
phtha_hjust<- phtha_long %>% 
  group_by(Chemicals, Timepoint) %>% 
  dplyr::summarise(mean = mean(log2(value)))


## combine into one long format data
phtha_long<- phtha_long %>% 
  inner_join(phtha_inform, by = c("Chemicals", "Timepoint")) %>% 
  inner_join(phtha_hjust,  by = c("Chemicals", "Timepoint"))




phtha_long$Chemicals<- factor(phtha_long$Chemicals,
                              levels = c('adjusted_MEP','adjusted_MCPP','adjusted_MBZP','adjusted_MBP','adjusted_MIBP','adjusted_MEHP','adjusted_MEOHP','adjusted_MEHHP','adjusted_MECPP','adjusted_MCIOP','adjusted_MCINP','adjusted_MEHTP','adjusted_MEOHTP','adjusted_MEHHTP','adjusted_MECPTP'),
                              labels = c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'))

phtha_long$Group<- factor(phtha_long$Group,
                           levels = c("LMWPs", "HMWPs", "PYR metabolites", "OC pesticides", "OP pesticides"))

phtha_long$Parent<- factor(phtha_long$Parent,
                          levels = c("DEP", "DiBP", "DBP", "DEHTP", "DEHP", "DiNP", "BBzP", "DiDP"))

# put in plot
p2 <- (ggplot(phtha_long, aes(y=reorder(Chemicals, mean), x=log2(value),fill=Group, alpha=Timepoint)) + 
         stat_summary(fun ="mean", geom="bar", width = 0.7, position=position_dodge(), size=1)+
         stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width=.2, position=position_dodge(.8), size=0.8)+
         geom_text(data=phtha_long, aes(y=Chemicals, x=0, label = LOD, alpha=Timepoint, hjust= ifelse(mean <0, 0.2, "top")), position=position_dodge(.8), size=3, show.legend=FALSE) +
         scale_x_continuous(expand = c(0.2, 0.001))+
         scale_fill_manual(drop = FALSE,
                           values = c( "#AA4414", "#41553A",  "#6E3F2B", "#EDCAA0", "#B3686F"),
                           labels = c("LMWPs", "HMWPs" ,"PYR metabolites", "OC pesticides", "OP pesticides"))+
         scale_alpha_manual(values = c(0.6, 1), labels = c("Preconception", "Pregnancy"),limits = rev(levels(pest_long$Timepoint)))+
         labs(y ="",x = "Mean (log2 Concentrations) w/ (>LOD)%",
              title = "Phthalates") + 
         facet_grid(Parent ~ ., scales = "free_y", space = "free")+
         theme_bw())

pm2 = p2 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
                 axis.text.y = element_text(size = 9,face = "bold"),
                 legend.text = element_text(size = 9,face = "bold"),
                 legend.title = element_text(size = 10,face = "bold"),
                 plot.title = element_text(size = 10,face = "bold"),
                 axis.title=element_text(size=12,face="bold"),
                 plot.margin = unit(c(0,1,0,0), "lines"),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 10),
                 panel.spacing.y=unit(0.1, "lines"))
            



pm2





#-----------------------------------------Pesticides
Pesticides<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_long_20230317.xlsx")

pest_LOD<- c('99.0%','96.2%','10.2%','5.1%','82.7%','80.8%','93.9%','92.3%','18.4%','21.8%','79.6%','91.0%','96.9%','94.9%','98.0%','97.4%','90.8%','88.5%','48.0%','57.7%','99.0%','100.0%','100.0%','100.0%','58.2%','79.5%')
pest_name<- c('adjusted_PBA','adjusted_PBA','adjusted_FPBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PCP','adjusted_PNP','adjusted_PNP','adjusted_TCP','adjusted_TCP','adjusted_DMP','adjusted_DMP','adjusted_DMTP','adjusted_DMTP','adjusted_DMDP','adjusted_DMDP','adjusted_DEP','adjusted_DEP','adjusted_DETP','adjusted_DETP','adjusted_DEDP','adjusted_DEDP')
pest_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")
pest_group<- c("PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites","PYR metabolites", "PYR metabolites", "OC pesticides", "OC pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides")

## info
pest_inform <- data.frame(Chemicals = pest_name,
                           LOD = pest_LOD,
                           Timepoint = pest_time,
                           Group = pest_group) %>% 
                arrange(by_group=pest_group)

## data in long format
pest_long<- Pesticides %>% 
  pivot_longer(cols=c(adjusted_PBA,adjusted_FPBA,adjusted_CIS_DCCA,adjusted_TRANS_DCCA,adjusted_PCP,adjusted_PNP,adjusted_TCP,adjusted_DMP,adjusted_DMTP,adjusted_DMDP,adjusted_DEP,adjusted_DETP,adjusted_DEDP),
               names_to = "Chemicals",
               values_to="value")

## calculate mean
pest_hjust<- pest_long %>% 
  group_by(Chemicals, Timepoint) %>% 
  dplyr::summarise(mean = mean(log2(value)))


## combine into one long format data
pest_long<- pest_long %>% 
  inner_join(pest_inform, by = c("Chemicals", "Timepoint")) %>% 
  inner_join(pest_hjust,  by = c("Chemicals", "Timepoint"))




pest_long$Chemicals<- factor(pest_long$Chemicals,
                              levels = c('adjusted_PBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PNP','adjusted_TCP','adjusted_DMP','adjusted_DMTP','adjusted_DMDP','adjusted_DEP','adjusted_DETP','adjusted_DEDP'),
                              labels = c('PBA','FPBA','CIS DCCA','TRANS DCCA','PCP','PNP','TCP','DMP','DMTP','DMDP','DEP','DETP','DEDP'))


pest_long$Group<- factor(pest_long$Group,
                          levels = c("LMWPs", "HMWPs", "PYR metabolites", "OC pesticides", "OP pesticides"))

pest_long$Timepoint<- factor(pest_long$Timepoint,
                         levels = c("pcv2", "pgv3"))


# put in plot
p3 <- (ggplot(pest_long, aes(y=reorder(Chemicals, mean), x=log2(value),fill=Group, alpha=Timepoint)) + 
         stat_summary(fun ="mean", geom="bar", width = 0.7, position=position_dodge(), size=1)+
         stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width=.2, position=position_dodge(.8), size=0.8)+
         geom_text(aes(y=Chemicals, x=0, label = LOD, alpha=Timepoint, hjust= ifelse(mean <0, "bottom", "top")), position=position_dodge(.8), size=3, show.legend=FALSE) +
         scale_fill_manual(drop = FALSE,
                           values = c( "#AA4414", "#41553A",  "#6E3F2B", "#EDCAA0", "#B3686F"),
                           labels = c("LMWPs", "HMWPs" ,"PYR metabolites", "OC pesticides", "OP pesticides"))+
         scale_alpha_manual(values = c(0.6, 1), labels = c("Preconception", "Pregnancy"),limits = rev(levels(pest_long$Timepoint)))+
         labs(y ="",x = "Mean (log2 Concentrations) w/ (>LOD)%",
              title = "Pesticides") +
         theme_bw())

pm3 = p3 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
                 axis.text.y = element_text(size = 9,face = "bold"),
                 legend.text = element_text(size = 9,face = "bold"),
                 legend.title = element_text(size = 10,face = "bold"),
                 plot.title = element_text(size = 10,face = "bold"),
                 axis.title=element_text(size=12,face="bold"),plot.margin = unit(c(0,0,0,0), "lines"))




pm3


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure1.jpeg",
     units="in", width=18, height=14, res=500)

ggarrange(pm1, pm2, pm3,
          ncol = 3, nrow = 1,
          common.legend = TRUE,
          align = "hv")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.position = "bottom")

dev.off()

