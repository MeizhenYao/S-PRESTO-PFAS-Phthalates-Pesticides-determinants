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
library(ggpmisc)
library(patchwork)
library(gridExtra)

#------------------------------------------------------Plot
#-----------------------------------------PFAS

# import data (change the path to the location where you save the dataset)
PFAS<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")

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
  dplyr::summarise(mean = mean(value))


## combine into one long format data
PFAS_long<- PFAS_long %>% 
            inner_join(PFAS_inform, by = c("Chemicals")) %>% 
            inner_join(PFAS_hjust, by = c("Chemicals"))



PFAS_long$Chemicals<- factor(PFAS_long$Chemicals,
                            levels=c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'),
                            labels=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'))


# put in plot
p1 <- (ggplot(PFAS_long, aes(x=Chemicals, y=value)) + 
        stat_summary(fun ="mean", geom="bar", width = 0.7, position=position_dodge(), fill="#2E5FA1", size=1)+
        stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width=.2, position=position_dodge(.9), size=0.8)+
        scale_x_discrete(limits = rev(levels(PFAS_long$Chemicals)))+
        labs(x ="",y = "Mean Concentrations",
             title = "PFAS") +  
        coord_flip() + 
        theme_bw())

pm1 = p1 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
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
                                                                                                                        


pm1





#-----------------------------------------Phthalates
Phthalates<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_long_20230317.xlsx")
Phthalates_wide<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")

## info
phtha_LOD<- c("100.0%", "100.0%", "90.8%", "88.2%", "81.6%", "86.8%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "98.7%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "71.1%", "61.8%", "0.0%", "3.9%", "100.0%", "92.1%", "100.0%", "97.4%", "100.0%", "100.0%")
phtha_name<- c('adjusted_MEP','adjusted_MEP','adjusted_MCPP','adjusted_MCPP','adjusted_MBZP','adjusted_MBZP','adjusted_MBP','adjusted_MBP','adjusted_MIBP','adjusted_MIBP','adjusted_MEHP','adjusted_MEHP','adjusted_MEOHP','adjusted_MEOHP','adjusted_MEHHP','adjusted_MEHHP','adjusted_MECPP','adjusted_MECPP','adjusted_MCIOP','adjusted_MCIOP','adjusted_MCINP','adjusted_MCINP','adjusted_MEHTP','adjusted_MEHTP','adjusted_MEOHTP','adjusted_MEOHTP','adjusted_MEHHTP','adjusted_MEHHTP','adjusted_MECPTP','adjusted_MECPTP')
phtha_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")
phtha_parent<- c("Diethyl\nphthalate", "Diethyl\nphthalate", "Di-n-butyl\nphthalate", "Di-n-butyl\nphthalate", "Butylbenzyl\nphthalate", "Butylbenzyl\nphthalate", "Di-n-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-iso-nonyl\nphthalate", "Di-iso-nonyl\nphthalate", "Di-isodecyl\nphthalate", "Di-isodecyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate","Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate")
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
  dplyr::summarise(mean = mean(value))


## combine into one long format data
phtha_long<- phtha_long %>% 
  inner_join(phtha_inform, by = c("Chemicals", "Timepoint")) %>% 
  inner_join(phtha_hjust,  by = c("Chemicals", "Timepoint"))




phtha_long$Chemicals<- factor(phtha_long$Chemicals,
                              levels = c('adjusted_MEP','adjusted_MCPP','adjusted_MBZP','adjusted_MBP','adjusted_MIBP','adjusted_MEHP','adjusted_MEOHP','adjusted_MEHHP','adjusted_MECPP','adjusted_MCIOP','adjusted_MCINP','adjusted_MEHTP','adjusted_MEOHTP','adjusted_MEHHTP','adjusted_MECPTP'),
                              labels = c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'))

phtha_long$Group<- factor(phtha_long$Group,
                           levels = c("PFAS", "LMWPs", "HMWPs", "PYR metabolites", "OC pesticides", "OP pesticides"))

phtha_long$Parent<- factor(phtha_long$Parent,
                          levels = c("Diethyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-iso-nonyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Butylbenzyl\nphthalate", "Di-isodecyl\nphthalate"))

# put in plot
p2 <- (ggplot(phtha_long, aes(y=reorder(Chemicals, mean), x=value,fill=Group, alpha=Timepoint)) + 
         stat_summary(fun ="mean", geom="bar", width = 0.7, position=position_dodge(), size=1)+
         stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width=.2, position=position_dodge(.8), size=0.8)+
         scale_fill_manual(drop = FALSE,
                           values = c("#2E5FA1", "#E3882F", "#1B7C3D",  "#8E549E", "#C52A20", "#545454"),
                           labels = c("PFAS", "LMWPs", "HMWPs" ,"PYR metabolites", "OC pesticides", "OP pesticides"))+
         scale_alpha_manual(values = c(0.6, 1), labels = c("Preconception", "Pregnancy"),limits = rev(levels(phtha_long$Timepoint)))+
         labs(y ="",x = "Mean Concentrations",
              title = "Phthalates") + 
         facet_grid(Parent ~ ., scales = "free_y", space = "free")+
         theme_bw())

pm2 = p2 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
                 axis.text.y = element_text(size = 9,face = "bold"),
                 legend.text = element_text(size = 9,face = "bold"),
                 legend.title = element_text(size = 10,face = "bold"),
                 plot.title = element_text(size = 10,face = "bold"),
                 axis.title=element_text(size=12,face="bold"),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 9),
                 panel.spacing.y=unit(0.3, "lines"))
            


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
  dplyr::summarise(mean = mean(value))


## combine into one long format data
pest_long<- pest_long %>% 
  inner_join(pest_inform, by = c("Chemicals", "Timepoint")) %>% 
  inner_join(pest_hjust,  by = c("Chemicals", "Timepoint"))




pest_long$Chemicals<- factor(pest_long$Chemicals,
                              levels = c('adjusted_PBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PNP','adjusted_TCP','adjusted_DMP','adjusted_DMTP','adjusted_DMDP','adjusted_DEP','adjusted_DETP','adjusted_DEDP'),
                              labels = c('PBA','FPBA','CIS DCCA','TRANS DCCA','PCP','PNP','TCP','DMP','DMTP','DMDP','DEP','DETP','DEDP'))


pest_long$Group<- factor(pest_long$Group,
                          levels = c("PFAS", "LMWPs", "HMWPs", "PYR metabolites", "OC pesticides", "OP pesticides"))

pest_long$Timepoint<- factor(pest_long$Timepoint,
                         levels = c("pcv2", "pgv3"))


# put in plot
p3 <- (ggplot(pest_long, aes(y=reorder(Chemicals, mean), x=value,fill=Group, alpha=Timepoint)) + 
         stat_summary(fun ="mean", geom="bar", width = 0.7, position=position_dodge(), size=1)+
         stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width=.2, position=position_dodge(.8), size=0.8)+
         scale_fill_manual(drop = FALSE,
                           values = c("#2E5FA1", "#E3882F", "#1B7C3D",  "#8E549E", "#545454", "#C52A20"),
                           labels = c("PFAS", "LMWPs", "HMWPs" ,"PYR metabolites", "OC pesticides", "OP pesticides"))+
         scale_alpha_manual(values = c(0.6, 1), labels = c("Preconception", "Pregnancy"),limits = rev(levels(pest_long$Timepoint)))+
         labs(y ="",x = "Mean Concentrations",
              title = "Pesticides") +
         theme_bw())

pm3 = p3 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
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




pm3


#------------------------------------------------------Table
## LOD
### PFAS
PFAS_name<- c('PFOS Total', 'PFOA Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS')
PFAS_LOD<- c("NA", "97.4%", "60.7%", "100.0%", "41.4%", "33.9%", "48.4%", "0.3%", "0.0%", "1.6%", "1.3%", "0.5%", "0.0%", "0.0%", "0.0%")

PFAS_inform<- data.frame(PFAS = PFAS_name, 
                         `(>LOD)%` = PFAS_LOD)

PFAS_NA_info<- data.frame(PFAS = rep("", 15),
                          `(>LOD)%` = rep("", 15))
PFAS_inform<- rbind(PFAS_inform,
                    PFAS_NA_info)

### phtha
phtha_LOD<- c("100.0%", "100.0%", "90.8%", "88.2%", "81.6%", "86.8%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "98.7%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "71.1%", "61.8%", "0.0%", "3.9%", "100.0%", "92.1%", "100.0%", "97.4%", "100.0%", "100.0%")
phtha_name<- c(rep('MEP',2),rep('MCPP',2),rep('MBZP',2),rep('MBP',2),rep('MIBP',2),rep('MEHP',2),rep('MEOHP',2),rep('MEHHP',2),rep('MECPP',2),rep('MCIOP',2),rep('MCINP',2),rep('MEHTP',2),rep('MEOHTP',2),rep('MEHHTP',2),rep('MECPTP',2))
phtha_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")

phtha_inform <- data.frame(Phthalates = phtha_name,
                           Timepoint = phtha_time,
                           `(>LOD)%` = phtha_LOD) 

phtha_inform$Phthalates<- factor(phtha_inform$Phthalates,
                                levels = c('MEP','MIBP','MBP','MCPP','MECPTP','MEHHTP','MEOHTP','MEHTP','MECPP','MEHHP','MEOHP','MEHP','MCIOP','MBZP','MCINP'))
phtha_inform<- phtha_inform %>% 
               arrange(Phthalates)


### pest
pest_LOD<- c('99.0%','96.2%','10.2%','5.1%','82.7%','80.8%','93.9%','92.3%','18.4%','21.8%','79.6%','91.0%','96.9%','94.9%','98.0%','97.4%','90.8%','88.5%','48.0%','57.7%','99.0%','100.0%','100.0%','100.0%','58.2%','79.5%')
pest_name<- c(rep('PBA',2), rep('FPBA',2),rep('CIS DCCA',2),rep('TRANS DCCA',2),rep('PCP',2),rep('PNP',2),rep('TCP',2),rep('DMP',2),rep('DMTP',2),rep('DMDP',2),rep('DEP',2),rep('DETP',2),rep('DEDP',2))
pest_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")


pest_inform <- data.frame(Pesticides = pest_name,
                          Timepoint = pest_time,
                          `(>LOD)%` = pest_LOD) 

pest_inform$Pesticides<- factor(pest_inform$Pesticides,
                               levels = c("DEP", "DMP", "DETP", "DMTP", "TCP", "PNP", "DEDP", "DMDP", "PBA", "TRANS DCCA", "CIS DCCA", "PCP", "FPBA"))

pest_NA_info<- data.frame(Pesticides = rep("", 4),
                          Timepoint = rep("", 4),
                          `(>LOD)%` = rep("", 4))

pest_inform<- rbind(pest_inform,
                    pest_NA_info)%>% 
                    arrange(Pesticides)

### combine into one table
table_content<- cbind(PFAS_inform,
                      phtha_inform,
                      pest_inform)
colnames(table_content)[2]<- "(>LOD)%"
colnames(table_content)[5]<- "(>LOD)%"
colnames(table_content)[8]<- "(>LOD)%"


ggp_table <- tableGrob(table_content)
  
ggp_table$widths <- unit(rep(1, ncol(ggp_table)), "null")
ggp_table$heights <- unit(rep(1, nrow(ggp_table)), "null")

table<- ggplot() +
        annotation_custom(ggp_table) + 
        theme()


jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure1.jpeg",
     units="in", width=18, height=24, res=500)

plot<- ggarrange(pm1, pm2, pm3,
          ncol = 3, nrow = 1,
          common.legend = TRUE)+
  theme(legend.position = "top")


plot + table + plot_layout(ncol = 1)

dev.off()

