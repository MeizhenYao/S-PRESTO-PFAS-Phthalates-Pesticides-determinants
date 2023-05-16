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


## data in long format
PFAS_long<- PFAS %>% 
  pivot_longer(cols=c(PFOS_Total, PFOA_Linear, PFNA, PFHxS, PFDA, PFHpA, PFHpS, PFBS, NEtFOSAA, NMeFOSAA, PAP, diPAP, FTS, PFOSA, PFDS),
               names_to = "Chemicals",
               values_to="value")

PFAS_long$Chemicals<- factor(PFAS_long$Chemicals,
                             levels=c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'),
                             labels=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'))


# put in plot
p1 <- (ggplot(PFAS_long, aes(x=Chemicals, y=log2(value))) + 
         geom_boxplot(color="#2E5FA1", size=0.8, fill="#FDFEFE")+
         scale_x_discrete(limits = rev(levels(PFAS_long$Chemicals)))+
         labs(x ="",y = "log2(Concentrations(ng/ml))",
              title = "PFAS") +  
         coord_flip()+
         theme_classic())


pm1 = p1 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
                 axis.text.y = element_text(size = 9,face = "bold"),
                 legend.text = element_text(size = 9,face = "bold"),
                 legend.title = element_text(size = 10,face = "bold"),
                 plot.title = element_text(size = 10,face = "bold"),
                 axis.title=element_text(size=12,face="bold"),
                 panel.grid.major = element_line(color = '#FDFEFE', size = 0.8),
                 panel.grid.minor = element_line(color = '#FDFEFE', size = 0.6, linetype = "dotted"),
                 panel.background = element_rect(fill = '#F4F6F7'),
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
phtha_name<- c('adjusted_MEP','adjusted_MEP','adjusted_MCPP','adjusted_MCPP','adjusted_MBZP','adjusted_MBZP','adjusted_MBP','adjusted_MBP','adjusted_MIBP','adjusted_MIBP','adjusted_MEHP','adjusted_MEHP','adjusted_MEOHP','adjusted_MEOHP','adjusted_MEHHP','adjusted_MEHHP','adjusted_MECPP','adjusted_MECPP','adjusted_MCIOP','adjusted_MCIOP','adjusted_MCINP','adjusted_MCINP','adjusted_MEHTP','adjusted_MEHTP','adjusted_MEOHTP','adjusted_MEOHTP','adjusted_MEHHTP','adjusted_MEHHTP','adjusted_MECPTP','adjusted_MECPTP')
phtha_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")
phtha_parent<- c("Diethyl\nphthalate", "Diethyl\nphthalate", "Di-n-butyl\nphthalate", "Di-n-butyl\nphthalate", "Butylbenzyl\nphthalate", "Butylbenzyl\nphthalate", "Di-n-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-iso-nonyl\nphthalate", "Di-iso-nonyl\nphthalate", "Di-isodecyl\nphthalate", "Di-isodecyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate","Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate")
phtha_group<- c("LMWPs", "LMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "LMWPs", "LMWPs", "LMWPs", "LMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs")

phtha_inform <- data.frame(Chemicals = phtha_name,
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
  dplyr::summarise(median = median(value))


## combine into one long format data
phtha_long<- phtha_long %>% 
  inner_join(phtha_inform, by = c("Chemicals", "Timepoint")) %>% 
  inner_join(phtha_hjust,  by = c("Chemicals", "Timepoint"))

phtha_long$Timepoint<- factor(phtha_long$Timepoint,
                             levels = c("pcv2", "pgv3"))

phtha_long$Chemicals<- factor(phtha_long$Chemicals,
                              levels = c('adjusted_MEP','adjusted_MCPP','adjusted_MBZP','adjusted_MBP','adjusted_MIBP','adjusted_MEHP','adjusted_MEOHP','adjusted_MEHHP','adjusted_MECPP','adjusted_MCIOP','adjusted_MCINP','adjusted_MEHTP','adjusted_MEOHTP','adjusted_MEHHTP','adjusted_MECPTP'),
                              labels = c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'))

phtha_long$Group<- factor(phtha_long$Group,
                          levels = c("PFAS", "LMWPs", "HMWPs", "OP pesticides", "PYR metabolites", "OC pesticides"))

phtha_long$Parent<- factor(phtha_long$Parent,
                           levels = c("Diethyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-iso-nonyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Butylbenzyl\nphthalate", "Di-isodecyl\nphthalate"))

# put in plot
p2 <- (ggplot(phtha_long, aes(y=reorder(Chemicals, median), x=log2(value),color=Group, fill=Timepoint)) + 
         geom_boxplot(size=0.8)+
         scale_color_manual(drop = FALSE,
                            values = c("#2E5FA1",  "#E3882F", "#1B7C3D", "#C52A20" , "#8E549E", "#545454"),
                            labels = c("PFAS", "LMWPs", "HMWPs" , "OP pesticides", "PYR metabolites", "OC pesticides"))+
         scale_fill_manual(drop = FALSE,
                           values = c( "#FDFEFE", "#909497"),
                           labels = c("Preconception", "Pregnancy"),
                           limits = rev(levels(phtha_long$Timepoint)))+
         labs(y ="",x = "log2(Concentrations(µg/g))",
              title = "Phthalates") + 
         facet_grid(Parent ~ ., scales = "free_y", space = "free")+
         theme_classic())

pm2 = p2 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
                 axis.text.y = element_text(size = 9,face = "bold"),
                 legend.text = element_text(size = 9,face = "bold"),
                 legend.title = element_text(size = 10,face = "bold"),
                 plot.title = element_text(size = 10,face = "bold"),
                 axis.title=element_text(size=12,face="bold"),
                 panel.grid.major = element_line(color = '#FDFEFE', size = 0.8),
                 panel.grid.minor = element_line(color = '#FDFEFE', size = 0.6, linetype = "dotted"),
                 panel.background = element_rect(fill = '#F4F6F7'),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 9),
                 panel.spacing.y=unit(0.3, "lines"))



pm2


#-----------------------------------------Pesticides
Pesticides<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_long_20230317.xlsx")

pest_name<- c('adjusted_PBA','adjusted_PBA','adjusted_FPBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PCP','adjusted_PNP','adjusted_PNP','adjusted_TCP','adjusted_TCP','adjusted_DMP','adjusted_DMP','adjusted_DMTP','adjusted_DMTP','adjusted_DMDP','adjusted_DMDP','adjusted_DEP','adjusted_DEP','adjusted_DETP','adjusted_DETP','adjusted_DEDP','adjusted_DEDP')
pest_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")
pest_group<- c("PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites","PYR metabolites", "PYR metabolites", "OC pesticides", "OC pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides")

## info
pest_inform <- data.frame(Chemicals = pest_name,
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
  dplyr::summarise(median = median(value))


## combine into one long format data
pest_long<- pest_long %>% 
  inner_join(pest_inform, by = c("Chemicals", "Timepoint")) %>% 
  inner_join(pest_hjust,  by = c("Chemicals", "Timepoint"))




pest_long$Chemicals<- factor(pest_long$Chemicals,
                             levels = c('adjusted_DEP', 'adjusted_DMP', 'adjusted_DETP', 'adjusted_DMTP', 'adjusted_TCP', 'adjusted_PNP', 'adjusted_DEDP', 'adjusted_DMDP', 'adjusted_PBA', 'adjusted_TRANS_DCCA', 'adjusted_CIS_DCCA',  'adjusted_FPBA', 'adjusted_PCP'),
                             labels = c('DEP', 'DMP', 'DETP', 'DMTP', 'TCP', 'PNP', 'DEDP', 'DMDP', 'PBA', 'TRANS DCCA', 'CIS DCCA', 'FPBA', 'PCP'))


pest_long$Group<- factor(pest_long$Group,
                         levels = c("PFAS", "LMWPs", "HMWPs", "OP pesticides", "PYR metabolites", "OC pesticides"))

pest_long$Timepoint<- factor(pest_long$Timepoint,
                             levels = c("pcv2", "pgv3"))


# put in plot
p3 <- (ggplot(pest_long, aes(y=Chemicals, x=log2(value),color=Group, fill=Timepoint)) + 
         geom_boxplot(size=0.8)+
         scale_y_discrete(limits = rev(levels(pest_long$Chemicals)))+
         scale_color_manual(drop = FALSE,
                            values = c("#2E5FA1", "#E3882F", "#1B7C3D", "#C52A20" , "#8E549E", "#545454"),
                            labels = c("PFAS", "LMWPs", "HMWPs" , "OP pesticides", "PYR metabolites", "OC pesticides"))+
         scale_fill_manual(drop = FALSE,
                           values = c( "#FDFEFE", "#909497"),
                           labels = c("Preconception", "Pregnancy"),
                           limits = rev(levels(phtha_long$Timepoint)))+
         labs(y ="",x = "log2(Concentrations(µg/g))",
              title = "Pesticides") + 
         theme_classic())

pm3 = p3 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 10),
                 axis.text.y = element_text(size = 9,face = "bold"),
                 legend.text = element_text(size = 9,face = "bold"),
                 legend.title = element_text(size = 10,face = "bold"),
                 legend.position = "top",
                 plot.title = element_text(size = 10,face = "bold"),
                 axis.title=element_text(size=12,face="bold"),
                 panel.grid.major = element_line(color = '#FDFEFE', size = 0.8),
                 panel.grid.minor = element_line(color = '#FDFEFE', size = 0.6, linetype = "dotted"),
                 panel.background = element_rect(fill = '#F4F6F7'),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 9),
                 panel.spacing.y=unit(0.3, "lines"))+ 
          guides(color = guide_legend(nrow = 1))



pm3



plot<- ggarrange(pm1, pm2, pm3,
                 ncol = 3, nrow = 1,
                 common.legend = TRUE,
                 legend.grob = get_legend(pm3))



jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure1.jpeg",
     units="in", width=22, height=12, res=500)

plot

dev.off()




