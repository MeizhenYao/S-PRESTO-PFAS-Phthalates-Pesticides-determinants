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
library(grid)
library(gridGraphics)
library(ggplot2)
library(ggplotify)
library(ggcorrplot)
library(corrplot)



#------------------------------------------------------Plot
#-----------------------------------------PFAS
#--- boxplot
# import data (change the path to the location where you save the dataset)
PFAS<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")


## data in long format
PFAS_long<- PFAS %>% 
  pivot_longer(cols=c(PFOS_Total, PFOA_Linear, PFNA, PFHxS, PFDA, PFHpA, PFHpS, PFBS, NEtFOSAA, NMeFOSAA, PAP, diPAP, FTS, PFOSA, PFDS),
               names_to = "Chemicals",
               values_to="value")

PFAS_long$Chemicals<- factor(PFAS_long$Chemicals,
                             levels=c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', 'PAP', 'diPAP', 'FTS', 'PFOSA', 'PFDS'),
                             labels=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', '6:2 PAP', '6:2 diPAP', '6:2 FTS', 'PFOSA', 'PFDS'))


# put in plot
p1 <- (ggplot(PFAS_long, aes(x=Chemicals, y=log2(value))) + 
         geom_boxplot(color="#2E5FA1", size=0.8, fill="#FDFEFE")+
         scale_x_discrete(limits = rev(levels(PFAS_long$Chemicals)))+
         labs(x ="",y = "log2(Concentrations(ng/ml))",
              title = "PFAS") +  
         coord_flip()+
         theme_classic())


pm1_1 = p1 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 18),
                 axis.text.y = element_text(size = 18,face = "bold"),
                 legend.text = element_text(size = 18,face = "bold"),
                 legend.title = element_text(size = 18,face = "bold"),
                 plot.title = element_text(size = 18,face = "bold"),
                 axis.title=element_text(size=18,face="bold"),
                 panel.grid.major = element_line(color = '#FDFEFE', size = 0.8),
                 panel.grid.minor = element_line(color = '#FDFEFE', size = 0.6, linetype = "dotted"),
                 panel.background = element_rect(fill = '#F4F6F7'),
                 plot.margin = unit(c(0,1,0,0), "lines"),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 10),
                 panel.spacing.y=unit(0.1, "lines"))



pm1_1

#--- corrplot
corr_fun<- function(data){

corrplot(data, 
         method="color" ,
         col = colorRampPalette(c("steelblue", "white", "darkred"))(100),cl.lim=c(0,1),
         type="upper",
         # order="hclust" ,
         tl.pos = 'tp',
         tl.srt=30,
         tl.col = "black",
) 
corrplot(data, 
         method="number", 
         type="lower", 
         # order="hclust" ,
         col = 'black', 
         tl.pos = 'n',
         cl.pos = 'n',
         add=TRUE
) 
}

# corrplot(data, 
#          method="number" ,
#          col = colorRampPalette(c("steelblue", "white", "darkred"))(100),cl.lim=c(0,1),
#          type="full",
#          # order="hclust" ,
#          # addrect = 2,
#          tl.pos = 'lt',
#          tl.srt=30,
#          tl.col = "black",
# )


# specify metal mixture
mixture_PFAS=c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NMeFOSAA', 'PAP', 'diPAP')
PFAS_name=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NMeFOSAA', '6:2 PAP', '6:2 diPAP')

# mixture_PFAS

PFAS_corr = cor(data.frame(PFAS[,mixture_PFAS]), method = "spearman")
colnames(PFAS_corr)<- PFAS_name[1:11]
rownames(PFAS_corr)<- PFAS_name[1:11]

jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/number/Supp_figure1.jpeg",
     units="in", width=16, height=12, res=500)


corr_fun(PFAS_corr) 

dev.off()

# ggplot(melt(cor(data.frame(PFAS[,mixture_PFAS]), method = "spearman")), aes(Var1, Var2, fill=value)) +
#   geom_tile(height=0.7, width=0.7) +
#   scale_fill_gradient2(low="blue", mid="white", high="red") +
#   theme_bw() +
#   coord_equal() +
#   labs(x="",y="",fill="Correlation", tag = "(B)") +
#   theme(axis.text.x=element_text(size=9, angle=50, vjust=1, hjust=1, 
#                                  margin=margin(-3,0,0,0), face = "bold"),
#         axis.ticks = element_blank(),
#         plot.tag = element_text(size = 14,face = "bold"),
#         axis.text.y=element_text(size=9, margin=margin(0,-2,0,0), face = "bold"),
#         panel.grid.major=element_blank(), plot.margin = margin(0,-5,0,-5),
#         legend.text = element_text(angle = 90, hjust = 1, vjust = 1)) 


#-----------------------------------------Phthalates
Phthalates<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_long_20230317.xlsx")
Phthalates_wide<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")

#--- boxplot
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

pm2_1 = p2 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 18),
                 axis.text.y = element_text(size = 18,face = "bold"),
                 legend.text = element_text(size = 18,face = "bold"),
                 legend.title = element_text(size = 18,face = "bold"),
                 plot.title = element_text(size = 18,face = "bold"),
                 axis.title=element_text(size=18,face="bold"),
                 panel.grid.major = element_line(color = '#FDFEFE', size = 0.8),
                 panel.grid.minor = element_line(color = '#FDFEFE', size = 0.6, linetype = "dotted"),
                 panel.background = element_rect(fill = '#F4F6F7'),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 12),
                 panel.spacing.y=unit(0.3, "lines"))



pm2_1

#--- corrplot
# specify metal mixture
mixture_phtha<- c('adjusted_MEP_pcv2',
                  'adjusted_MCPP_pcv2',
                  'adjusted_MBZP_pcv2',
                  'adjusted_MBP_pcv2',
                  'adjusted_MIBP_pcv2',
                  'adjusted_MEHP_pcv2',
                  'adjusted_MEOHP_pcv2',
                  'adjusted_MEHHP_pcv2',
                  'adjusted_MECPP_pcv2',
                  'adjusted_MCIOP_pcv2',
                  'adjusted_MCINP_pcv2',
                  'adjusted_MEHTP_pcv2',
                  'adjusted_MEOHTP_pcv2',
                  'adjusted_MEHHTP_pcv2',
                  'adjusted_MECPTP_pcv2',
                  'adjusted_MEP_pgv3',
                  'adjusted_MCPP_pgv3',
                  'adjusted_MBZP_pgv3',
                  'adjusted_MBP_pgv3',
                  'adjusted_MIBP_pgv3',
                  'adjusted_MEHP_pgv3',
                  'adjusted_MEOHP_pgv3',
                  'adjusted_MEHHP_pgv3',
                  'adjusted_MECPP_pgv3',
                  'adjusted_MCIOP_pgv3',
                  'adjusted_MCINP_pgv3',
                  'adjusted_MEHTP_pgv3',
                  'adjusted_MEOHTP_pgv3',
                  'adjusted_MEHHTP_pgv3',
                  'adjusted_MECPTP_pgv3'
                  )

phtha_name=c('MEP at pcv2',
             'MCPP at pcv2',
             'MBZP at pcv2',
             'MBP at pcv2',
             'MIBP at pcv2',
             'MEHP at pcv2',
             'MEOHP at pcv2',
             'MEHHP at pcv2',
             'MECPP at pcv2',
             'MCIOP at pcv2',
             'MCINP at pcv2',
             'MEHTP at pcv2',
             'MEOHTP at pcv2',
             'MEHHTP at pcv2',
             'MECPTP at pcv2',
             'MEP at pgv3',
             'MCPP at pgv3',
             'MBZP at pgv3',
             'MBP at pgv3',
             'MIBP at pgv3',
             'MEHP at pgv3',
             'MEOHP at pgv3',
             'MEHHP at pgv3',
             'MECPP at pgv3',
             'MCIOP at pgv3',
             'MCINP at pgv3',
             'MEHTP at pgv3',
             'MEOHTP at pgv3',
             'MEHHTP at pgv3',
             'MECPTP at pgv3'
             )
# mixture_PFAS

phtha_corr = cor(data.frame(Phthalates_wide[,mixture_phtha]), method = "spearman")
colnames(phtha_corr)<- phtha_name[1:30]
rownames(phtha_corr)<- phtha_name[1:30]

jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/number/Supp_figure2.jpeg",
     units="in", width=16, height=12, res=500)


corr_fun(phtha_corr) %>%
  corrRect(name = c('MEP at pcv2', 'MEP at pgv3', 'MECPTP at pgv3'))

dev.off()


#-----------------------------------------Pesticides
Pesticides<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_long_20230317.xlsx")
Pesticides_wide<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_wide_20230317.xlsx")

pest_name<- c('adjusted_PBA','adjusted_PBA','adjusted_FPBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PCP','adjusted_PNP','adjusted_PNP','adjusted_TCP','adjusted_TCP','adjusted_DMP','adjusted_DMP','adjusted_DMTP','adjusted_DMTP','adjusted_DMDP','adjusted_DMDP','adjusted_DEP','adjusted_DEP','adjusted_DETP','adjusted_DETP','adjusted_DEDP','adjusted_DEDP')
pest_time<- c("pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3","pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3", "pcv2", "pgv3")
pest_group<- c("PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites","PYR metabolites", "PYR metabolites", "OC pesticides", "OC pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides")

#--- boxplot
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
                         levels = c("PFAS", "LMWPs", "HMWPs", "OP pesticides", "PYR metabolites", "OC pesticides"),
                         labels = c("PFAS", "LMWPs", "HMWPs", "OP pesticides", "Pyrethroids", "OC pesticides"))

pest_long$Timepoint<- factor(pest_long$Timepoint,
                             levels = c("pcv2", "pgv3"))


# put in plot
p3 <- (ggplot(pest_long, aes(y=Chemicals, x=log2(value),color=Group, fill=Timepoint)) + 
         geom_boxplot(size=0.8)+
         scale_y_discrete(limits = rev(levels(pest_long$Chemicals)))+
         scale_color_manual(drop = FALSE,
                            values = c("#2E5FA1", "#E3882F", "#1B7C3D", "#C52A20" , "#8E549E", "#545454"),
                            labels = c("PFAS", "LMWPs", "HMWPs" , "OP pesticides", "Pyrethroids", "OC pesticides"))+
         scale_fill_manual(drop = FALSE,
                           values = c( "#FDFEFE", "#909497"),
                           labels = c("Preconception", "Pregnancy"),
                           limits = rev(levels(phtha_long$Timepoint)))+
         labs(y ="",x = "log2(Concentrations(µg/g))",
              title = "Pesticides") + 
         theme_classic())

pm3_1 = p3 + theme(axis.text.x= element_text(face="bold", angle = 0, size = 18),
                 axis.text.y = element_text(size = 18,face = "bold"),
                 legend.text = element_text(size = 18,face = "bold"),
                 legend.title = element_text(size = 18,face = "bold"),
                 legend.position = "top",
                 plot.title = element_text(size = 18,face = "bold"),
                 axis.title=element_text(size=18,face="bold"),
                 panel.grid.major = element_line(color = '#FDFEFE', size = 0.8),
                 panel.grid.minor = element_line(color = '#FDFEFE', size = 0.6, linetype = "dotted"),
                 panel.background = element_rect(fill = '#F4F6F7'),
                 strip.placement = "outside",
                 strip.background = element_blank(),
                 strip.text = element_text(face="bold", angle = 0, size = 14),
                 panel.spacing.y=unit(0.3, "lines"))+ 
          guides(color = guide_legend(nrow = 1))



pm3_1



#--- corrplot
# specify metal mixture
mixture_pest<- c('adjusted_PBA_pcv2',
                 'adjusted_FPBA_pcv2',
                 'adjusted_CIS_DCCA_pcv2',
                 'adjusted_TRANS_DCCA_pcv2',
                 'adjusted_PCP_pcv2',
                 'adjusted_PNP_pcv2',
                 'adjusted_TCP_pcv2',
                 'adjusted_DMP_pcv2',
                 'adjusted_DMTP_pcv2',
                 'adjusted_DMDP_pcv2',
                 'adjusted_DEP_pcv2',
                 'adjusted_DETP_pcv2',
                 'adjusted_DEDP_pcv2',
                 'adjusted_PBA_pgv3',
                 'adjusted_FPBA_pgv3',
                 'adjusted_CIS_DCCA_pgv3',
                 'adjusted_TRANS_DCCA_pgv3',
                 'adjusted_PCP_pgv3',
                 'adjusted_PNP_pgv3',
                 'adjusted_TCP_pgv3',
                 'adjusted_DMP_pgv3',
                 'adjusted_DMTP_pgv3',
                 'adjusted_DMDP_pgv3',
                 'adjusted_DEP_pgv3',
                 'adjusted_DETP_pgv3',
                 'adjusted_DEDP_pgv3'
                 
)


pest_name=c('PBA at pcv2',
            'FPBA at pcv2',
            'CIS DCCA at pcv2',
            'TRANS DCCA at pcv2',
            'PCP at pcv2',
            'PNP at pcv2',
            'TCP at pcv2',
            'DMP at pcv2',
            'DMTP at pcv2',
            'DMDP at pcv2',
            'DEP at pcv2',
            'DETP at pcv2',
            'DEDP at pcv2',
            'PBA at pgv3',
            'FPBA at pgv3',
            'CIS DCCA at pgv3',
            'TRANS DCCA at pgv3',
            'PCP at pgv3',
            'PNP at pgv3',
            'TCP at pgv3',
            'DMP at pgv3',
            'DMTP at pgv3',
            'DMDP at pgv3',
            'DEP at pgv3',
            'DETP at pgv3',
            'DEDP at pgv3'
)

# mixture_PFAS

pest_corr = cor(data.frame(Pesticides_wide[,mixture_pest]), method = "spearman")
colnames(pest_corr)<- pest_name[1:26]
rownames(pest_corr)<- pest_name[1:26]

jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/number/Supp_figure3.jpeg",
     units="in", width=16, height=12, res=500)

corr_fun(pest_corr)%>%
corrRect(name = c('PBA at pcv2', 'PBA at pgv3', 'DEDP at pgv3'))

dev.off()







plot<- ggarrange(pm1_1, pm2_1, pm3_1,
                 ncol = 3, nrow = 1,
                 common.legend = TRUE,
                 legend.grob = get_legend(pm3_1))
plot




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure1.jpeg",
     units="in", width=24, height=15, res=600)

plot

dev.off()




