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
PFAS<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")


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


# specify metal mixture
mixture_PFAS=c('PFOS_Total', 'PFOA_Linear', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NMeFOSAA', 'PAP', 'diPAP')
PFAS_name=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NMeFOSAA', '6:2 PAP', '6:2 diPAP')


library(corrplot)
library(Hmisc)

# compute r and p for your PFAS set
X <- as.matrix(PFAS[, mixture_PFAS])
rc <- Hmisc::rcorr(X, type = "spearman")

# rename to your display names
colnames(rc$r) <- PFAS_name
rownames(rc$r) <- PFAS_name
colnames(rc$P) <- PFAS_name
rownames(rc$P) <- PFAS_name

rc$P[is.na(rc$P)] <- 1
# your plotting function, now with stars
corr_fun <- function(corr_mat, p_mat) {
  corrplot(corr_mat,
           method = "color",
           col = colorRampPalette(c("steelblue", "white", "darkred"))(100),
           tl.pos = "tp",
           tl.srt = 30,
           tl.col = "black",
           p.mat = p_mat,
           sig.level = c(0.001, 0.01, 0.05),  # thresholds for *, **, ***
           insig = "label_sig",               # draw stars on significant cells
           pch.cex = 1.5,                     # star size
           pch.col = "black")                 # star color
}



jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/Supp_figure1.jpeg",
     units="in", width=14, height=10, res=500)

# call with your matrices
corr_fun(rc$r, rc$P)



dev.off()



# Long format (all cells)
corr_long_all <- PFAS_corr  %>% 
  as.data.frame(check.names = FALSE) |>
  rownames_to_column("PFAS1") |>
  pivot_longer(-PFAS1, names_to = "PFAS2", values_to = "cor")

# Keep one pair per combination and drop the diagonal
corr_long <- corr_long_all |>
  filter(PFAS1 < PFAS2) |>
  mutate(
    PFAS1 = factor(PFAS1, levels = rownames(PFAS_corr)),
    PFAS2 = factor(PFAS2, levels = colnames(PFAS_corr))
  )
summary(corr_long$cor)


#-----------------------------------------Phthalates
Phthalates<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_long_20230317.xlsx")
Phthalates_wide<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")

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
p2 <- (ggplot(phtha_long, aes(y=reorder(Chemicals, median), x=log2(value),color=Group, fill=fct_rev(Timepoint))) + 
         geom_boxplot(size=0.8)+
         scale_color_manual(drop = FALSE,
                            values = c("#2E5FA1",  "#E3882F", "#1B7C3D", "#C52A20" , "#8E549E", "#545454"),
                            labels = c("PFAS", "LMWPs", "HMWPs" , "OP pesticides", "PYR metabolites", "OC pesticides"))+
         scale_fill_manual(drop = FALSE,
                           values = c( "#FDFEFE", "#909497"),
                           labels = c("Preconception", "Pregnancy"),
                           breaks = c("pcv2", "pgv3"))+
         labs(y ="",x = "log2(Concentrations(µg/g))",
              title = "Phthalates",
              fill = "Timepoint") + 
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

phtha_name=c('MEP at PC',
             'MCPP at PC',
             'MBZP at PC',
             'MBP at PC',
             'MIBP at PC',
             'MEHP at PC',
             'MEOHP at PC',
             'MEHHP at PC',
             'MECPP at PC',
             'MCIOP at PC',
             'MCINP at PC',
             'MEHTP at PC',
             'MEOHTP at PC',
             'MEHHTP at PC',
             'MECPTP at PC',
             'MEP at MP',
             'MCPP at MP',
             'MBZP at MP',
             'MBP at MP',
             'MIBP at MP',
             'MEHP at MP',
             'MEOHP at MP',
             'MEHHP at MP',
             'MECPP at MP',
             'MCIOP at MP',
             'MCINP at MP',
             'MEHTP at MP',
             'MEOHTP at MP',
             'MEHHTP at MP',
             'MECPTP at MP'
             )
# correlation between phtha



# compute r and p for your PFAS set
X <- as.matrix(Phthalates_wide[,mixture_phtha])
rc <- Hmisc::rcorr(X, type = "spearman")

# rename to your display names
colnames(rc$r) <- phtha_name
rownames(rc$r) <- phtha_name
colnames(rc$P) <- phtha_name
rownames(rc$P) <- phtha_name

rc$P[is.na(rc$P)] <- 1
# your plotting function, now with stars
corr_fun <- function(corr_mat, p_mat) {
  corrplot(corr_mat,
           method = "color",
           col = colorRampPalette(c("steelblue", "white", "darkred"))(100),
           tl.pos = "tp",
           tl.srt = 30,
           tl.col = "black",
           p.mat = p_mat,
           sig.level = c(0.001, 0.01, 0.05),  # thresholds for *, **, ***
           insig = "label_sig",               # draw stars on significant cells
           pch.cex = 1.5,                     # star size
           pch.col = "black")                 # star color
}




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/Supp_figure2.jpeg",
     units="in", width=16, height=12, res=500)


corr_fun(rc$r, rc$P) %>%
  corrRect(name = c('MEP at PC', 'MEP at MP', 'MECPTP at MP'))

dev.off()




# Long format (all cells)
corr_long_all <- phtha_corr [1:15, 16:30]  %>% 
  diag()


summary(corr_long_all)

#-----------------------------------------Pesticides
Pesticides<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_long_20230317.xlsx")
Pesticides_wide<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_wide_20230317.xlsx")

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
p3 <- (ggplot(pest_long, aes(y=Chemicals, x=log2(value),color=Group, fill=fct_rev(Timepoint))) + 
         geom_boxplot(size=0.8)+
         scale_y_discrete(limits = rev(levels(pest_long$Chemicals)))+
         scale_color_manual(drop = FALSE,
                            values = c("#2E5FA1", "#E3882F", "#1B7C3D", "#C52A20" , "#8E549E", "#545454"),
                            labels = c("PFAS", "LMWPs", "HMWPs" , "OP pesticides", "Pyrethroids", "OC pesticides"))+
         scale_fill_manual(drop = FALSE,
                           values = c( "#FDFEFE", "#909497"),
                           labels = c("Preconception", "Pregnancy"),
                           breaks = c("pcv2", "pgv3"))+
         labs(y ="",x = "log2(Concentrations(µg/g))",
              title = "Pesticides",
              fill = "Timepoint") + 
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


pest_name=c('PBA at PC',
            'FPBA at PC',
            'CIS DCCA at PC',
            'TRANS DCCA at PC',
            'PCP at PC',
            'PNP at PC',
            'TCP at PC',
            'DMP at PC',
            'DMTP at PC',
            'DMDP at PC',
            'DEP at PC',
            'DETP at PC',
            'DEDP at PC',
            'PBA at MP',
            'FPBA at MP',
            'CIS DCCA at MP',
            'TRANS DCCA at MP',
            'PCP at MP',
            'PNP at MP',
            'TCP at MP',
            'DMP at MP',
            'DMTP at MP',
            'DMDP at MP',
            'DEP at MP',
            'DETP at MP',
            'DEDP at MP'
)

# mixture_PFAS

# compute r and p for your PFAS set
X <- as.matrix(Pesticides_wide[,mixture_pest])
rc <- Hmisc::rcorr(X, type = "spearman")

# rename to your display names
colnames(rc$r) <- pest_name
rownames(rc$r) <- pest_name
colnames(rc$P) <- pest_name
rownames(rc$P) <- pest_name

rc$P[is.na(rc$P)] <- 1
# your plotting function, now with stars
corr_fun <- function(corr_mat, p_mat) {
  corrplot(corr_mat,
           method = "color",
           col = colorRampPalette(c("steelblue", "white", "darkred"))(100),
           tl.pos = "tp",
           tl.srt = 30,
           tl.col = "black",
           p.mat = p_mat,
           sig.level = c(0.001, 0.01, 0.05),  # thresholds for *, **, ***
           insig = "label_sig",               # draw stars on significant cells
           pch.cex = 1.5,                     # star size
           pch.col = "black")                 # star color
}






jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/Supp_figure3.jpeg",
     units="in", width=16, height=12, res=500)

corr_fun(rc$r, rc$P) %>%
corrRect(name = c('PBA at PC', 'PBA at MP', 'DEDP at MP'))

dev.off()


# Long format (all cells)
corr_long_all <- pest_corr [1:13, 14:26]  %>% 
  diag()


summary(corr_long_all)

summary(corr_long_all[1:4])
summary(corr_long_all[6:13])



plot<- ggarrange(pm1_1, pm2_1, pm3_1,
                 ncol = 3, nrow = 1,
                 common.legend = TRUE,
                 legend.grob = get_legend(pm3_1))
plot




jpeg("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/code/R/chemical & covariates/paper plot/figure1.jpeg",
     units="in", width=24, height=15, res=600)

plot

dev.off()




