library(yaml)
library(rmarkdown)
library(tidyverse)
library(flextable)
library(officer)
library(Rcpp)
library(scales)
library(formattable)
library(broom)
library(readxl)

#------------------------------------------------
# PFAS
PFAS<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_PFAS_20230316.xlsx")

names(PFAS)[30]<- paste("FTS_Comment")
names(PFAS)[32]<- paste("PAP_Comment")
names(PFAS)[34]<- paste("diPAP_Comment")

attach(PFAS)
## NAME
Name_of_PFAS<- c("PFBS","PFHxS","PFHpA","Total PFOS","Linear PFOA",
                 "PFNA","PFDA","PFHpS","NMeFOSAA","NEtFOSAA",
                 "6:2 FTS","6:2 PAP","6:2 diPAP","PFOSA","PFDS")
LOD<- c(0.2, 0.1, 0.2, 0.2, 0.5, 0.5, 0.5, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1)


## number of subject
n1_PFBS<-PFAS %>% 
  filter(is.na(PFBS)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFHxS<-PFAS %>% 
  filter(is.na(PFHxS)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFHpA<-PFAS %>% 
  filter(is.na(PFHpA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFOS_Total<-PFAS %>% 
  filter(is.na(PFOS_Total)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFOA_Linear<-PFAS %>% 
  filter(is.na(PFOA_Linear)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFNA<-PFAS %>% 
  filter(is.na(PFNA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFDA<-PFAS %>% 
  filter(is.na(PFDA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFHpS<-PFAS %>% 
  filter(is.na(PFHpS)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_NMeFOSAA<-PFAS %>% 
  filter(is.na(NMeFOSAA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_NEtFOSAA<-PFAS %>% 
  filter(is.na(NEtFOSAA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_FTS<-PFAS %>% 
  filter(is.na('FTS')==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PAP<-PFAS %>% 
  filter(is.na('PAP')==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_diPAP<-PFAS %>% 
  filter(is.na('diPAP')==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFOSA<-PFAS %>% 
  filter(is.na(PFOSA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_PFDS<-PFAS %>% 
  filter(is.na(PFDS)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
PFAS_N<-rbind(n1_PFBS,n1_PFHxS,n1_PFHpA,n1_PFOS_Total,n1_PFOA_Linear,n1_PFNA,n1_PFDA,n1_PFHpS,n1_NMeFOSAA,n1_NEtFOSAA,n1_FTS,n1_PAP,n1_diPAP,n1_PFOSA,n1_PFDS)

## <LOD

LOD_PFBS<- PFAS %>% 
  filter(PFBS_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFBS),2)
LOD_PFHxS<- PFAS %>% 
  filter(PFHxS_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFHxS),2)
LOD_PFHpA<- PFAS %>% 
  filter(PFHpA_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFHpA),2)
LOD_PFOS_Total<- PFAS %>% 
  filter(PFOS_Linear_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFOS_Total),2)
LOD_PFOA_Linear<- PFAS %>% 
  filter(PFOA_Linear_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFOA_Linear),2)
LOD_PFNA<- PFAS %>% 
  filter(PFNA_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFNA),2)
LOD_PFDA<- PFAS %>% 
  filter(PFDA_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFDA),2)
LOD_PFHpS<- PFAS %>% 
  filter(PFHpS_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFHpS),2)
LOD_NMeFOSAA<- PFAS %>% 
  filter(NMeFOSAA_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_NMeFOSAA),2)
LOD_NEtFOSAA<- PFAS %>% 
  filter(NEtFOSAA_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_NEtFOSAA),2)
LOD_FTS<- PFAS %>% 
  filter(FTS_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_FTS),2)
LOD_PAP<- PFAS %>% 
  filter(PAP_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PAP),2)
LOD_diPAP<- PFAS %>% 
  filter(diPAP_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_diPAP),2)
LOD_PFOSA<- PFAS %>% 
  filter(PFOSA_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFOSA),2)
LOD_PFDS<- PFAS %>% 
  filter(PFDS_Comment==0) %>% 
  summarise(percent=percent(length(unique(Subject_ID))/n1_PFDS),2)
PFAS_LOD<-rbind(LOD_PFBS,LOD_PFHxS,LOD_PFHpA,LOD_PFOS_Total,LOD_PFOA_Linear,LOD_PFNA,LOD_PFDA,LOD_PFHpS,LOD_NMeFOSAA,LOD_NEtFOSAA,LOD_FTS,LOD_PAP,LOD_diPAP,LOD_PFOSA,LOD_PFDS)

## minimum
PFAS_min<-    rbind(min(PFBS,na.rm=TRUE),
                    min(PFHxS, na.rm=TRUE),
                    min(PFHpA, na.rm=TRUE),
                    min(PFOS_Total, na.rm=TRUE),
                    min(PFOA_Linear, na.rm=TRUE),
                    min(PFNA, na.rm=TRUE),
                    min(PFDA, na.rm=TRUE),
                    min(PFHpS, na.rm=TRUE),
                    min(NMeFOSAA, na.rm=TRUE),
                    min(NEtFOSAA, na.rm=TRUE),
                    min(FTS, na.rm=TRUE),
                    min(PAP, na.rm=TRUE),
                    min(diPAP, na.rm=TRUE),
                    min(PFOSA, na.rm=TRUE),
                    min(PFDS, na.rm=TRUE))
## Quantile
PFAS_quantile<-     rbind(quantile(PFBS,c(.05,.25,.50,.75,.95),na.rm=TRUE),
                          quantile(PFHxS,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFHpA,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFOS_Total,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFOA_Linear,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFNA,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFDA,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFHpS, c(.05,.25,.50,.75,.95),na.rm=TRUE),
                          quantile(NMeFOSAA,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(NEtFOSAA,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(FTS,c(.05,.25,.50,.75,.95),na.rm=TRUE),
                          quantile(PAP,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(diPAP,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFOSA,c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PFDS,c(.05,.25,.50,.75,.95), na.rm=TRUE))
## maxmum
PFAS_max <-   rbind(max(PFBS,na.rm=TRUE),
                    max(PFHxS, na.rm=TRUE),
                    max(PFHpA, na.rm=TRUE),
                    max(PFOS_Total, na.rm=TRUE),
                    max(PFOA_Linear, na.rm=TRUE),
                    max(PFNA, na.rm=TRUE),
                    max(PFDA, na.rm=TRUE),
                    max(PFHpS, na.rm=TRUE),
                    max(NMeFOSAA, na.rm=TRUE),
                    max(NEtFOSAA, na.rm=TRUE),
                    max(FTS, na.rm=TRUE),
                    max(PAP, na.rm=TRUE),
                    max(diPAP, na.rm=TRUE),
                    max(PFOSA, na.rm=TRUE),
                    max(PFDS, na.rm=TRUE))

## data.frame
PFAS_data<- data.frame(Name_of_PFAS,
                       LOD,
                       N=PFAS_N$n,
                       LOD_per=PFAS_LOD$percent,
                       min=round(PFAS_min,2),
                       Quantile=round(PFAS_quantile,2),
                       max=round(PFAS_max,2))

PFAS_data$Name_of_PFAS <- factor(PFAS_data$Name_of_PFAS ,
                          levels=c('Total PFOS', 'Linear PFOA', 'PFNA', 'PFHxS', 'PFDA', 'PFHpA', 'PFHpS', 'PFBS', 'NEtFOSAA', 'NMeFOSAA', '6:2 PAP', '6:2 diPAP', '6:2 FTS', 'PFOSA', 'PFDS'))


PFAS_table<-flextable(PFAS_data%>% arrange(by = Name_of_PFAS))


## add one header line
PFAS_table<- add_header_row(PFAS_table,value=c("PFAS"),colwidths=c(11))

## change header name
PFAS_table<- set_header_labels(PFAS_table, Name_of_PFAS="PFAS (ng/ml)",N="Number of Subject", LOD="(<LOD)%",Quantile.5.="P5",Quantile.25.="P25",Quantile.50.="P50",Quantile.75.="P75",Quantile.95.="P95")

## add one footer line
PFAS_table<- add_footer_lines(PFAS_table,
                              values=c("(<LOD)% greater than 50 percent is marked in red",
                                       "If <LOD, then the value of LOD/2 is used instead."))

PFAS_table<-  PFAS_table %>% 
  theme_box() %>% 
  # vline(j=c(1,11,21) ,border = fp_border_default()) %>% 
  align(align="center",part="header") 
PFAS_table


#------------------------------------------------
# Phthalates
phtha_raw<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_long_20230317.xlsx")
phtha_wide<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_wide_20230317.xlsx")
phtha_comment <- read_excel("~/Projects/S-PRESTO/input/chemical/phtha_adjusted_CRE.xlsx")


phtha<- phtha_raw %>% 
        left_join(phtha_comment %>% select(Subject_ID, Timepoint, starts_with("Comment1")), by=c("Subject_ID", "Timepoint"))
# pcv2
pcv2data<- phtha %>% 
           filter(Timepoint=="pcv2")
attach(pcv2data)

## obs COLUMN
obs_adMEP<- pcv2data %>% 
  filter(is.na(adjusted_MEP)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adMCPP<- pcv2data %>% 
  filter(is.na(adjusted_MCPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBZP<- pcv2data %>% 
  filter(is.na(adjusted_MBZP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBP<- pcv2data %>% 
  filter(is.na(adjusted_MBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMIBP<- pcv2data %>% 
  filter(is.na(adjusted_MIBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHP<- pcv2data %>% 
  filter(is.na(adjusted_MEHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHP<- pcv2data %>% 
  filter(is.na(adjusted_MEOHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHP<- pcv2data %>% 
  filter(is.na(adjusted_MEHHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPP<- pcv2data %>% 
  filter(is.na(adjusted_MECPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCIOP<- pcv2data %>% 
  filter(is.na(adjusted_MCIOP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCINP<- pcv2data %>% 
  filter(is.na(adjusted_MCINP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHTP<- pcv2data %>% 
  filter(is.na(adjusted_MEHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHTP<- pcv2data %>% 
  filter(is.na(adjusted_MEOHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHTP<- pcv2data %>% 
  filter(is.na(adjusted_MEHHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPTP<- pcv2data %>% 
  filter(is.na(adjusted_MECPTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
pcv2data_obs<- rbind(obs_adMEP,obs_adMCPP,obs_adMBZP, obs_adMBP,obs_adMIBP,obs_adMEHP,obs_adMEOHP, obs_adMEHHP,obs_adMECPP, obs_adMCIOP, obs_adMCINP, obs_adMEHTP, obs_adMEOHTP,obs_adMEHHTP,obs_adMECPTP)

## FIRST COLUMN
n1_adMEP<- pcv2data %>% 
  filter(is.na(adjusted_MEP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMCPP<- pcv2data %>% 
  filter(is.na(adjusted_MCPP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMBZP<- pcv2data %>% 
  filter(is.na(adjusted_MBZP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMBP<- pcv2data %>% 
  filter(is.na(adjusted_MBP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMIBP<- pcv2data %>% 
  filter(is.na(adjusted_MIBP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHP<- pcv2data %>% 
  filter(is.na(adjusted_MEHP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEOHP<- pcv2data %>% 
  filter(is.na(adjusted_MEOHP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHHP<- pcv2data %>% 
  filter(is.na(adjusted_MEHHP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMECPP<- pcv2data %>% 
  filter(is.na(adjusted_MECPP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMCIOP<- pcv2data %>% 
  filter(is.na(adjusted_MCIOP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMCINP<- pcv2data %>% 
  filter(is.na(adjusted_MCINP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHTP<- pcv2data %>% 
  filter(is.na(adjusted_MEHTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEOHTP<- pcv2data %>% 
  filter(is.na(adjusted_MEOHTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHHTP<- pcv2data %>% 
  filter(is.na(adjusted_MEHHTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMECPTP<- pcv2data %>% 
  filter(is.na(adjusted_MECPTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
pcv2data_first<- rbind(n1_adMEP,n1_adMCPP,n1_adMBZP, n1_adMBP, n1_adMIBP, n1_adMEHP, n1_adMEOHP, n1_adMEHHP, n1_adMECPP, n1_adMCIOP, n1_adMCINP, n1_adMEHTP, n1_adMEOHTP, n1_adMEHHTP,n1_adMECPTP)


## SECOND COLUMN
LOD_MEP<- pcv2data %>% 
  filter(Comment1_MEP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEP, 2))
LOD_MCPP<- pcv2data %>% 
  filter(Comment1_MCPP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMCPP,2))
LOD_MBZP<- pcv2data %>% 
  filter(Comment1_MBZP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMBZP,2))
LOD_MBP<- pcv2data %>% 
  filter(Comment1_MBP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMBP,2))
LOD_MIBP<- pcv2data %>% 
  filter(Comment1_MIBP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMIBP,2))
LOD_MEHP<- pcv2data %>% 
  filter(Comment1_MEHP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHP,2))
LOD_MEOHP<- pcv2data %>% 
  filter(Comment1_MEOHP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEOHP,2))
LOD_MEHHP<- pcv2data %>% 
  filter(Comment1_MEHHP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHHP,2))
LOD_MECPP<- pcv2data %>% 
  filter(Comment1_MECPP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMECPP,2))
LOD_MCIOP<- pcv2data %>% 
  filter(Comment1_MCIOP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMCIOP,2))
LOD_MCINP<- pcv2data %>% 
  filter(Comment1_MCINP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMCINP,2))
LOD_MEHTP<- pcv2data %>% 
  filter(Comment1_MEHTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHTP,2))
LOD_MEOHTP<- pcv2data %>% 
  filter(Comment1_MEOHTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEOHTP,2))
LOD_MEHHTP<- pcv2data %>% 
  filter(Comment1_MEHHTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHHTP,2))
LOD_MECPTP<- pcv2data %>% 
  filter(Comment1_MECPTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMECPTP,2))

pcv2data_second<- rbind(LOD_MEP, LOD_MCPP, LOD_MBZP, LOD_MBP, LOD_MIBP, LOD_MEHP, LOD_MEOHP, LOD_MEHHP, LOD_MECPP, LOD_MCIOP, LOD_MCINP, LOD_MEHTP, LOD_MEOHTP, LOD_MEHHTP, LOD_MECPTP)


## THIRD COLUMN

pcv2data_third<- rbind(min(adjusted_MEP, na.rm=TRUE),
                       min(adjusted_MCPP, na.rm=TRUE),
                       min(adjusted_MBZP, na.rm=TRUE),
                       min(adjusted_MBP, na.rm=TRUE),
                       min(adjusted_MIBP, na.rm=TRUE),
                       min(adjusted_MEHP, na.rm=TRUE),
                       min(adjusted_MEOHP, na.rm=TRUE),
                       min(adjusted_MEHHP, na.rm=TRUE),
                       min(adjusted_MECPP, na.rm=TRUE),
                       min(adjusted_MCIOP, na.rm=TRUE),
                       min(adjusted_MCINP, na.rm=TRUE),
                       min(adjusted_MEHTP, na.rm=TRUE),
                       min(adjusted_MEOHTP, na.rm=TRUE),
                       min(adjusted_MEHHTP, na.rm=TRUE),
                       min(adjusted_MECPTP, na.rm=TRUE))

## QUANTILE COLOMN
pcv2data_quantile<- rbind(quantile(adjusted_MEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MCPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MBZP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MIBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEOHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MECPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MCIOP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MCINP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEOHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MECPTP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pcv2data_last<- rbind(max(adjusted_MEP, na.rm=TRUE),
                      max(adjusted_MCPP, na.rm=TRUE),
                      max(adjusted_MBZP, na.rm=TRUE),
                      max(adjusted_MBP, na.rm=TRUE),
                      max(adjusted_MIBP, na.rm=TRUE),
                      max(adjusted_MEHP, na.rm=TRUE),
                      max(adjusted_MEOHP, na.rm=TRUE),
                      max(adjusted_MEHHP, na.rm=TRUE),
                      max(adjusted_MECPP, na.rm=TRUE),
                      max(adjusted_MCIOP, na.rm=TRUE),
                      max(adjusted_MCINP, na.rm=TRUE),
                      max(adjusted_MEHTP, na.rm=TRUE),
                      max(adjusted_MEOHTP, na.rm=TRUE),
                      max(adjusted_MEHHTP, na.rm=TRUE),
                      max(adjusted_MECPTP, na.rm=TRUE))

pcv2data_overall<- data.frame(Name_of_phthalates=c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'),
                              obs=pcv2data_obs,
                              N=pcv2data_first$n,
                              Percent=pcv2data_second$percent,
                              min=round(pcv2data_third,2),
                              Quantile=round(pcv2data_quantile,2),
                              max=round(pcv2data_last,2)
)


# pgv3
pgv3data<- phtha %>% 
  filter(Timepoint=="pgv3")
attach(pgv3data)

## obs COLUMN
obs_adMEP<- pgv3data %>% 
  filter(is.na(adjusted_MEP)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adMCPP<- pgv3data %>% 
  filter(is.na(adjusted_MCPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBZP<- pgv3data %>% 
  filter(is.na(adjusted_MBZP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBP<- pgv3data %>% 
  filter(is.na(adjusted_MBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMIBP<- pgv3data %>% 
  filter(is.na(adjusted_MIBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHP<- pgv3data %>% 
  filter(is.na(adjusted_MEHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHP<- pgv3data %>% 
  filter(is.na(adjusted_MEOHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHP<- pgv3data %>% 
  filter(is.na(adjusted_MEHHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPP<- pgv3data %>% 
  filter(is.na(adjusted_MECPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCIOP<- pgv3data %>% 
  filter(is.na(adjusted_MCIOP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCINP<- pgv3data %>% 
  filter(is.na(adjusted_MCINP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHTP<- pgv3data %>% 
  filter(is.na(adjusted_MEHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHTP<- pgv3data %>% 
  filter(is.na(adjusted_MEOHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHTP<- pgv3data %>% 
  filter(is.na(adjusted_MEHHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPTP<- pgv3data %>% 
  filter(is.na(adjusted_MECPTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
pgv3data_obs<- rbind(obs_adMEP,obs_adMCPP,obs_adMBZP, obs_adMBP,obs_adMIBP,obs_adMEHP,obs_adMEOHP, obs_adMEHHP,obs_adMECPP, obs_adMCIOP, obs_adMCINP, obs_adMEHTP, obs_adMEOHTP,obs_adMEHHTP,obs_adMECPTP)

## FIRST COLUMN
n1_adMEP<- pgv3data %>% 
  filter(is.na(adjusted_MEP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMCPP<- pgv3data %>% 
  filter(is.na(adjusted_MCPP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMBZP<- pgv3data %>% 
  filter(is.na(adjusted_MBZP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMBP<- pgv3data %>% 
  filter(is.na(adjusted_MBP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMIBP<- pgv3data %>% 
  filter(is.na(adjusted_MIBP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHP<- pgv3data %>% 
  filter(is.na(adjusted_MEHP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEOHP<- pgv3data %>% 
  filter(is.na(adjusted_MEOHP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHHP<- pgv3data %>% 
  filter(is.na(adjusted_MEHHP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMECPP<- pgv3data %>% 
  filter(is.na(adjusted_MECPP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMCIOP<- pgv3data %>% 
  filter(is.na(adjusted_MCIOP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMCINP<- pgv3data %>% 
  filter(is.na(adjusted_MCINP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHTP<- pgv3data %>% 
  filter(is.na(adjusted_MEHTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEOHTP<- pgv3data %>% 
  filter(is.na(adjusted_MEOHTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMEHHTP<- pgv3data %>% 
  filter(is.na(adjusted_MEHHTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adMECPTP<- pgv3data %>% 
  filter(is.na(adjusted_MECPTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
pgv3data_first<- rbind(n1_adMEP,n1_adMCPP,n1_adMBZP, n1_adMBP, n1_adMIBP, n1_adMEHP, n1_adMEOHP, n1_adMEHHP, n1_adMECPP, n1_adMCIOP, n1_adMCINP, n1_adMEHTP, n1_adMEOHTP, n1_adMEHHTP,n1_adMECPTP)


## SECOND COLUMN
LOD_MEP<- pgv3data %>% 
  filter(Comment1_MEP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEP, 2))
LOD_MCPP<- pgv3data %>% 
  filter(Comment1_MCPP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMCPP,2))
LOD_MBZP<- pgv3data %>% 
  filter(Comment1_MBZP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMBZP,2))
LOD_MBP<- pgv3data %>% 
  filter(Comment1_MBP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMBP,2))
LOD_MIBP<- pgv3data %>% 
  filter(Comment1_MIBP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMIBP,2))
LOD_MEHP<- pgv3data %>% 
  filter(Comment1_MEHP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHP,2))
LOD_MEOHP<- pgv3data %>% 
  filter(Comment1_MEOHP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEOHP,2))
LOD_MEHHP<- pgv3data %>% 
  filter(Comment1_MEHHP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHHP,2))
LOD_MECPP<- pgv3data %>% 
  filter(Comment1_MECPP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMECPP,2))
LOD_MCIOP<- pgv3data %>% 
  filter(Comment1_MCIOP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMCIOP,2))
LOD_MCINP<- pgv3data %>% 
  filter(Comment1_MCINP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMCINP,2))
LOD_MEHTP<- pgv3data %>% 
  filter(Comment1_MEHTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHTP,2))
LOD_MEOHTP<- pgv3data %>% 
  filter(Comment1_MEOHTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEOHTP,2))
LOD_MEHHTP<- pgv3data %>% 
  filter(Comment1_MEHHTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMEHHTP,2))
LOD_MECPTP<- pgv3data %>% 
  filter(Comment1_MECPTP==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adMECPTP,2))

pgv3data_second<- rbind(LOD_MEP, LOD_MCPP, LOD_MBZP, LOD_MBP, LOD_MIBP, LOD_MEHP, LOD_MEOHP, LOD_MEHHP, LOD_MECPP, LOD_MCIOP, LOD_MCINP, LOD_MEHTP, LOD_MEOHTP, LOD_MEHHTP, LOD_MECPTP)


## THIRD COLUMN

pgv3data_third<- rbind(min(adjusted_MEP, na.rm=TRUE),
                       min(adjusted_MCPP, na.rm=TRUE),
                       min(adjusted_MBZP, na.rm=TRUE),
                       min(adjusted_MBP, na.rm=TRUE),
                       min(adjusted_MIBP, na.rm=TRUE),
                       min(adjusted_MEHP, na.rm=TRUE),
                       min(adjusted_MEOHP, na.rm=TRUE),
                       min(adjusted_MEHHP, na.rm=TRUE),
                       min(adjusted_MECPP, na.rm=TRUE),
                       min(adjusted_MCIOP, na.rm=TRUE),
                       min(adjusted_MCINP, na.rm=TRUE),
                       min(adjusted_MEHTP, na.rm=TRUE),
                       min(adjusted_MEOHTP, na.rm=TRUE),
                       min(adjusted_MEHHTP, na.rm=TRUE),
                       min(adjusted_MECPTP, na.rm=TRUE))

## QUANTILE COLOMN
pgv3data_quantile<- rbind(quantile(adjusted_MEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MCPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MBZP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MIBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEOHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MECPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MCIOP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MCINP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEOHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MEHHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_MECPTP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pgv3data_last<- rbind(max(adjusted_MEP, na.rm=TRUE),
                      max(adjusted_MCPP, na.rm=TRUE),
                      max(adjusted_MBZP, na.rm=TRUE),
                      max(adjusted_MBP, na.rm=TRUE),
                      max(adjusted_MIBP, na.rm=TRUE),
                      max(adjusted_MEHP, na.rm=TRUE),
                      max(adjusted_MEOHP, na.rm=TRUE),
                      max(adjusted_MEHHP, na.rm=TRUE),
                      max(adjusted_MECPP, na.rm=TRUE),
                      max(adjusted_MCIOP, na.rm=TRUE),
                      max(adjusted_MCINP, na.rm=TRUE),
                      max(adjusted_MEHTP, na.rm=TRUE),
                      max(adjusted_MEOHTP, na.rm=TRUE),
                      max(adjusted_MEHHTP, na.rm=TRUE),
                      max(adjusted_MECPTP, na.rm=TRUE))

pgv3data_overall<- data.frame(Name_of_phthalates=c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'),
                              obs=pgv3data_obs,
                              N=pgv3data_first$n,
                              Percent=pgv3data_second$percent,
                              min=round(pgv3data_third,2),
                              Quantile=round(pgv3data_quantile,2),
                              max=round(pgv3data_last,2)
)


Timecor_adMEP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEP~.$Timepoint) %>% tidy) %>%
  select(p.value)

Timecor_adMCPP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MCPP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMBZP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MBZP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMBP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MBP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMIBP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MIBP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEHP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEOHP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEOHP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHHP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEHHP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMECPP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MECPP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMCIOP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MCIOP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMCINP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MCINP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHTP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEHTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEOHTP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEOHTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHHTP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MEHHTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMECPTP<- phtha %>% 
  do(., wilcox.test(.$adjusted_MECPTP~.$Timepoint) %>% tidy) %>%
  select(p.value)

Timecor<-rbind(Timecor_adMEP,Timecor_adMCPP,Timecor_adMBZP, Timecor_adMBP,Timecor_adMIBP,Timecor_adMEHP,Timecor_adMEOHP, Timecor_adMEHHP,Timecor_adMECPP, Timecor_adMCIOP, Timecor_adMCINP, Timecor_adMEHTP, Timecor_adMEOHTP,Timecor_adMEHHTP,Timecor_adMECPTP)


 
## initial table
phtha_data_rela<- data.frame(Name_of_phthalates=c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'),
                             Timecor=round(Timecor,3))
phtha_data_rela<- flextable(phtha_data_rela)

# add one header line
phtha_data_rela<- phtha_data_rela %>%
  theme_box() %>%
  # vline(j=c(1,11,21) ,border = fp_border_default()) %>%
  align(align="center",part="header") %>%
  merge_v(j=1,part="header")

phtha_data_rela


pcv2_data<- pcv2data_overall %>% 
            mutate(TIME="pcv2")  
             

pgv3_data<- pgv3data_overall %>% 
            mutate(TIME="pgv3")  
phtha_data<- rbind(pcv2_data, pgv3_data)

phtha_data$Name_of_phthalates<- factor(phtha_data$Name_of_phthalates,
                                       levels = c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'))
phtha_parent<- c("Diethyl\nphthalate", "Diethyl\nphthalate", "Di-n-butyl\nphthalate", "Di-n-butyl\nphthalate", "Butylbenzyl\nphthalate", "Butylbenzyl\nphthalate", "Di-n-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Di-iso-nonyl\nphthalate", "Di-iso-nonyl\nphthalate", "Di-isodecyl\nphthalate", "Di-isodecyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate","Di-2-ethylhexyl\nterephthalate", "Di-2-ethylhexyl\nterephthalate")
phtha_group<- c("LMWPs", "LMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "LMWPs", "LMWPs", "LMWPs", "LMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs", "HMWPs")


ovarall_data<- data.frame(phtha_data %>% 
                        arrange(Name_of_phthalates, TIME),
                        Parent = phtha_parent,
                        Group = phtha_group)
                        
ovarall_data$Parent<- factor(ovarall_data$Parent,
                           levels = c("Diethyl\nphthalate", "Di-iso-butyl\nphthalate", "Di-n-butyl\nphthalate", "Di-2-ethylhexyl\nterephthalate", "Di-iso-nonyl\nphthalate", "Di-2-ethylhexyl\nphthalate", "Butylbenzyl\nphthalate", "Di-isodecyl\nphthalate"))

flextable(ovarall_data %>% arrange(desc(Group), Parent, TIME, desc(Quantile.50.))) %>% 
  theme_box()



#------------------------------------------------
# Pesticides
pest_raw<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_long_20230317.xlsx")
pest_wide<- read_excel("~/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_wide_20230317.xlsx")
pest_comment <- read_excel("~/Projects/S-PRESTO/input/chemical/pest_adjusted_CRE.xlsx")


pest<- pest_raw %>% 
       left_join(pest_comment %>% select(Subject_ID, Timepoint, CRE, ends_with("Comment")), by=c("Subject_ID", "Timepoint"))


# pcv2
pcv2data<- pest %>% 
  filter(Timepoint=="pcv2")
attach(pcv2data)

## obs COLUMN
obs_CRE<- pcv2data %>% 
  filter(is.na(CRE)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adPBA<- pcv2data %>% 
  filter(is.na(adjusted_PBA)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adFPBA<- pcv2data %>% 
  filter(is.na(adjusted_FPBA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adCIS_DCCA<- pcv2data %>% 
  filter(is.na(adjusted_CIS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTRANS_DCCA<- pcv2data %>% 
  filter(is.na(adjusted_TRANS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPCP<- pcv2data %>% 
  filter(is.na(adjusted_PCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPNP<- pcv2data %>% 
  filter(is.na(adjusted_PNP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTCP<- pcv2data %>% 
  filter(is.na(adjusted_TCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMP<- pcv2data %>% 
  filter(is.na(adjusted_DMP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMTP<- pcv2data %>% 
  filter(is.na(adjusted_DMTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMDP<- pcv2data %>% 
  filter(is.na(adjusted_DMDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEP<- pcv2data %>% 
  filter(is.na(adjusted_DEP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDETP<- pcv2data %>% 
  filter(is.na(adjusted_DETP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEDP<- pcv2data %>% 
  filter(is.na(adjusted_DEDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))

pcv2data_obs<- rbind(obs_CRE,obs_adPBA,obs_adFPBA,obs_adCIS_DCCA, obs_adTRANS_DCCA,obs_adPCP,obs_adPNP,obs_adTCP, obs_adDMP,obs_adDMTP, obs_adDMDP, obs_adDEP, obs_adDETP, obs_adDEDP)

## FIRST COLUMN
n1_CRE<- pcv2data %>% 
  filter(is.na(CRE)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adPBA<- pcv2data %>% 
  filter(is.na(adjusted_PBA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adFPBA<- pcv2data %>% 
  filter(is.na(adjusted_FPBA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adCIS_DCCA<- pcv2data %>% 
  filter(is.na(adjusted_CIS_DCCA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adTRANS_DCCA<- pcv2data %>% 
  filter(is.na(adjusted_TRANS_DCCA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adPCP<- pcv2data %>% 
  filter(is.na(adjusted_PCP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adPNP<- pcv2data %>% 
  filter(is.na(adjusted_PNP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adTCP<- pcv2data %>% 
  filter(is.na(adjusted_TCP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDMP<- pcv2data %>% 
  filter(is.na(adjusted_DMP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDMTP<- pcv2data %>% 
  filter(is.na(adjusted_DMTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDMDP<- pcv2data %>% 
  filter(is.na(adjusted_DMDP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDEP<- pcv2data %>% 
  filter(is.na(adjusted_DEP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDETP<- pcv2data %>% 
  filter(is.na(adjusted_DETP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDEDP<- pcv2data %>% 
  filter(is.na(adjusted_DEDP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))

pcv2data_first<- rbind(n1_CRE,n1_adPBA,n1_adFPBA,n1_adCIS_DCCA, n1_adTRANS_DCCA, n1_adPCP, n1_adPNP, n1_adTCP, n1_adDMP, n1_adDMTP, n1_adDMDP, n1_adDEP, n1_adDETP, n1_adDEDP)


## SECOND COLUMN
LOD_PBA<- pcv2data %>% 
  filter(PBA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adPBA, 2))
LOD_FPBA<- pcv2data %>% 
  filter(FPBA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adFPBA,2))
LOD_CIS_DCCA<- pcv2data %>% 
  filter(CIS_DCCA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adCIS_DCCA,2))
LOD_TRANS_DCCA<- pcv2data %>% 
  filter(TRANS_DCCA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adTRANS_DCCA,2))
LOD_PCP<- pcv2data %>% 
  filter(PCP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adPCP,2))
LOD_PNP<- pcv2data %>% 
  filter(PNP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adPNP,2))
LOD_TCP<- pcv2data %>% 
  filter(TCP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adTCP,2))
LOD_DMP<- pcv2data %>% 
  filter(DMP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDMP,2))
LOD_DMTP<- pcv2data %>% 
  filter(DMTP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDMTP,2))
LOD_DMDP<- pcv2data %>% 
  filter(DMDP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDMDP,2))
LOD_DEP<- pcv2data %>% 
  filter(DEP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDEP,2))
LOD_DETP<- pcv2data %>% 
  filter(DETP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDETP,2))
LOD_DEDP<- pcv2data %>% 
  filter(DEDP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDEDP,2))


pcv2data_second<- rbind(0, LOD_PBA, LOD_FPBA, LOD_CIS_DCCA, LOD_TRANS_DCCA, LOD_PCP, LOD_PNP, LOD_TCP, LOD_DMP, LOD_DMTP, LOD_DMDP, LOD_DEP, LOD_DETP, LOD_DEDP)


## THIRD COLUMN

pcv2data_third<- rbind(min(CRE,na.rm=TRUE),
                       min(adjusted_PBA, na.rm=TRUE),
                       min(adjusted_FPBA, na.rm=TRUE),
                       min(adjusted_CIS_DCCA, na.rm=TRUE),
                       min(adjusted_TRANS_DCCA, na.rm=TRUE),
                       min(adjusted_PCP, na.rm=TRUE),
                       min(adjusted_PNP, na.rm=TRUE),
                       min(adjusted_TCP, na.rm=TRUE),
                       min(adjusted_DMP, na.rm=TRUE),
                       min(adjusted_DMTP, na.rm=TRUE),
                       min(adjusted_DMDP, na.rm=TRUE),
                       min(adjusted_DEP, na.rm=TRUE),
                       min(adjusted_DETP, na.rm=TRUE),
                       min(adjusted_DEDP, na.rm=TRUE))

## QUANTILE COLOMN
pcv2data_quantile<- rbind(quantile(CRE, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_PBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_FPBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_CIS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_TRANS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_PCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_PNP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_TCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DMP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DMTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DMDP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DETP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DEDP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pcv2data_last<- rbind(max(CRE,na.rm=TRUE),
                      max(adjusted_PBA, na.rm=TRUE),
                      max(adjusted_FPBA, na.rm=TRUE),
                      max(adjusted_CIS_DCCA, na.rm=TRUE),
                      max(adjusted_TRANS_DCCA, na.rm=TRUE),
                      max(adjusted_PCP, na.rm=TRUE),
                      max(adjusted_PNP, na.rm=TRUE),
                      max(adjusted_TCP, na.rm=TRUE),
                      max(adjusted_DMP, na.rm=TRUE),
                      max(adjusted_DMTP, na.rm=TRUE),
                      max(adjusted_DMDP, na.rm=TRUE),
                      max(adjusted_DEP, na.rm=TRUE),
                      max(adjusted_DETP, na.rm=TRUE),
                      max(adjusted_DEDP, na.rm=TRUE))

pcv2data_overall<- data.frame(Name_of_pesticides=c("CRE",'adjusted_PBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PNP','adjusted_TCP','adjusted_DMP','adjusted_DMTP','adjusted_DMDP','adjusted_DEP','adjusted_DETP','adjusted_DEDP'), 
                              obs=pcv2data_obs,
                              N=pcv2data_first$n,
                              Percent=pcv2data_second$percent,
                              min=round(pcv2data_third,2),
                              Quantile=round(pcv2data_quantile,2),
                              max=round(pcv2data_last,2)
)


# pgv3
pgv3data<- pest %>% 
  filter(Timepoint=="pgv3")
attach(pgv3data)

## obs COLUMN
obs_CRE<- pgv3data %>% 
  filter(is.na(CRE)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adPBA<- pgv3data %>% 
  filter(is.na(adjusted_PBA)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adFPBA<- pgv3data %>% 
  filter(is.na(adjusted_FPBA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adCIS_DCCA<- pgv3data %>% 
  filter(is.na(adjusted_CIS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTRANS_DCCA<- pgv3data %>% 
  filter(is.na(adjusted_TRANS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPCP<- pgv3data %>% 
  filter(is.na(adjusted_PCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPNP<- pgv3data %>% 
  filter(is.na(adjusted_PNP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTCP<- pgv3data %>% 
  filter(is.na(adjusted_TCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMP<- pgv3data %>% 
  filter(is.na(adjusted_DMP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMTP<- pgv3data %>% 
  filter(is.na(adjusted_DMTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMDP<- pgv3data %>% 
  filter(is.na(adjusted_DMDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEP<- pgv3data %>% 
  filter(is.na(adjusted_DEP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDETP<- pgv3data %>% 
  filter(is.na(adjusted_DETP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEDP<- pgv3data %>% 
  filter(is.na(adjusted_DEDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))

pgv3data_obs<- rbind(obs_CRE,obs_adPBA,obs_adFPBA,obs_adCIS_DCCA, obs_adTRANS_DCCA,obs_adPCP,obs_adPNP,obs_adTCP, obs_adDMP,obs_adDMTP, obs_adDMDP, obs_adDEP, obs_adDETP, obs_adDEDP)

## FIRST COLUMN
n1_CRE<- pgv3data %>% 
  filter(is.na(CRE)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adPBA<- pgv3data %>% 
  filter(is.na(adjusted_PBA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adFPBA<- pgv3data %>% 
  filter(is.na(adjusted_FPBA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adCIS_DCCA<- pgv3data %>% 
  filter(is.na(adjusted_CIS_DCCA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adTRANS_DCCA<- pgv3data %>% 
  filter(is.na(adjusted_TRANS_DCCA)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adPCP<- pgv3data %>% 
  filter(is.na(adjusted_PCP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adPNP<- pgv3data %>% 
  filter(is.na(adjusted_PNP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adTCP<- pgv3data %>% 
  filter(is.na(adjusted_TCP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDMP<- pgv3data %>% 
  filter(is.na(adjusted_DMP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDMTP<- pgv3data %>% 
  filter(is.na(adjusted_DMTP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDMDP<- pgv3data %>% 
  filter(is.na(adjusted_DMDP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDEP<- pgv3data %>% 
  filter(is.na(adjusted_DEP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDETP<- pgv3data %>% 
  filter(is.na(adjusted_DETP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))
n1_adDEDP<- pgv3data %>% 
  filter(is.na(adjusted_DEDP)==FALSE) %>% 
  summarise(n=length(unique(Subject_ID)))

pgv3data_first<- rbind(n1_CRE,n1_adPBA,n1_adFPBA,n1_adCIS_DCCA, n1_adTRANS_DCCA, n1_adPCP, n1_adPNP, n1_adTCP, n1_adDMP, n1_adDMTP, n1_adDMDP, n1_adDEP, n1_adDETP, n1_adDEDP)


## SECOND COLUMN
LOD_PBA<- pgv3data %>% 
  filter(PBA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adPBA, 2))
LOD_FPBA<- pgv3data %>% 
  filter(FPBA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adFPBA,2))
LOD_CIS_DCCA<- pgv3data %>% 
  filter(CIS_DCCA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adCIS_DCCA,2))
LOD_TRANS_DCCA<- pgv3data %>% 
  filter(TRANS_DCCA_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adTRANS_DCCA,2))
LOD_PCP<- pgv3data %>% 
  filter(PCP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adPCP,2))
LOD_PNP<- pgv3data %>% 
  filter(PNP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adPNP,2))
LOD_TCP<- pgv3data %>% 
  filter(TCP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adTCP,2))
LOD_DMP<- pgv3data %>% 
  filter(DMP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDMP,2))
LOD_DMTP<- pgv3data %>% 
  filter(DMTP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDMTP,2))
LOD_DMDP<- pgv3data %>% 
  filter(DMDP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDMDP,2))
LOD_DEP<- pgv3data %>% 
  filter(DEP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDEP,2))
LOD_DETP<- pgv3data %>% 
  filter(DETP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDETP,2))
LOD_DEDP<- pgv3data %>% 
  filter(DEDP_Comment==0) %>% 
  summarise(percent=percent(length(Subject_ID)/obs_adDEDP,2))


pgv3data_second<- rbind(0, LOD_PBA, LOD_FPBA, LOD_CIS_DCCA, LOD_TRANS_DCCA, LOD_PCP, LOD_PNP, LOD_TCP, LOD_DMP, LOD_DMTP, LOD_DMDP, LOD_DEP, LOD_DETP, LOD_DEDP)


## THIRD COLUMN

pgv3data_third<- rbind(min(CRE,na.rm=TRUE),
                       min(adjusted_PBA, na.rm=TRUE),
                       min(adjusted_FPBA, na.rm=TRUE),
                       min(adjusted_CIS_DCCA, na.rm=TRUE),
                       min(adjusted_TRANS_DCCA, na.rm=TRUE),
                       min(adjusted_PCP, na.rm=TRUE),
                       min(adjusted_PNP, na.rm=TRUE),
                       min(adjusted_TCP, na.rm=TRUE),
                       min(adjusted_DMP, na.rm=TRUE),
                       min(adjusted_DMTP, na.rm=TRUE),
                       min(adjusted_DMDP, na.rm=TRUE),
                       min(adjusted_DEP, na.rm=TRUE),
                       min(adjusted_DETP, na.rm=TRUE),
                       min(adjusted_DEDP, na.rm=TRUE))

## QUANTILE COLOMN
pgv3data_quantile<- rbind(quantile(CRE, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_PBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_FPBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_CIS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_TRANS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_PCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_PNP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_TCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DMP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DMTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DMDP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DETP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(adjusted_DEDP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pgv3data_last<- rbind(max(CRE,na.rm=TRUE),
                      max(adjusted_PBA, na.rm=TRUE),
                      max(adjusted_FPBA, na.rm=TRUE),
                      max(adjusted_CIS_DCCA, na.rm=TRUE),
                      max(adjusted_TRANS_DCCA, na.rm=TRUE),
                      max(adjusted_PCP, na.rm=TRUE),
                      max(adjusted_PNP, na.rm=TRUE),
                      max(adjusted_TCP, na.rm=TRUE),
                      max(adjusted_DMP, na.rm=TRUE),
                      max(adjusted_DMTP, na.rm=TRUE),
                      max(adjusted_DMDP, na.rm=TRUE),
                      max(adjusted_DEP, na.rm=TRUE),
                      max(adjusted_DETP, na.rm=TRUE),
                      max(adjusted_DEDP, na.rm=TRUE))

pgv3data_overall<- data.frame(Name_of_pesticides=c("CRE",'adjusted_PBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PNP','adjusted_TCP','adjusted_DMP','adjusted_DMTP','adjusted_DMDP','adjusted_DEP','adjusted_DETP','adjusted_DEDP'), 
                              obs=pgv3data_obs,
                              N=pgv3data_first$n,
                              Percent=pgv3data_second$percent,
                              min=round(pgv3data_third,2),
                              Quantile=round(pgv3data_quantile,2),
                              max=round(pgv3data_last,2)
)


pest_timecor_CRE<- pest %>% 
  do(., wilcox.test(.$CRE~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adPBA<- pest %>% 
  do(., wilcox.test(.$adjusted_PBA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adFPBA<- pest %>% 
  do(., wilcox.test(.$adjusted_FPBA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adCIS_DCCA<- pest %>% 
  do(., wilcox.test(.$adjusted_CIS_DCCA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adTRANS_DCCA<- pest %>% 
  do(., wilcox.test(.$adjusted_TRANS_DCCA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adPCP<- pest %>% 
  do(., wilcox.test(.$adjusted_PCP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adPNP<- pest %>% 
  do(., wilcox.test(.$adjusted_PNP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adTCP<- pest %>% 
  do(., wilcox.test(.$adjusted_TCP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDMP<- pest %>% 
  do(., wilcox.test(.$adjusted_DMP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDMTP<- pest %>% 
  do(., wilcox.test(.$adjusted_DMTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDMDP<- pest %>% 
  do(., wilcox.test(.$adjusted_DMDP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDEP<- pest %>% 
  do(., wilcox.test(.$adjusted_DEP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDETP<- pest %>% 
  do(., wilcox.test(.$adjusted_DETP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDEDP<- pest %>% 
  do(., wilcox.test(.$adjusted_DEDP~.$Timepoint) %>% tidy) %>%
  select(p.value)

pest_timecor<-rbind(pest_timecor_CRE,pest_timecor_adPBA,pest_timecor_adFPBA, 
                    pest_timecor_adCIS_DCCA, pest_timecor_adTRANS_DCCA,pest_timecor_adPCP, 
                    pest_timecor_adPNP,pest_timecor_adTCP, pest_timecor_adDMP,pest_timecor_adDMTP, 
                    pest_timecor_adDMDP, pest_timecor_adDEP, pest_timecor_adDETP,pest_timecor_adDEDP)


## initial table
pest_data_rela_data<- data.frame(Name_of_pesticides=c("CRE",'adjusted_PBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PNP','adjusted_TCP','adjusted_DMP','adjusted_DMTP','adjusted_DMDP','adjusted_DEP','adjusted_DETP','adjusted_DEDP'), 
                             Timecor=round(pest_timecor,3))
pest_data_rela<- flextable(pest_data_rela_data) %>% 
                 theme_box()
pest_data_rela


pcv2_data<- pcv2data_overall %>% 
  mutate(TIME="pcv2")  


pgv3_data<- pgv3data_overall %>% 
  mutate(TIME="pgv3")  
pest_data<- rbind(pcv2_data, pgv3_data)

pest_data$Name_of_pesticides<- factor(pest_data$Name_of_pesticides,
                                      levels = c("CRE",'adjusted_PBA','adjusted_FPBA','adjusted_CIS_DCCA','adjusted_TRANS_DCCA','adjusted_PCP','adjusted_PNP','adjusted_TCP','adjusted_DMP','adjusted_DMTP','adjusted_DMDP','adjusted_DEP','adjusted_DETP','adjusted_DEDP'))

pest_group<- c(NA, NA, "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites","PYR metabolites", "PYR metabolites", "OC pesticides", "OC pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides")


ovarall_data<- data.frame(pest_data %>% 
                          arrange(Name_of_pesticides, TIME),
                          Group = pest_group)

ovarall_data$Name_of_pesticides<- factor(ovarall_data$Name_of_pesticides,
                                      levels = c('CRE', 'adjusted_DEP', 'adjusted_DMP', 'adjusted_DETP', 'adjusted_DMTP', 'adjusted_TCP', 'adjusted_PNP', 'adjusted_DEDP', 'adjusted_DMDP', 'adjusted_PBA', 'adjusted_TRANS_DCCA', 'adjusted_CIS_DCCA',  'adjusted_FPBA', 'adjusted_PCP'))


flextable(ovarall_data %>% arrange(Name_of_pesticides)) %>% 
  theme_box()



