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
library('psych')


#------------------------------------------------
# Phthalates
## long format
phtha <- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/chemical/phtha_adjusted_CRE.xlsx")

phtha_raw<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Phthalates_long_20230317.xlsx")

phtha_ID<- phtha_raw$Subject_ID

phtha<- phtha %>% 
  filter(Subject_ID %in% phtha_ID) 

# pcv2
pcv2data<- phtha %>% 
  filter(Timepoint=="pcv2")

attach(pcv2data)

## obs COLUMN
obs_adMEP<- pcv2data %>% 
  filter(is.na(MEP)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adMCPP<- pcv2data %>% 
  filter(is.na(MCPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBZP<- pcv2data %>% 
  filter(is.na(MBZP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBP<- pcv2data %>% 
  filter(is.na(MBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMIBP<- pcv2data %>% 
  filter(is.na(MIBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHP<- pcv2data %>% 
  filter(is.na(MEHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHP<- pcv2data %>% 
  filter(is.na(MEOHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHP<- pcv2data %>% 
  filter(is.na(MEHHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPP<- pcv2data %>% 
  filter(is.na(MECPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCIOP<- pcv2data %>% 
  filter(is.na(MCIOP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCINP<- pcv2data %>% 
  filter(is.na(MCINP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHTP<- pcv2data %>% 
  filter(is.na(MEHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHTP<- pcv2data %>% 
  filter(is.na(MEOHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHTP<- pcv2data %>% 
  filter(is.na(MEHHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPTP<- pcv2data %>% 
  filter(is.na(MECPTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
pcv2data_obs<- rbind(obs_adMEP,obs_adMCPP,obs_adMBZP, obs_adMBP,obs_adMIBP,obs_adMEHP,obs_adMEOHP, obs_adMEHHP,obs_adMECPP, obs_adMCIOP, obs_adMCINP, obs_adMEHTP, obs_adMEOHTP,obs_adMEHHTP,obs_adMECPTP)


## first COLUMN
pcv2data_first<- rbind(mean(MEP, na.rm=TRUE),
                       mean(MCPP, na.rm=TRUE),
                       mean(MBZP, na.rm=TRUE),
                       mean(MBP, na.rm=TRUE),
                       mean(MIBP, na.rm=TRUE),
                       mean(MEHP, na.rm=TRUE),
                       mean(MEOHP, na.rm=TRUE),
                       mean(MEHHP, na.rm=TRUE),
                       mean(MECPP, na.rm=TRUE),
                       mean(MCIOP, na.rm=TRUE),
                       mean(MCINP, na.rm=TRUE),
                       mean(MEHTP, na.rm=TRUE),
                       mean(MEOHTP, na.rm=TRUE),
                       mean(MEHHTP, na.rm=TRUE),
                       mean(MECPTP, na.rm=TRUE))

## GM COLUMN
pcv2data_GM<- rbind(geometric.mean(MEP, na.rm=TRUE),
                    geometric.mean(MCPP, na.rm=TRUE),
                    geometric.mean(MBZP, na.rm=TRUE),
                    geometric.mean(MBP, na.rm=TRUE),
                    geometric.mean(MIBP, na.rm=TRUE),
                    geometric.mean(MEHP, na.rm=TRUE),
                    geometric.mean(MEOHP, na.rm=TRUE),
                    geometric.mean(MEHHP, na.rm=TRUE),
                    geometric.mean(MECPP, na.rm=TRUE),
                    geometric.mean(MCIOP, na.rm=TRUE),
                    geometric.mean(MCINP, na.rm=TRUE),
                    geometric.mean(MEHTP, na.rm=TRUE),
                    geometric.mean(MEOHTP, na.rm=TRUE),
                    geometric.mean(MEHHTP, na.rm=TRUE),
                    geometric.mean(MECPTP, na.rm=TRUE))




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

pcv2data_third<- rbind(min(MEP, na.rm=TRUE),
                       min(MCPP, na.rm=TRUE),
                       min(MBZP, na.rm=TRUE),
                       min(MBP, na.rm=TRUE),
                       min(MIBP, na.rm=TRUE),
                       min(MEHP, na.rm=TRUE),
                       min(MEOHP, na.rm=TRUE),
                       min(MEHHP, na.rm=TRUE),
                       min(MECPP, na.rm=TRUE),
                       min(MCIOP, na.rm=TRUE),
                       min(MCINP, na.rm=TRUE),
                       min(MEHTP, na.rm=TRUE),
                       min(MEOHTP, na.rm=TRUE),
                       min(MEHHTP, na.rm=TRUE),
                       min(MECPTP, na.rm=TRUE))

## QUANTILE COLOMN
pcv2data_quantile<- rbind(quantile(MEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MCPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MBZP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MIBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEOHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MECPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MCIOP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MCINP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEOHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MECPTP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pcv2data_last<- rbind(max(MEP, na.rm=TRUE),
                      max(MCPP, na.rm=TRUE),
                      max(MBZP, na.rm=TRUE),
                      max(MBP, na.rm=TRUE),
                      max(MIBP, na.rm=TRUE),
                      max(MEHP, na.rm=TRUE),
                      max(MEOHP, na.rm=TRUE),
                      max(MEHHP, na.rm=TRUE),
                      max(MECPP, na.rm=TRUE),
                      max(MCIOP, na.rm=TRUE),
                      max(MCINP, na.rm=TRUE),
                      max(MEHTP, na.rm=TRUE),
                      max(MEOHTP, na.rm=TRUE),
                      max(MEHHTP, na.rm=TRUE),
                      max(MECPTP, na.rm=TRUE))

pcv2data_overall<- data.frame(Name_of_phthalates=c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'),
                              obs=pcv2data_obs,
                              Percent=pcv2data_second$percent,
                              min=round(pcv2data_third,2),
                              mean = round(pcv2data_first,2),
                              GM = round(pcv2data_GM,2),
                              Quantile=round(pcv2data_quantile,2),
                              max=round(pcv2data_last,2)
)


# pgv3
pgv3data<- phtha %>% 
  filter(Timepoint=="pgv3")
attach(pgv3data)

## obs COLUMN
obs_adMEP<- pgv3data %>% 
  filter(is.na(MEP)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adMCPP<- pgv3data %>% 
  filter(is.na(MCPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBZP<- pgv3data %>% 
  filter(is.na(MBZP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMBP<- pgv3data %>% 
  filter(is.na(MBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMIBP<- pgv3data %>% 
  filter(is.na(MIBP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHP<- pgv3data %>% 
  filter(is.na(MEHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHP<- pgv3data %>% 
  filter(is.na(MEOHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHP<- pgv3data %>% 
  filter(is.na(MEHHP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPP<- pgv3data %>% 
  filter(is.na(MECPP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCIOP<- pgv3data %>% 
  filter(is.na(MCIOP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMCINP<- pgv3data %>% 
  filter(is.na(MCINP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHTP<- pgv3data %>% 
  filter(is.na(MEHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEOHTP<- pgv3data %>% 
  filter(is.na(MEOHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMEHHTP<- pgv3data %>% 
  filter(is.na(MEHHTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adMECPTP<- pgv3data %>% 
  filter(is.na(MECPTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
pgv3data_obs<- rbind(obs_adMEP,obs_adMCPP,obs_adMBZP, obs_adMBP,obs_adMIBP,obs_adMEHP,obs_adMEOHP, obs_adMEHHP,obs_adMECPP, obs_adMCIOP, obs_adMCINP, obs_adMEHTP, obs_adMEOHTP,obs_adMEHHTP,obs_adMECPTP)


## first COLUMN
pcv3data_first<- rbind(mean(MEP, na.rm=TRUE),
                       mean(MCPP, na.rm=TRUE),
                       mean(MBZP, na.rm=TRUE),
                       mean(MBP, na.rm=TRUE),
                       mean(MIBP, na.rm=TRUE),
                       mean(MEHP, na.rm=TRUE),
                       mean(MEOHP, na.rm=TRUE),
                       mean(MEHHP, na.rm=TRUE),
                       mean(MECPP, na.rm=TRUE),
                       mean(MCIOP, na.rm=TRUE),
                       mean(MCINP, na.rm=TRUE),
                       mean(MEHTP, na.rm=TRUE),
                       mean(MEOHTP, na.rm=TRUE),
                       mean(MEHHTP, na.rm=TRUE),
                       mean(MECPTP, na.rm=TRUE))

## GM COLUMN
pcv3data_GM<- rbind(geometric.mean(MEP, na.rm=TRUE),
                    geometric.mean(MCPP, na.rm=TRUE),
                    geometric.mean(MBZP, na.rm=TRUE),
                    geometric.mean(MBP, na.rm=TRUE),
                    geometric.mean(MIBP, na.rm=TRUE),
                    geometric.mean(MEHP, na.rm=TRUE),
                    geometric.mean(MEOHP, na.rm=TRUE),
                    geometric.mean(MEHHP, na.rm=TRUE),
                    geometric.mean(MECPP, na.rm=TRUE),
                    geometric.mean(MCIOP, na.rm=TRUE),
                    geometric.mean(MCINP, na.rm=TRUE),
                    geometric.mean(MEHTP, na.rm=TRUE),
                    geometric.mean(MEOHTP, na.rm=TRUE),
                    geometric.mean(MEHHTP, na.rm=TRUE),
                    geometric.mean(MECPTP, na.rm=TRUE))



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

pgv3data_third<- rbind(min(MEP, na.rm=TRUE),
                       min(MCPP, na.rm=TRUE),
                       min(MBZP, na.rm=TRUE),
                       min(MBP, na.rm=TRUE),
                       min(MIBP, na.rm=TRUE),
                       min(MEHP, na.rm=TRUE),
                       min(MEOHP, na.rm=TRUE),
                       min(MEHHP, na.rm=TRUE),
                       min(MECPP, na.rm=TRUE),
                       min(MCIOP, na.rm=TRUE),
                       min(MCINP, na.rm=TRUE),
                       min(MEHTP, na.rm=TRUE),
                       min(MEOHTP, na.rm=TRUE),
                       min(MEHHTP, na.rm=TRUE),
                       min(MECPTP, na.rm=TRUE))

## QUANTILE COLOMN
pgv3data_quantile<- rbind(quantile(MEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MCPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MBZP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MIBP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEOHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHHP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MECPP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MCIOP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MCINP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEOHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MEHHTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(MECPTP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pgv3data_last<- rbind(max(MEP, na.rm=TRUE),
                      max(MCPP, na.rm=TRUE),
                      max(MBZP, na.rm=TRUE),
                      max(MBP, na.rm=TRUE),
                      max(MIBP, na.rm=TRUE),
                      max(MEHP, na.rm=TRUE),
                      max(MEOHP, na.rm=TRUE),
                      max(MEHHP, na.rm=TRUE),
                      max(MECPP, na.rm=TRUE),
                      max(MCIOP, na.rm=TRUE),
                      max(MCINP, na.rm=TRUE),
                      max(MEHTP, na.rm=TRUE),
                      max(MEOHTP, na.rm=TRUE),
                      max(MEHHTP, na.rm=TRUE),
                      max(MECPTP, na.rm=TRUE))

pgv3data_overall<- data.frame(Name_of_phthalates=c('MEP','MCPP','MBZP','MBP','MIBP','MEHP','MEOHP','MEHHP','MECPP','MCIOP','MCINP','MEHTP','MEOHTP','MEHHTP','MECPTP'),
                              obs=pgv3data_obs,
                              Percent=pgv3data_second$percent,
                              min=round(pgv3data_third,2),
                              mean = round(pcv2data_first,2),
                              GM = round(pcv2data_GM,2),
                              Quantile=round(pgv3data_quantile,2),
                              max=round(pgv3data_last,2)
)


Timecor_adMEP<- phtha %>% 
  do(., wilcox.test(.$MEP~.$Timepoint) %>% tidy) %>%
  select(p.value)

Timecor_adMCPP<- phtha %>% 
  do(., wilcox.test(.$MCPP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMBZP<- phtha %>% 
  do(., wilcox.test(.$MBZP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMBP<- phtha %>% 
  do(., wilcox.test(.$MBP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMIBP<- phtha %>% 
  do(., wilcox.test(.$MIBP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHP<- phtha %>% 
  do(., wilcox.test(.$MEHP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEOHP<- phtha %>% 
  do(., wilcox.test(.$MEOHP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHHP<- phtha %>% 
  do(., wilcox.test(.$MEHHP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMECPP<- phtha %>% 
  do(., wilcox.test(.$MECPP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMCIOP<- phtha %>% 
  do(., wilcox.test(.$MCIOP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMCINP<- phtha %>% 
  do(., wilcox.test(.$MCINP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHTP<- phtha %>% 
  do(., wilcox.test(.$MEHTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEOHTP<- phtha %>% 
  do(., wilcox.test(.$MEOHTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMEHHTP<- phtha %>% 
  do(., wilcox.test(.$MEHHTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
Timecor_adMECPTP<- phtha %>% 
  do(., wilcox.test(.$MECPTP~.$Timepoint) %>% tidy) %>%
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
pest<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/chemical/pest_adjusted_CRE.xlsx")


pest_raw<- read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Projects/S-PRESTO/input/for paper/chemical_paper/prepared_Pesticides_long_20230317.xlsx")

pest_ID<- pest_raw$Subject_ID


pest<- pest %>% 
       filter(Subject_ID %in% pest_ID) 


# pcv2
pcv2data<- pest %>% 
  filter(Timepoint=="pcv2")
attach(pcv2data)

## obs COLUMN
obs_CRE<- pcv2data %>% 
  filter(is.na(CRE)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adPBA<- pcv2data %>% 
  filter(is.na(PBA)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adFPBA<- pcv2data %>% 
  filter(is.na(FPBA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adCIS_DCCA<- pcv2data %>% 
  filter(is.na(CIS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTRANS_DCCA<- pcv2data %>% 
  filter(is.na(TRANS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPCP<- pcv2data %>% 
  filter(is.na(PCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPNP<- pcv2data %>% 
  filter(is.na(PNP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTCP<- pcv2data %>% 
  filter(is.na(TCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMP<- pcv2data %>% 
  filter(is.na(DMP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMTP<- pcv2data %>% 
  filter(is.na(DMTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMDP<- pcv2data %>% 
  filter(is.na(DMDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEP<- pcv2data %>% 
  filter(is.na(DEP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDETP<- pcv2data %>% 
  filter(is.na(DETP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEDP<- pcv2data %>% 
  filter(is.na(DEDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))

pcv2data_obs<- rbind(obs_CRE,obs_adPBA,obs_adFPBA,obs_adCIS_DCCA, obs_adTRANS_DCCA,obs_adPCP,obs_adPNP,obs_adTCP, obs_adDMP,obs_adDMTP, obs_adDMDP, obs_adDEP, obs_adDETP, obs_adDEDP)

## first COLUMN

pcv2data_first<- rbind(mean(CRE,na.rm=TRUE),
                       mean(PBA, na.rm=TRUE),
                       mean(FPBA, na.rm=TRUE),
                       mean(CIS_DCCA, na.rm=TRUE),
                       mean(TRANS_DCCA, na.rm=TRUE),
                       mean(PCP, na.rm=TRUE),
                       mean(PNP, na.rm=TRUE),
                       mean(TCP, na.rm=TRUE),
                       mean(DMP, na.rm=TRUE),
                       mean(DMTP, na.rm=TRUE),
                       mean(DMDP, na.rm=TRUE),
                       mean(DEP, na.rm=TRUE),
                       mean(DETP, na.rm=TRUE),
                       mean(DEDP, na.rm=TRUE))

## GM COLUMN

pcv2data_GM<- rbind(geometric.mean(CRE,na.rm=TRUE),
                    geometric.mean(PBA, na.rm=TRUE),
                    geometric.mean(FPBA, na.rm=TRUE),
                    geometric.mean(CIS_DCCA, na.rm=TRUE),
                    geometric.mean(TRANS_DCCA, na.rm=TRUE),
                    geometric.mean(PCP, na.rm=TRUE),
                    geometric.mean(PNP, na.rm=TRUE),
                    geometric.mean(TCP, na.rm=TRUE),
                    geometric.mean(DMP, na.rm=TRUE),
                    geometric.mean(DMTP, na.rm=TRUE),
                    geometric.mean(DMDP, na.rm=TRUE),
                    geometric.mean(DEP, na.rm=TRUE),
                    geometric.mean(DETP, na.rm=TRUE),
                    geometric.mean(DEDP, na.rm=TRUE))

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
                       min(PBA, na.rm=TRUE),
                       min(FPBA, na.rm=TRUE),
                       min(CIS_DCCA, na.rm=TRUE),
                       min(TRANS_DCCA, na.rm=TRUE),
                       min(PCP, na.rm=TRUE),
                       min(PNP, na.rm=TRUE),
                       min(TCP, na.rm=TRUE),
                       min(DMP, na.rm=TRUE),
                       min(DMTP, na.rm=TRUE),
                       min(DMDP, na.rm=TRUE),
                       min(DEP, na.rm=TRUE),
                       min(DETP, na.rm=TRUE),
                       min(DEDP, na.rm=TRUE))

## QUANTILE COLOMN
pcv2data_quantile<- rbind(quantile(CRE, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(FPBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(CIS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(TRANS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PNP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(TCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DMP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DMTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DMDP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DETP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DEDP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pcv2data_last<- rbind(max(CRE,na.rm=TRUE),
                      max(PBA, na.rm=TRUE),
                      max(FPBA, na.rm=TRUE),
                      max(CIS_DCCA, na.rm=TRUE),
                      max(TRANS_DCCA, na.rm=TRUE),
                      max(PCP, na.rm=TRUE),
                      max(PNP, na.rm=TRUE),
                      max(TCP, na.rm=TRUE),
                      max(DMP, na.rm=TRUE),
                      max(DMTP, na.rm=TRUE),
                      max(DMDP, na.rm=TRUE),
                      max(DEP, na.rm=TRUE),
                      max(DETP, na.rm=TRUE),
                      max(DEDP, na.rm=TRUE))

pcv2data_overall<- data.frame(Name_of_pesticides=c("CRE",'PBA','FPBA','CIS_DCCA','TRANS_DCCA','PCP','PNP','TCP','DMP','DMTP','DMDP','DEP','DETP','DEDP'), 
                              obs=pcv2data_obs,
                              Percent=pcv2data_second$percent,
                              min=round(pcv2data_third,2),
                              mean = round(pcv2data_first,2),
                              GM = round(pcv2data_GM,2),
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
  filter(is.na(PBA)==FALSE) %>% 
  summarise(n=length(Subject_ID))
obs_adFPBA<- pgv3data %>% 
  filter(is.na(FPBA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adCIS_DCCA<- pgv3data %>% 
  filter(is.na(CIS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTRANS_DCCA<- pgv3data %>% 
  filter(is.na(TRANS_DCCA)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPCP<- pgv3data %>% 
  filter(is.na(PCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adPNP<- pgv3data %>% 
  filter(is.na(PNP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adTCP<- pgv3data %>% 
  filter(is.na(TCP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMP<- pgv3data %>% 
  filter(is.na(DMP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMTP<- pgv3data %>% 
  filter(is.na(DMTP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDMDP<- pgv3data %>% 
  filter(is.na(DMDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEP<- pgv3data %>% 
  filter(is.na(DEP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDETP<- pgv3data %>% 
  filter(is.na(DETP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))
obs_adDEDP<- pgv3data %>% 
  filter(is.na(DEDP)==FALSE) %>% 
  summarise(n=length((Subject_ID)))

pgv3data_obs<- rbind(obs_CRE,obs_adPBA,obs_adFPBA,obs_adCIS_DCCA, obs_adTRANS_DCCA,obs_adPCP,obs_adPNP,obs_adTCP, obs_adDMP,obs_adDMTP, obs_adDMDP, obs_adDEP, obs_adDETP, obs_adDEDP)

## first COLUMN

pcv3data_first<- rbind(mean(CRE,na.rm=TRUE),
                       mean(PBA, na.rm=TRUE),
                       mean(FPBA, na.rm=TRUE),
                       mean(CIS_DCCA, na.rm=TRUE),
                       mean(TRANS_DCCA, na.rm=TRUE),
                       mean(PCP, na.rm=TRUE),
                       mean(PNP, na.rm=TRUE),
                       mean(TCP, na.rm=TRUE),
                       mean(DMP, na.rm=TRUE),
                       mean(DMTP, na.rm=TRUE),
                       mean(DMDP, na.rm=TRUE),
                       mean(DEP, na.rm=TRUE),
                       mean(DETP, na.rm=TRUE),
                       mean(DEDP, na.rm=TRUE))

## GM COLUMN

pcv3data_GM<- rbind(geometric.mean(CRE,na.rm=TRUE),
                    geometric.mean(PBA, na.rm=TRUE),
                    geometric.mean(FPBA, na.rm=TRUE),
                    geometric.mean(CIS_DCCA, na.rm=TRUE),
                    geometric.mean(TRANS_DCCA, na.rm=TRUE),
                    geometric.mean(PCP, na.rm=TRUE),
                    geometric.mean(PNP, na.rm=TRUE),
                    geometric.mean(TCP, na.rm=TRUE),
                    geometric.mean(DMP, na.rm=TRUE),
                    geometric.mean(DMTP, na.rm=TRUE),
                    geometric.mean(DMDP, na.rm=TRUE),
                    geometric.mean(DEP, na.rm=TRUE),
                    geometric.mean(DETP, na.rm=TRUE),
                    geometric.mean(DEDP, na.rm=TRUE))


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
                       min(PBA, na.rm=TRUE),
                       min(FPBA, na.rm=TRUE),
                       min(CIS_DCCA, na.rm=TRUE),
                       min(TRANS_DCCA, na.rm=TRUE),
                       min(PCP, na.rm=TRUE),
                       min(PNP, na.rm=TRUE),
                       min(TCP, na.rm=TRUE),
                       min(DMP, na.rm=TRUE),
                       min(DMTP, na.rm=TRUE),
                       min(DMDP, na.rm=TRUE),
                       min(DEP, na.rm=TRUE),
                       min(DETP, na.rm=TRUE),
                       min(DEDP, na.rm=TRUE))

## QUANTILE COLOMN
pgv3data_quantile<- rbind(quantile(CRE, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(FPBA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(CIS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(TRANS_DCCA, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(PNP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(TCP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DMP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DMTP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DMDP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DEP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DETP, c(.05,.25,.50,.75,.95), na.rm=TRUE),
                          quantile(DEDP, c(.05,.25,.50,.75,.95), na.rm=TRUE))

## LAST COLUMN
pgv3data_last<- rbind(max(CRE,na.rm=TRUE),
                      max(PBA, na.rm=TRUE),
                      max(FPBA, na.rm=TRUE),
                      max(CIS_DCCA, na.rm=TRUE),
                      max(TRANS_DCCA, na.rm=TRUE),
                      max(PCP, na.rm=TRUE),
                      max(PNP, na.rm=TRUE),
                      max(TCP, na.rm=TRUE),
                      max(DMP, na.rm=TRUE),
                      max(DMTP, na.rm=TRUE),
                      max(DMDP, na.rm=TRUE),
                      max(DEP, na.rm=TRUE),
                      max(DETP, na.rm=TRUE),
                      max(DEDP, na.rm=TRUE))

pgv3data_overall<- data.frame(Name_of_pesticides=c("CRE",'PBA','FPBA','CIS_DCCA','TRANS_DCCA','PCP','PNP','TCP','DMP','DMTP','DMDP','DEP','DETP','DEDP'), 
                              obs=pgv3data_obs,
                              Percent=pgv3data_second$percent,
                              min=round(pgv3data_third,2),
                              mean = round(pcv3data_first,2),
                              GM = round(pcv3data_GM,2),
                              Quantile=round(pgv3data_quantile,2),
                              max=round(pgv3data_last,2)
)


pest_timecor_CRE<- pest %>% 
  do(., wilcox.test(.$CRE~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adPBA<- pest %>% 
  do(., wilcox.test(.$PBA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adFPBA<- pest %>% 
  do(., wilcox.test(.$FPBA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adCIS_DCCA<- pest %>% 
  do(., wilcox.test(.$CIS_DCCA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adTRANS_DCCA<- pest %>% 
  do(., wilcox.test(.$TRANS_DCCA~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adPCP<- pest %>% 
  do(., wilcox.test(.$PCP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adPNP<- pest %>% 
  do(., wilcox.test(.$PNP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adTCP<- pest %>% 
  do(., wilcox.test(.$TCP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDMP<- pest %>% 
  do(., wilcox.test(.$DMP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDMTP<- pest %>% 
  do(., wilcox.test(.$DMTP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDMDP<- pest %>% 
  do(., wilcox.test(.$DMDP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDEP<- pest %>% 
  do(., wilcox.test(.$DEP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDETP<- pest %>% 
  do(., wilcox.test(.$DETP~.$Timepoint) %>% tidy) %>%
  select(p.value)
pest_timecor_adDEDP<- pest %>% 
  do(., wilcox.test(.$DEDP~.$Timepoint) %>% tidy) %>%
  select(p.value)

pest_timecor<-rbind(pest_timecor_CRE,pest_timecor_adPBA,pest_timecor_adFPBA, 
                    pest_timecor_adCIS_DCCA, pest_timecor_adTRANS_DCCA,pest_timecor_adPCP, 
                    pest_timecor_adPNP,pest_timecor_adTCP, pest_timecor_adDMP,pest_timecor_adDMTP, 
                    pest_timecor_adDMDP, pest_timecor_adDEP, pest_timecor_adDETP,pest_timecor_adDEDP)


## initial table
pest_data_rela_data<- data.frame(Name_of_pesticides=c("CRE",'PBA','FPBA','CIS_DCCA','TRANS_DCCA','PCP','PNP','TCP','DMP','DMTP','DMDP','DEP','DETP','DEDP'), 
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
                                      levels = c("CRE",'PBA','FPBA','CIS_DCCA','TRANS_DCCA','PCP','PNP','TCP','DMP','DMTP','DMDP','DEP','DETP','DEDP'))

pest_group<- c(NA, NA, "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites", "PYR metabolites","PYR metabolites", "PYR metabolites", "OC pesticides", "OC pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides", "OP pesticides")


ovarall_data<- data.frame(pest_data %>% 
                            arrange(Name_of_pesticides, TIME),
                          Group = pest_group)

ovarall_data$Name_of_pesticides<- factor(ovarall_data$Name_of_pesticides,
                                         levels = c('CRE', 'DEP', 'DMP', 'DETP', 'DMTP', 'TCP', 'PNP', 'DEDP', 'DMDP', 'PBA', 'TRANS_DCCA', 'CIS_DCCA',  'FPBA', 'PCP'))


flextable(ovarall_data %>% arrange(Name_of_pesticides)) %>% 
  theme_box()



