# subgroup analysis - sex #
setwd('/vol/userdata9/sta_room241/')

library(haven)
library(sas7bdat)
library(data.table)
library(dplyr)
library(moonBook)
library(cmprsk)
library(survival)
library(broom)
library(knitr)
library(prodlim)

#################################
### competing risk regression ###
#################################

# cuminc------
cuminc_plot = function(df){
  data <- read_sas(df)
  data <- data[data$SEX_TP_CD == "2",] ###
  name <- strsplit(df,'.sas7bdat')[[1]]
  
  data$death_yn[is.na(data$death_yn)] = 0
  data$drug_group[is.na(data$drug_group)] = 0
  data$dem_outcome[is.na(data$dem_outcome)] = 0
  
  data$status <- ifelse(data$dem_outcome == 1, 1,
                        ifelse(data$death_yn == 1, 2, 0))
  
  data <- as.data.table(data)
  data[,dm_time:=ifelse(status==1&drug_group==1, as.numeric(first_dm_date - first_c_date),
                        ifelse(status==1&drug_group==0, as.numeric(first_dm_date - first_c_date), NA))]
  data[,time:=ifelse(status==1, dm_time, death_day)]
  data[,time_yr:=time/365.25]
  
  data_now <- data
  data_now$status <- factor(data_now$status)
  
  km <- prodlim(Hist(time_yr, status, cens.code=0) ~ drug_group, data=data_now)
  pdf(paste0("1105_cuminc_",name,"_female.pdf"),height=7,width=10) ###
  plot(km,
       xlim=c(0,10),
       ylim=c(0,0.2),
       legend.x="topleft",
       legend.cex=1,
       legend.title="",
       atrisk=FALSE,
       # background.horizontal = seq(0,0.2,0.05),
       background.horizontal = "white",
       col=c("red","blue"),
       xlab="Time (years)",
       # cex.axis=1.5,
       axis2.at=seq(0,0.2,0.05),
       axis1.at=seq(0,10,1)
  )
  # abline(v=5, col="grey", lty=2, lwd=2)
  # abline(v=10, col="grey", lty=2, lwd=2)
  dev.off()
}
cuminc_plot('all_diag_1_15_land.sas7bdat')
cuminc_plot('all_diag_1_30_land.sas7bdat')
cuminc_plot('all_diag_1_60_land.sas7bdat')

cuminc_plot('all_diag_2_15_land.sas7bdat')
cuminc_plot('all_diag_2_30_land.sas7bdat')
cuminc_plot('all_diag_2_60_land.sas7bdat')

cuminc_plot('all_diag_3_15_land.sas7bdat')
cuminc_plot('all_diag_3_30_land.sas7bdat')
cuminc_plot('all_diag_3_60_land.sas7bdat')


# cause-specific hazard model------
cause_specific = function(df){
  print(df)
  data <- read_sas(df)
  data <- data[data$SEX_TP_CD == "2",] ###
  
  data$death_yn[is.na(data$death_yn)] = 0
  data$drug_group[is.na(data$drug_group)] = 0
  data$dem_outcome[is.na(data$dem_outcome)] = 0
  
  data$status <- ifelse(data$dem_outcome == 1, 1,
                        ifelse(data$death_yn == 1, 2, 0))
  
  data <- as.data.table(data)
  data[,dm_time:=ifelse(status==1&drug_group==1, as.numeric(first_dm_date - first_c_date),
                        ifelse(status==1&drug_group==0, as.numeric(first_dm_date - first_c_date), NA))]
  data[,time:=ifelse(status==1, dm_time, death_day)]
  data[,time_yr:=time/365.25]
  
  data$age <- data$first_c_age
  data$ageg4 <- ifelse(data$age<=3,0,
                       ifelse(data$age<=7,1,
                              ifelse(data$age<=11,2,
                                     ifelse(data$age<=15,3,4))))
  data$ageg4 <- as.numeric(data$ageg4)
  

    ### uni result ###
  uni_model <- coxph(
    Surv(time_yr, ifelse(status == 1, 1, 0)) ~ as.factor(drug_group),
    data = data
  )  
  
  uni_tidy <- tidy(uni_model, exponentiate = TRUE, conf.int=TRUE) %>%
    mutate(
      HR = estimate,
      CI = sprintf("%.2f, %.2f", conf.low, conf.high),
      `p-value`=p.value
    ) %>% 
    select(term, HR, CI, `p-value`)
  
  print(kable(uni_tidy, col.names = c("Chracteristics","HR","95% CI", "p-value")))
  
  ### multi result - 1 ###
  multi_model1 <- coxph(
    Surv(time_yr, ifelse(status == 1, 1, 0)) ~ as.factor(drug_group) + ageg4,
    data = data
  ) 
  
  multi_tidy1 <- tidy(multi_model1, exponentiate = TRUE, conf.int=TRUE) %>%
    mutate(
      HR = estimate,
      CI = sprintf("%.2f, %.2f", conf.low, conf.high),
      `p-value`=p.value
    ) %>% 
    select(term, HR, CI, `p-value`)
  
  print(kable(multi_tidy1, col.names = c("Chracteristics","HR","95% CI", "p-value")))
  
  ### multi result - 2 ###
  multi_model2 <- coxph(
    Surv(time_yr, ifelse(status == 1, 1, 0)) ~ as.factor(drug_group) + ageg4 + as.factor(c8_yes) + as.factor(c9_yes) + as.factor(mi_yes) + as.factor(chf_yes) + as.factor(pvd_yes) + as.factor(cvd_yes) + as.factor(cpd_yes) + as.factor(rhe_yes) + as.factor(pud_yes) + as.factor(ld_yes) + as.factor(diab_yes) + as.factor(hp_yes) + as.factor(rd_yes) + as.factor(aids_yes),
    data = data
  ) 
  
  multi_tidy2 <- tidy(multi_model2, exponentiate = TRUE, conf.int=TRUE) %>%
    mutate(
      HR = estimate,
      CI = sprintf("%.2f, %.2f", conf.low, conf.high),
      `p-value`=p.value
    ) %>% 
    select(term, HR, CI, `p-value`)
  
  print(kable(multi_tidy2, col.names = c("Chracteristics","HR","95% CI", "p-value")))
}
print('type 1:')
cause_specific('all_diag_1_15_land.sas7bdat')
cause_specific('all_diag_1_30_land.sas7bdat')
cause_specific('all_diag_1_60_land.sas7bdat')

print('type 2:')
cause_specific('all_diag_2_15_land.sas7bdat')
cause_specific('all_diag_2_30_land.sas7bdat')
cause_specific('all_diag_2_60_land.sas7bdat')

print('type 3:')
cause_specific('all_diag_3_15_land.sas7bdat')
cause_specific('all_diag_3_30_land.sas7bdat')
cause_specific('all_diag_3_60_land.sas7bdat')





