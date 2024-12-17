# fine and gray는 r 터진다.. #
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

generation = c('0-19','20-39','40-59','60-79','more80') # 다 필요
data$ageg4 <- ifelse(data$age<=3,1,
                     ifelse(data$age<=7,2,
                            ifelse(data$age<=11,3,
                                   ifelse(data$age<=15,4,5))))
generation = c('20s','30s','40s','50s','60s','70s','80s')# 다 필요
data$ageg4 <- ifelse(data$age<=3,0,
                     ifelse(data$age<=5,1,
                            ifelse(data$age<=7,2,
                                   ifelse(data$age<=9,3,
                                          ifelse(data$age<=11,4,
                                                 ifelse(data$age<=13,5,
                                                        ifelse(data$age<=15,6,
                                                               ifelse(data$age<=17,7,8))))))))
generation = c('60s','more70') # [2]만 필요
data$ageg4 <- ifelse(data$age<=11,0,
                     ifelse(data$age<=13,1,2))
generation = c('more60s') # [1]만 필요
data$ageg4 <- ifelse(data$age<=11,1,2)

generation = c('before40') # [1]만 필요
data$ageg4 <- ifelse(data$age<=7,1,2)


#################################
### competing risk regression ###
#################################

# cuminc------
cuminc_plot = function(df, age_num){
  data <- read_sas(df)
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

  data$age <- data$first_c_age
  # # origin ver 
  # data$ageg4 <- ifelse(data$age<=3,0,
  #                      ifelse(data$age<=7,1,
  #                             ifelse(data$age<=11,2,
  #                                    ifelse(data$age<=15,3,4))))
  ### changed option(below) ### 
  generation = c('before40') # [1]만 필요
  data$ageg4 <- ifelse(data$age<=7,1,2)
  
  
  data$ageg4 <- as.numeric(data$ageg4)
  data <- data[data$ageg4==age_num,]
  
  data_now <- data
  data_now$status <- factor(data_now$status)
  
  km <- prodlim(Hist(time_yr, status, cens.code=0) ~ drug_group, data=data_now)
  pdf(paste0("1107_cuminc_",name,"_age",generation[age_num],".pdf"),height=7,width=10)
  plot(km, 
       xlim=c(0,10),
       ylim=c(0,0.1), ### changed option
       legend.x="topleft",
       legend.cex=1,
       legend.title="",
       atrisk=FALSE,
       background.horizontal = "white",
       col=c("red","blue"),
       xlab="Time (years)",
       axis2.at=seq(0,0.1,0.025), ### changed option
       axis1.at=seq(0,10,1)
  )
  dev.off()
}
# 적은 나이 0.1
# 중간대 나이는 0.2
# 많은 나이 0.8쪽
for (i in 1:1) ### changed option
{
  cuminc_plot('all_diag_1_15_land.sas7bdat', age_num=i)
  cuminc_plot('all_diag_1_30_land.sas7bdat', age_num=i)
  cuminc_plot('all_diag_1_60_land.sas7bdat', age_num=i)

  cuminc_plot('all_diag_2_15_land.sas7bdat', age_num=i)
  cuminc_plot('all_diag_2_30_land.sas7bdat', age_num=i)
  cuminc_plot('all_diag_2_60_land.sas7bdat', age_num=i)

  cuminc_plot('all_diag_3_15_land.sas7bdat', age_num=i)
  cuminc_plot('all_diag_3_30_land.sas7bdat', age_num=i)
  cuminc_plot('all_diag_3_60_land.sas7bdat', age_num=i)
}


# cause-specific hazard model------
cause_specific = function(df, age_num){
  print(df)
  data <- read_sas(df)
  
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
  # data$ageg4 <- ifelse(data$age<=3,0,
  #                      ifelse(data$age<=7,1,
  #                             ifelse(data$age<=11,2,
  #                                    ifelse(data$age<=15,3,4))))
  data$ageg4 <- ifelse(data$age<=7,0,
                       ifelse(data$age<=9,1, # 40s
                              ifelse(data$age<=11,2,3))) # 50s # 60s
  data$ageg4 <- as.numeric(data$ageg4)
  data <- data[data$ageg4==age_num,]
  

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
    Surv(time_yr, ifelse(status == 1, 1, 0)) ~ as.factor(drug_group) + SEX_TP_CD,
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
    Surv(time_yr, ifelse(status == 1, 1, 0)) ~ as.factor(drug_group) + SEX_TP_CD + as.factor(c8_yes) + as.factor(c9_yes) + as.factor(mi_yes) + as.factor(chf_yes) + as.factor(pvd_yes) + as.factor(cvd_yes) + as.factor(cpd_yes) + as.factor(rhe_yes) + as.factor(pud_yes) + as.factor(ld_yes) + as.factor(diab_yes) + as.factor(hp_yes) + as.factor(rd_yes) + as.factor(aids_yes),
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
for (i in 3:3) ### 끊어서 돌려야 됨.
{
  cause_specific('all_diag_1_15_land.sas7bdat', age_num=i)
  cause_specific('all_diag_1_30_land.sas7bdat', age_num=i)
  cause_specific('all_diag_1_60_land.sas7bdat', age_num=i)
  
  cause_specific('all_diag_2_15_land.sas7bdat', age_num=i)
  cause_specific('all_diag_2_30_land.sas7bdat', age_num=i)
  cause_specific('all_diag_2_60_land.sas7bdat', age_num=i)
  
  cause_specific('all_diag_3_15_land.sas7bdat', age_num=i)
  cause_specific('all_diag_3_30_land.sas7bdat', age_num=i)
  cause_specific('all_diag_3_60_land.sas7bdat', age_num=i)
}




