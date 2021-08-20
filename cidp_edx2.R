# CIDP 
# CIDP EDX criteria (updated 2021) 에 포함된 demyelinating parameter 각각에 대해서 
# 각 기준을 만족하는 nerve 개수를 구함. 

setwd("/Users/hong/Dropbox/NCViewer/Dataset")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

df = read_excel("BRM_NCS_2020.xlsx", sheet = 1,
                col_types = c("text", "text", rep("text", 3), 
                              rep("numeric", 113))) 
# age and 112 ncs parameters

df = df %>%
  mutate_if(is.numeric, as.integer)

dur = df %>%
  gather(key = "side.nerve.param", value = "value", R.MM.DML:L.SS.NCV1) %>%
  filter(str_detect(side.nerve.param, "Dur"))

others = df %>%
  gather(key = "side.nerve.param", value = "value", R.MM.DML:L.SS.NCV1) %>%
  filter(str_detect(side.nerve.param, "Dur", negate = T))

# cmap duration criteria have changed in 2021 criteria 
mm = dur %>%
  filter(str_detect(side.nerve.param, "MM")) %>%
  mutate(value = value*6.6/8.4)
um = dur %>%
  filter(str_detect(side.nerve.param, "UM")) %>%
  mutate(value = value*6.7/9.6)
pm = dur %>%
  filter(str_detect(side.nerve.param, "PM")) %>%
  mutate(value = value*7.6/8.8)
tm = dur %>%
  filter(str_detect(side.nerve.param, "TM")) %>%
  mutate(value = value*8.8/9.2)

dur = rbind(mm, um, pm, tm)
df = rbind(dur, others)
df = distinct(df)
df = spread(df, key = "side.nerve.param", value = "value")

ptTable = df %>% 
  mutate(Date = ymd(Date)) %>%
  select(Hosp, Date, Name, ID, Sex, Age)

i = grep("임혜민", df$Name)
i = i[1]

list_out = list()

for (i in 1:dim(df)[1]) {
  
  df_long = df[i,] %>%
    gather(key = "side.nerve.param", 
           value = "value", 
           L.MM.CMAP1:R.US.SNAP3) %>% 
    separate(side.nerve.param, 
             into = c("side", "nerve", "param"), 
             sep = "\\.") %>%
    mutate(side.nerve = paste(side, nerve, sep=".")) %>%
    mutate(side.nerve = factor(side.nerve, 
                               levels = 
                                 c("R.MM", "R.UM", "R.PM", "R.TM", 
                                   "L.TM", "L.PM", "L.UM", "L.MM", 
                                   "R.MS", "R.US", "R.SS",
                                   "L.SS", "L.US", "L.MS"))) %>%
    mutate(param = factor(param, 
                          levels = c("CMAP1", "CMAP2", "CMAP3", "CMAP4",  
                                     "DML", "Dur1", "Dur2", "Dur3", "Dur4",
                                     "NCV1", "NCV2", "NCV3", "NCV4", "FL", 
                                     "SNAP1", "SNAP2", "SNAP3", "SNAP4"
                          ))) %>%
    select(side.nerve, param, value)
  
  # split motor and sensory nerves 
  motor_long = df_long %>%
    filter(str_detect(side.nerve, "MM|UM|PM|TM")) 
  motor_wide = spread(motor_long, key = param, value = value)
  
  sensory_long = df_long %>%
    filter(str_detect(side.nerve, "MS|US|SS"))
  sensory_wide = spread(sensory_long, key = param, value = value)
  
  # SNAP 
  SNAP = sensory_wide %>%
    group_by(side.nerve) %>%
    summarise(snap = case_when(
      is.na(SNAP1) ~ NA, 
      SNAP1 < 100 ~ T, 
      TRUE ~ F))
  
  # SNCV 
  SNCV = sensory_wide %>%
    group_by(side.nerve) %>%
    summarise(sncv = case_when(
      is.na(NCV1) ~ NA,
      NCV1 < 100 ~ T,
      TRUE ~ F))
  
  # DML 
  DML = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(dml = case_when(
      is.na(DML) ~ NA, 
      DML >=150 ~ T, 
      TRUE ~ F)) 
  
  # NCV 
  NCV1 = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(ncv1 = case_when(
      is.na(NCV1) ~ NA,
      NCV1 <=70 ~ T, 
      TRUE ~ F))  
  NCV2 = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(ncv2 = case_when(
      is.na(NCV2) ~ NA,
      NCV2 <=70 ~ T, 
      TRUE ~ F))  
  NCV3 = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(ncv3 = case_when(
      is.na(NCV3) ~ NA,
      NCV3 <=70 ~ T, 
      TRUE ~ F))  
  
  # FL
  FL = motor_wide %>%
    group_by(side.nerve) %>%
    summarize(fl = case_when(
      is.na(CMAP1) ~ NA, 
      (FL >= 120 & CMAP1 >=80)|(FL >= 150 & CMAP1 <80) ~ T, 
      TRUE ~ F))
  
  # FA
  FA = motor_wide %>%
    group_by(side.nerve) %>%
    summarize(fa = case_when(
      is.na(CMAP1) ~ NA,
      is.na(FL) & CMAP1 >=20 ~ T, 
      TRUE ~ F))
  
  # CB
  CB1 = motor_wide %>%
    group_by(side.nerve) %>%
    summarize(cb1 = case_when(
      is.na(CMAP1) | CMAP1 == 0 ~ NA,
      CMAP2/CMAP1 <= 0.3 & CMAP1 >=20 ~ T,
      TRUE ~ F))

  CB2 = motor_wide %>%
    group_by(side.nerve) %>%
    summarize(cb2 = case_when(
      is.na(CMAP1) | is.na(CMAP3) | CMAP1 == 0  ~ NA,
      CMAP3/CMAP1 <= 0.3 & CMAP1 >=20 ~ T,
      TRUE ~ F))
  
  CB3 = motor_wide %>%
    group_by(side.nerve) %>%
    summarize(cb3 = case_when(
      is.na(CMAP1) | is.na(CMAP4) | CMAP1==0 ~ NA,
      CMAP4/CMAP1 <= 0.3 & CMAP1 >=20 ~ T,
      TRUE ~ F))
  
  TD1 = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(td1 = case_when(
      is.na(CMAP1) | CMAP1==0 ~ NA,
      Dur2/Dur1 >1.3 ~ T,
      TRUE ~ F))
  
  TD2 = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(td2 = case_when(
      is.na(CMAP1) ~ NA,
      is.na(CMAP3) ~ NA,
      CMAP1==0 ~ NA,
      Dur3/Dur1 >1.3 ~ T,
      TRUE ~ F))
  
  TD3 = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(td3 = case_when(
      is.na(CMAP1) ~ NA,
      is.na(CMAP4) ~ NA,
      Dur4/Dur1 >1.3 ~ T,
      TRUE ~ F))
  
  DUR = motor_wide %>%
    group_by(side.nerve) %>%
    summarise(dur = case_when(
      is.na(CMAP1) ~ NA,
      CMAP1==0 ~ NA,
      Dur1 >=100 ~ T, 
      TRUE ~ F))
  
  motor_wide_table = data.frame(rbind(DML$dml, 
                                   NCV1$ncv1, NCV2$ncv2, NCV3$ncv3, 
                                   FL$fl, FA$fa, 
                                   CB1$cb1, CB2$cb2, CB3$cb3, 
                                   TD1$td1, TD2$td2, TD3$td3, 
                                   DUR$dur))
  colnames(motor_wide_table) = motor_wide$side.nerve
  motor_wide_table$param = c("DML", "NCV1", "NCV2", "NCV3", 
                          "FL", "FA", "CB1", "CB2", "CB3", 
                          "TD1", "TD2", "TD3", 
                          "DUR")
  sensory_wide_table = data.frame(rbind(SNAP$snap, 
                                        SNCV$sncv))
  colnames(sensory_wide_table) = sensory_wide$side.nerve
  sensory_wide_table$param = c("SNAP", "SNCV")

    # decision if it fulfills cidp edx criteria 
  mat = as.matrix(motor_wide_table[,1:8])
  rownames(mat) = motor_wide_table$param
  
  mat_sensory = as.matrix(sensory_wide_table[,1:6])
  rownames(mat_sensory) = sensory_wide_table$param
  
  SNAP = mat_sensory[1,]
  SNCV = mat_sensory[2,]
  
  DML = mat[1,]
  
  NCV = mat[c(2,3,4),]
  temp2 = c()
  for (j in 1:8) {
    temp2[j] = ifelse(TRUE %in% NCV[,j], 1, 0)
    temp2[j] = ifelse(all(is.na(NCV[,j])), NA, temp2[j])
  }
  MNCV = temp2 
  
  FL = mat[5,]
  FA = mat[6,]
  
  CB = mat[c(7,8,9),]
  temp2 = c()
  for (k in 1:8) {
    temp2[k] = ifelse(TRUE %in% CB[,k], 1, 0)
    temp2[k] = ifelse(all(is.na(CB[,k])), NA, temp2[k])
  }
  CB = temp2 
  
  TD = mat[c(10,11,12),]
  temp2 = c()
  for (m in 1:8) {
    temp2[m] = ifelse(TRUE %in% TD[,m], 1, 0)
    temp2[m] = ifelse(all(is.na(TD[,m])), NA, temp2[m])
    }
  TD = temp2 
  
  DUR = mat[13,]
  
  temp3 = rbind(DML, MNCV, FL, FA, CB, TD, DUR)
  temp4 = rowSums(temp3, na.rm = T)
  temp4df = t(data.frame(temp4))
  
  temp5 = rbind(SNAP, SNCV)
  temp6 = rowSums(temp5, na.rm = T)
  temp6df = t(data.frame(temp6))
  
  list_out[[i]] = cbind(data.frame(ptTable)[i,], temp4df, temp6df)
}

temp = list_out[[1]]
for (i in 2:length(list_out)){
  temp = rbind(temp, list_out[[i]])
}

edx_2020 = temp

# write.csv(edx_2020, "BRM_NCS_2020_CIDP_Criteria.csv", 
#           row.names = F, quote = F, fileEncoding = "UTF-8")

library(xlsx)
write.xlsx(edx_2020, "BRM_NCS_2020_CIDP_EDX_2.xlsx", 
           sheetName = "Sheet1",
           col.names = T, row.names = F)
