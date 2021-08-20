
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

setwd("/Users/hong/Dropbox/NCViewer/Dataset")
# df = read_excel("2020 NCS DATA ver1.1.xlsx", sheet = 1,
#                 col_types = c("text", "date", rep("text", 3),
#                            rep("numeric", 113)))
# df_selected = df[1,] %>%
#     mutate_if(is.numeric, as.integer)
# weird date or text or numeric import ???

df = read_excel("BRM_GBS_NCS_2010_2017.xlsx", sheet = 1,
                col_types = c("text", "text", rep("text", 3), 
                              rep("numeric", 113))) # age and 112 ncs parameters
df_selected = df %>%
  filter(ID == "1456214")

ptTable = df %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(Hosp, ID, Name, Date)

df_motor = df_selected %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate_if(is.numeric, as.integer) %>%
  gather(key = "side.nerve.param", 
           value = "value", 
         c(R.MM.DML:R.TM.FL, L.MM.DML:L.TM.FL)) %>% # select only motor nerve parameters 
  separate(side.nerve.param, 
             into = c("side", "nerve", "param"), 
             sep = "\\.") %>%
  mutate(side.nerve = paste(side, nerve, sep=".")) %>%
  mutate(side.nerve = factor(side.nerve, 
                               levels = 
                                 c("R.MM", "R.UM", "R.PM", "R.TM", 
                                   "L.TM", "L.PM", "L.UM", "L.MM"))) 

df_motor_all = df_motor %>% 
    filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4",  
                        "DML", "Dur1", "Dur2", "Dur3", "Dur4",
                        "NCV1", "NCV2", "NCV3", "FL")) %>%
    mutate(param = factor(param, 
                          levels = c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                                     "DML", "Dur1", "Dur2", "Dur3", "Dur4", 
                                     "NCV1", "NCV2", "NCV3", "FL"))) %>%
    select(Date, side.nerve, param, value)

df_motor_A = df_motor_all  %>%
    filter(param %in% c("DML", "Dur1", "Dur2", "Dur3", "Dur4")) %>%
    mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
    mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
df_motor_B = df_motor_all  %>%
    filter(param == "FL") %>%
    mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
    mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL")) 
df_motor_C = df_motor_all  %>%
    filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                        "NCV1", "NCV2", "NCV3")) %>%
    mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
    mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL"))
df_motor_all = rbind(df_motor_A, df_motor_B, df_motor_C)
df = df_motor_all %>%
  arrange(Date) %>%
  filter(Date == first(Date))

# Tile view 

df$cutoff = factor(df$cutoff)
p <- ggplot(df, aes(x=factor(side.nerve), y=param, 
                      fill = cutoff)) + 
  geom_tile(color = "black") + 
  geom_text(aes(label = value), size = 8) + theme_minimal() + 
  theme(axis.text.x = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 16, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid = element_blank(),
        legend.text = element_text(size = 16, face = "bold")) + 
  scale_fill_manual(values = c("Above ULN" = "red", 
                               "Below LLN" = "green", 
                               "Not elicited" = "black", 
                               "WNL" = "grey"), 
                    name = "")
p

# Radial graph for parameter and nerve view 
motor_radial <- df
motor_radial <- motor_radial %>%
  filter(param %in% c("CMAP1", "CMAP2",  
                      "DML", "Dur1", "Dur2",
                      "NCV1", "FL")) %>% # select common parameters
  mutate(param = factor(param)) %>%
  mutate(side.nerve = factor(side.nerve)) 
df_motor_radial = motor_radial

lay = function(x) {
  x %>% 
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(-50, (round(
            max(df_motor_radial$value, na.rm = T)/50)+1)*50) # maximal 50 margin
        ), 
        angularaxis = list(
          tickfont = list(size = 20)
        )
      ),
      legend = list(font = list(size = 20), x = 100, y = 0.5)
    )
}

# Parameter View
# angular axis = parameter, category = nerve
df_motor_radial %>%
  group_by(side.nerve) %>%
  arrange(param) %>%
  plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
  add_trace(r = ~value, 
            theta = ~param, 
            name = ~side.nerve) %>%
  add_trace(r = 100, 
            theta = ~param, 
            name = "ULN(LLN)", 
            line = list(dash = "dot"), 
            fill = 'toself') %>%
  lay() 

# Nerve View
# angular axis = nerve, category = parameter
df_motor_radial %>%
  group_by(param) %>%
  arrange(side.nerve) %>%
  plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
  add_trace(r = ~value, 
            theta = ~side.nerve, 
            name = ~param) %>%
  add_trace(r = 100, 
            theta = ~side.nerve, 
            name = "ULN(LLN)",
            line = list(dash = 'dot'), 
            fill = "toself")  %>%
  lay() 


# Hadden's criteria 
# DML 
df_criteria = df %>% select(-cutoff)
df_criteria = spread(df_criteria, key = param, value = value)
DML = df_criteria %>%
  group_by(side.nerve) %>%
  summarize(dml = case_when(
    is.na(DML) ~ "NA", 
    DML <=100 ~ "NL", 
    (CMAP1 >=100 & DML >110)|(CMAP1 <100 & DML >120) ~ "PD", 
    TRUE ~ "ND")) 
NCV = df_criteria %>%
  group_by(side.nerve) %>%
  summarise(ncv = case_when(
    is.na(NCV1) ~ "NA",
    NCV1 >100 ~ "NL",
    (CMAP1 >=50 & NCV1 <90)|(CMAP1 <50 & NCV1 <85) ~ "PD", 
    TRUE ~ "ND"))  
CB = df_criteria %>%
  group_by(side.nerve) %>%
  summarize(cb = case_when(
    is.na(CMAP1)|CMAP1==0 ~ "NA",
    CMAP2/CMAP1 >=0.5 ~ "NL",
    CMAP2/CMAP1 <0.5 & CMAP1 >=20 ~ "PD",
    TRUE ~ "ND"))
# Exclude the tibial nerve 
CB$cb[4] = ifelse(CB$cb[4] == "NA", CB$cb[4], "ND") # R.TM
CB$cb[5] = ifelse(CB$cb[5] == "NA", CB$cb[5], "ND") # L.TM
FL = df_criteria %>%
  group_by(side.nerve) %>%
  summarize(fl = case_when(
    is.na(CMAP1) ~ "NA",
    is.na(FL) & CMAP1 >0 ~ "FA", # F-wave absence 
    FL <=100 ~ "NL",
    FL >120 ~ "PD", 
    TRUE ~ "ND"))
df_table = data.frame(rbind(DML$dml, NCV$ncv, CB$cb, FL$fl))
colnames(df_table) = df_criteria$side.nerve
df_table$param = c("DML", "NCV", "CB", "FL")


# Tile view of demyelinating features (Hadden's criteria)
df_table_long = gather(df_table, key = "side.nerve", 
                       value = "feature", R.MM:L.MM) %>%
  mutate(feature = factor(feature)) %>%
  mutate(param = factor(param)) %>%
  mutate(side.nerve = factor(side.nerve, 
                             levels = c("R.MM", "R.UM", "R.PM", "R.TM", 
                                        "L.TM", "L.PM", "L.UM", "L.MM")))
levels(df_table_long$feature) = list(Normal = "NL", 
                                     Primary_demyelinating = "PD", 
                                     Not_determined = "ND",
                                     F_absence = "FA",
                                     Not_available = "NA")
levels(df_table_long$param) = list(DML = "DML", 
                                   NCV = "NCV", 
                                   CB = "CB", 
                                   FL = "FL")

p <- ggplot(df_table_long, aes(x=side.nerve, y=param, 
                      fill = feature)) + 
  geom_tile(color = "black") + theme_minimal() + 
  labs(title = "Hadden's criteria") + 
  theme(axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        title = element_text(size = 16, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size = 14, face = "bold"))
p <- p + scale_fill_manual(
  values = c("green", "red", "blue", "orange", "grey"), 
                             name = "") 
p

df_table_long %>%
  group_by(side.nerve) %>%
  filter(feature == "Primary_demyelinating") %>%
  summarise(cnt = n()) -> temp 

dim(temp)[1]
paste("Number of nerve with at least 1 primary demyelinating features:",
      dim(temp)[1], sep = " ")

# CIDP 

# DML 
# df = long data format, Date, side.nerve, param, value and cutoff
df_cidp = df %>% select(-cutoff)
df_cidp = spread(df_cidp, key = param, value = value)
DML = df_cidp %>%
  group_by(side.nerve) %>%
  summarize(dml = case_when(
    is.na(DML) ~ NA, 
    DML >=150 ~ T, 
    TRUE ~ F)) 
# Exclude the Median nerve 
# DML$dml[1] = ifelse(DML$dml[1] == "NA", DML$dml[1], "not applicable") # R.MM
# DML$dml[8] = ifelse(DML$dml[8] == "NA", DML$dml[8], "not applicable") # L.MM

NCV1 = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(ncv1 = case_when(
    is.na(NCV1) ~ NA,
    NCV1 <=70 ~ T, 
    TRUE ~ F))  
NCV2 = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(ncv2 = case_when(
    is.na(NCV2) ~ NA,
    NCV2 <=70 ~ T, 
    TRUE ~ F))  
NCV3 = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(ncv3 = case_when(
    is.na(NCV3) ~ NA,
    NCV3 <=70 ~ T, 
    TRUE ~ F))  

FL = df_cidp %>%
  group_by(side.nerve) %>%
  summarize(fl = case_when(
    is.na(CMAP1) ~ NA,
    (FL >130 & CMAP1 >=80)|(FL >150 & CMAP1 <80) ~ T, 
    TRUE ~ F))

FA = df_cidp %>%
  group_by(side.nerve) %>%
  summarize(fa = case_when(
    is.na(CMAP1) ~ NA,
    is.na(FL) & CMAP1 >=20 ~ T, # F-wave absence
    TRUE ~ F))

CB1 = df_cidp %>%
  group_by(side.nerve) %>%
  summarize(cb1 = case_when(
    is.na(CMAP1) ~ NA,
    CMAP1 == 0 ~ NA,
    CMAP2/CMAP1 <0.5 & CMAP1 >=20 ~ T,
    TRUE ~ F))
# Exclude the tibial nerve 
# CB$cb[4] = ifelse(CB$cb[4] == "NA", CB$cb[4], "ND") # R.TM
# CB$cb[5] = ifelse(CB$cb[5] == "NA", CB$cb[5], "ND") # L.TM
CB2 = df_cidp %>%
  group_by(side.nerve) %>%
  summarize(cb2 = case_when(
    is.na(CMAP1) ~ NA,
    CMAP1==0 ~ NA,
    is.na(CMAP3) ~ NA,
    CMAP3/CMAP1 <0.5 & CMAP1 >=20 ~ T,
    TRUE ~ F))
CB3 = df_cidp %>%
  group_by(side.nerve) %>%
  summarize(cb3 = case_when(
    is.na(CMAP1) ~ NA,
    CMAP1==0 ~ NA,
    is.na(CMAP4) ~ NA,
    CMAP4/CMAP1 <0.5 & CMAP1 >=20 ~ T,
    TRUE ~ F))

TD1 = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(td1 = case_when(
    is.na(CMAP1) ~ NA,
    CMAP1==0 ~ NA,
    Dur2/Dur1 >1.3 ~ T,
    TRUE ~ F))
TD2 = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(td2 = case_when(
    is.na(CMAP1) ~ NA,
    is.na(CMAP3) ~ NA,
    CMAP1==0 ~ NA,
    Dur3/Dur1 >1.3 ~ T,
    TRUE ~ F))
TD3 = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(td3 = case_when(
    is.na(CMAP1) ~ NA,
    is.na(CMAP4) ~ NA,
    Dur4/Dur1 >1.3 ~ T,
    TRUE ~ F))

DUR = df_cidp %>%
  group_by(side.nerve) %>%
  summarise(dur = case_when(
    is.na(CMAP1) ~ NA,
    CMAP1==0 ~ NA,
    Dur1 >=100 ~ T, 
    TRUE ~ F))

df_cidp_table = data.frame(rbind(DML$dml, 
                            NCV1$ncv1, NCV2$ncv2, NCV3$ncv3, 
                            FL$fl, FA$fa, 
                            CB1$cb1, CB2$cb2, CB3$cb3, 
                            TD1$td1, TD2$td2, TD3$td3, 
                            DUR$dur))
colnames(df_cidp_table) = df_cidp$side.nerve
df_cidp_table$param = c("DML", "NCV1", "NCV2", "NCV3", 
                   "FL", "FA", "CB1", "CB2", "CB3", 
                   "TD1", "TD2", "TD3", 
                   "DUR")

# Tile view of demyelinating features (CIDP EDX criteria)
df_cidp_table_long = gather(df_cidp_table, key = "side.nerve", 
                       value = "feature", R.MM:L.MM) %>%
  mutate(param = factor(param)) %>%
  mutate(side.nerve = factor(side.nerve, 
                             levels = c("R.MM", "R.UM", "R.PM", "R.TM", 
                                        "L.TM", "L.PM", "L.UM", "L.MM")))
levels(df_cidp_table_long$param) = list(DML = "DML", 
                                   NCV1 = "NCV1", 
                                   NCV2 = "NCV2", 
                                   NCV3 = "NCV3", 
                                   FL = "FL",
                                   FA = "FA",
                                   CB1 = "CB1", 
                                   CB2 = "CB2", 
                                   CB3 = "CB3",
                                   TD1 = "TD1", 
                                   TD2 = "TD2", 
                                   TD3 = "TD3",
                                   DUR = "DUR"
                                   )

p <- ggplot(df_cidp_table_long, aes(x=side.nerve, y=param, 
                               fill = feature)) + 
  geom_tile(color = "black") + theme_minimal() + 
  labs(title = "CIDP EDX criteria") + 
  theme(axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        title = element_text(size = 16, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size = 14, face = "bold"))
p <- p + scale_fill_manual(
  values = c("green", "red", "grey"), 
  name = "") 
p


# FU view 
fu_selected = df_motor %>%
  filter(param %in% c("CMAP1", "CMAP2", 
                        "DML", "Dur1", "Dur2", 
                        "NCV1", "FL")) %>%
  mutate(param = factor(param)) %>%
  select(Date, side.nerve, param, value) %>%
  group_by(side.nerve) %>%
  mutate(all_na = all(is.na(value))) %>%
  filter(all_na == F) 

fu_selected$side.nerve = factor(fu_selected$side.nerve)
temp_date = data.frame(date = sort(unique(fu_selected$Date)))
data.frame(date = temp_date[c(1,2),])

spg_plot_param = . %>%
  plot_ly(x = ~Date, y = ~value, color = ~param, 
          legendgroup = ~param, 
          colors = "Dark2") %>%
  add_lines(name = ~param, showlegend = F) %>%
  add_markers(showlegend = F) %>%
  add_annotations(
    text = ~unique(side.nerve),
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "middle",
    yanchor = "top",
    showarrow = FALSE,
    font = list(size = 15)
  ) %>%
  layout(
    xaxis = list(
      showgrid = T
    ),
    yaxis = list(
      showgrid = T
    ))



