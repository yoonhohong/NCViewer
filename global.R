
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

setwd("/Users/hong/Documents/GitHub/NCViewer/NCSdata")
# df = read_excel("2020 NCS DATA ver1.1.xlsx", sheet = 1,
#                 col_types = c("text", "date", rep("text", 3),
#                            rep("numeric", 113)))
# df_selected = df[1,] %>%
#     mutate_if(is.numeric, as.integer)
# weird date or text or numeric import ???

df = read_excel("BRM_GBS_NCS_2010_2017.xlsx", sheet = 1,
                col_types = c("text", "text", rep("text", 3),
                              rep("numeric", 113)))
df_selected = df %>%
  filter(ID == "1456214")

ptTable = df %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(Hosp, ID, Name, Date)

df_motor = df_selected %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate_if(is.numeric, as.integer) %>%
  gather(key = "side.nerve.param", 
           value = "value", c(R.MM.DML:R.TM.FL, L.MM.DML:L.TM.FL)) %>%
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
    mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
df_motor_B = df_motor_all  %>%
    filter(param == "FL") %>%
    mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL")) %>%
    mutate(cutoff = ifelse(is.na(value), "Not elicited", cutoff))
df_motor_C = df_motor_all  %>%
    filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                        "NCV1", "NCV2", "NCV3")) %>%
    mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL"))
df_motor_all = rbind(df_motor_A, df_motor_B, df_motor_C)

# tile view -> ncviewer.R

# Parameter View; angular axis = parameter, category = nerve
motor_radial <- df_motor_all 
motor_radial <- data.frame(motor_radial) %>%
  filter(param %in% c("CMAP1", "CMAP2",  
                      "DML", "Dur1", "Dur2",
                      "NCV1", "FL")) %>%
  mutate(param = factor(param)) 
df_motor_radial = motor_radial

lay = function(x) {
  x %>% 
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(-50, (round(
            max(df_motor_radial$value, na.rm = T)/50)+1)*50)
        ), 
        angularaxis = list(
          tickfont = list(size = 20)
        )
      ),
      legend = list(font = list(size = 20), x = 100, y = 0.5)
    )
}

df_motor_radial %>%
  group_by(side.nerve) %>%
  arrange(param) %>%
  plot_ly(type = 'scatterpolar') %>%
  add_trace(r = ~value, 
            theta = ~param, 
            name = ~side.nerve,
            mode = 'lines+markers') %>%
  add_trace(r = 100, 
            theta = ~param, 
            name = "ULN(LLN)", 
            line = list(dash = "dot")) %>%
  lay() 

df_motor_radial %>%
  group_by(param) %>%
  arrange(side.nerve) %>%
  plot_ly(type = 'scatterpolar') %>%
  add_trace(r = ~value, 
            theta = ~side.nerve, 
            name = ~param, 
            mode = 'lines+markers') %>%
  add_trace(r = 100, 
            theta = ~side.nerve, 
            name = "ULN(LLN)",
            line = list(dash = 'dot'))  %>%
  lay()


# Hadden's criteria 
df = df_motor_radial %>%
  arrange(Date) %>%
  filter(Date == first(Date))

# DML 
df = df %>% select(-cutoff)
df = spread(df, key = param, value = value)
DML = df %>%
  group_by(side.nerve) %>%
  summarize(dml = case_when(
    is.na(DML) ~ "NA", 
    DML <=100 ~ "NL", 
    (CMAP1 >=100 & DML >110)|(CMAP1 <100 & DML >120) ~ "PD", 
    TRUE ~ "ND")) 
NCV = df %>%
  group_by(side.nerve) %>%
  summarise(ncv = case_when(
    is.na(NCV1) ~ "NA",
    NCV1 >100 ~ "NL",
    (CMAP1 >=50 & NCV1 <90)|(CMAP1 <50 & NCV1 <85) ~ "PD", 
    TRUE ~ "ND"))  
CB = df %>%
  group_by(side.nerve) %>%
  summarize(cb = case_when(
    is.na(CMAP1)|CMAP1==0 ~ "NA",
    CMAP2/CMAP1 >=0.5 ~ "NL",
    CMAP2/CMAP1 <0.5 & CMAP1 >=20 ~ "PD",
    TRUE ~ "ND"))
# exclude the tibial nerve 
CB$cb[4] = ifelse(CB$cb[4] == "NA", CB$cb[4], "ND") # R.TM
CB$cb[5] = ifelse(CB$cb[5] == "NA", CB$cb[5], "ND") # L.TM
FL = df %>%
  group_by(side.nerve) %>%
  summarize(fl = case_when(
    is.na(CMAP1) ~ "NA",
    is.na(FL) & CMAP1 >0 ~ "FA", 
    FL <=100 ~ "NL",
    FL >120 ~ "PD", 
    TRUE ~ "ND"))
df_table = data.frame(rbind(DML$dml, NCV$ncv, CB$cb, FL$fl))
colnames(df_table) = df$side.nerve
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



