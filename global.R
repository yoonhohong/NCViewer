
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

setwd("/Users/hong/Documents/GitHub/NCViewer/NCSdata")
df = read_excel("2020 NCS DATA ver1.1.xlsx", sheet = 1,
                col_types = c("text", "date", rep("text", 3),
                           rep("numeric", 113)))

df = read_excel("BRM_GBS_NCS_2010_2017.xlsx", sheet = 1,
                col_types = c("text", "text", rep("text", 3),
                              rep("numeric", 113)))
# weird date or text or numeric import ???

ptTable = df %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    select(Hosp, ID, Name, Date)

# df_selected = df[1,] %>%
#     mutate_if(is.numeric, as.integer)

df_selected = df %>%
  filter(ID == "1456214")

tab_motor = df_selected %>%
    gather(key = "side.nerve.param", 
           value = "value", c(R.MM.DML:R.TM.FL, L.MM.DML:L.TM.FL)) %>%
    separate(side.nerve.param, 
             into = c("side", "nerve", "param"), 
             sep = "\\.") %>%
    mutate(side.nerve = paste(side, nerve, sep=".")) %>%
    mutate(side.nerve = factor(side.nerve, 
                               levels = 
                                 c("R.MM", "R.UM", "R.PM", "R.TM", 
                                   "L.TM", "L.PM", "L.UM", "L.MM"))) %>%
    filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4",  
                        "DML", "Dur1", "Dur2", "Dur3", "Dur4",
                        "NCV1", "NCV2", "NCV3", "FL")) %>%
    mutate(param = factor(param, 
                          levels = c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                                     "DML", "Dur1", "Dur2", "Dur3", "Dur4", 
                                     "NCV1", "NCV2", "NCV3", "FL"))) %>%
    select(Date, side.nerve, param, value)

  
tab_motor_A = tab_motor %>%
    filter(param %in% c("DML", "Dur1", "Dur2", "Dur3", "Dur4")) %>%
    mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
  
tab_motor_B = tab_motor %>%
    filter(param == "FL") %>%
    mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL")) %>%
    mutate(cutoff = ifelse(is.na(value), "Not elicited", cutoff))
  
tab_motor_C = tab_motor %>%
    filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                        "NCV1", "NCV2", "NCV3")) %>%
    mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL"))
  
tab_motor_all = rbind(tab_motor_A, tab_motor_B, tab_motor_C)
df_motor_all = tab_motor_all

# tile view 
temp = df_motor_all %>%
  group_by(side.nerve) %>%
  filter(!all(is.na(value))) 
temp$cutoff = factor(temp$cutoff)
p <- ggplot(temp, aes(x=side.nerve, y=param, 
                      fill = cutoff)) + 
  geom_tile(color = "black") + 
  geom_text(aes(label = value), size = 8) + theme_minimal() + 
  theme(axis.text.x = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 16, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size = 16, face = "bold"))

if (length(levels(temp$cutoff)) == 4) {
  p <- p + scale_fill_manual(values = c("red", "green", "black", "grey"), 
                             name = "")
} else {
  p <- p + scale_fill_manual(values = c("red", "green", "grey"), 
                             name = "")
} 
p

# Parameter View; angular axis = parameter, category = nerve

motor_radial <- df_motor_all 



motor_radial <- data.frame(motor_radial) %>%
  filter(param %in% c("CMAP1", "CMAP2",  
                      "DML", "Dur1", "Dur2",
                      "NCV1", "FL")) %>%
  mutate(param = factor(param)) 

# %>%
#   mutate(side.nerve = factor(side.nerve))
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
    ), 
    shapes=list(type='line', x0= 0, x1= max(.$Date), 
                y0=100, y1=100, 
                line=list(dash='dot', width=1)))

df_motor_radial_temp = df_motor_radial %>%
  group_by(side.nerve) %>%
  mutate(all_na = all(is.na(value))) %>%
  filter(all_na == F) 

df_motor_radial_temp$side.nerve = factor(df_motor_radial_temp$side.nerve)


len_nerve = length(levels(df_motor_radial_temp$side.nerve))
len_param = length(levels(df_motor_radial_temp$param))

p = df_motor_radial_temp %>%
  group_by(side.nerve) %>%
  do(p = spg_plot_param(.)) %>%
  subplot(nrows = round(len_nerve/3), shareY = T, shareX = T, 
          titleX = F, titleY = T) 
p1 = style(p, traces = 1:len_param, showlegend = T)
layout(p1, legend = list(font = list(size = 15)))

spg_plot_nerve = . %>%
  plot_ly(x = ~Date, y = ~value, color = ~side.nerve, 
          legendgroup = ~side.nerve, 
          colors = "Dark2") %>%
  add_lines(name = ~side.nerve, showlegend = F) %>%
  add_markers(showlegend = F) %>%
  add_annotations(
    text = ~unique(param),
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
    ), 
    shapes=list(type='line', x0= 0, x1= max(.$Date), 
                y0=100, y1=100, 
                line=list(dash='dot', width=1)))

p = df_motor_radial %>%
  group_by(param) %>%
  do(p = spg_plot_nerve(.)) %>%
  subplot(nrows = round(len_param/3), shareY = T, shareX = T, 
          titleX = F, titleY = T)
p1 = style(p, traces = 1:len_nerve, showlegend = T)
layout(p1, legend = list(font = list(size = 15)))
      
# Hadden's criteria 

df = df_motor_radial %>%
  arrange(Date) %>%
  filter(Date == first(Date))

# DML 

df = df %>% select(-c(cutoff, Date))
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

FL = df %>%
  group_by(side.nerve) %>%
  summarize(fl = case_when(
    is.na(CMAP1) ~ "NA",
    is.na(FL) & CMAP1 >0 ~ "FA", 
    FL <=100 ~ "NL",
    FL >120 ~ "PD", 
    TRUE ~ "ND"))
              
DML = DML$dml
NCV = NCV$ncv
CB = CB$cb
FL = FL$fl
df_table = data.frame(rbind(DML, NCV, CB, FL))
colnames(df_table) = df$side.nerve
df_table$param = c("DML", "NCV", "CB", "FL")

df_cnt = apply(df_table, 2, function(x){length(x[x=="PD"])})
length(df_cnt[df_cnt>=1])>=2

df_table_long = gather(df_table, key = "side.nerve", 
                       value = "feature", R.MM:L.MM) %>%
  mutate(feature = factor(feature)) %>%
  mutate(param = factor(param))
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
  theme(axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size = 14, face = "bold"))

p <- p + scale_fill_manual(
  values = c("green", "red", "blue", "orange", "grey"), 
                             name = "")
p

# CIDP 







