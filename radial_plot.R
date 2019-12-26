library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

## read all_wide_appended.csv, ncs percentage data, wide format 

df_wide = read.csv("NCSdata_BRM/all_wide_appended.csv")

# pick one record and make long data format 
df = gather(df_wide[1,], key = "side.nerve.param", value = "value", L.MM.CMAP1:R.UM.NCV3)

# split side.nerve.param into side.nerve and param
df = separate(df, side.nerve.param, into = c("side", "nerve", "param"),
         sep = "\\.")
df$side.nerve = paste(df$side, df$nerve, sep=".")

# type conversion: chr to factor
df$side.nerve = factor(df$side.nerve, 
                       levels = c("L.MM", "L.UM", "L.PM", "L.TM",
                           "R.TM", "R.PM", "R.UM", "R.MM"))
df$param = factor(df$param, 
                  levels = c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                             "DML", "DUR1", "DUR2", "DUR3", "DUR4",
                             "NCV1", "NCV2", "NCV3", "FL"))

# ncs data table 
df = subset(df, select = c(side.nerve, param, value))
df_tab = spread(df, key = side.nerve, value = value)
df_tab = select_if(df_tab, function(x){!all(is.na(x))})

## view
df_view = gather(df_tab, key = "side.nerve", value = "value", 
                 colnames(df_tab)[-1], factor_key = T)
df_view = filter(df_view, param %in% 
                   c("CMAP1", "CMAP2", "DML", "DUR1", "DUR2", 
                                  "NCV1", "FL"))
df_view$param = factor(df_view$param)
max.value = max(df_view$value, na.rm = T)
max.value = (round(max.value/50)+1)*50

# parameter view
p <- plot_ly(
  type = 'scatterpolar',
  mode = "lines+markers+texts",
  fill = 'none'
  ) 
p <- p %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max.value)
      )
    )
  )
for (i in 1:length(levels(df_view$param))) {
  temp = df_view %>%
    filter(param == levels(df_view$param)[i]) %>%
    select(side.nerve, value)
  p <- p %>% add_trace(
    r = c(temp[,2],temp[1,2]), # r: values in r-axes
    theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
    name = levels(df_view$param)[i] # name: record name 
  )}  
p

## nerve view
p <- plot_ly(
  type = 'scatterpolar',
  mode = "lines+markers+texts",
  fill = 'none'
) 
p <- p %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max.value)
      )
    )
  )
for (i in 1:length(levels(df_view$side.nerve))) {
  temp = df_view %>%
    filter(side.nerve == levels(df_view$side.nerve)[i]) %>%
    select(param, value)
  p <- p %>% add_trace(
    r = c(temp[,2],temp[1,2]), # r: values in r-axes
    theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
    name = levels(df_view$side.nerve)[i] # name: record name 
  )}  
p

# The End # 
