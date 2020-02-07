library(readxl)
setwd("/Users/hong/Documents/GitHub/NCViewer/NCSdata")
df = read_excel("2020 NCS DATA ver1.1.xlsx", sheet = 1, 
                col_types = c("text", "date", rep("text", 3), 
                           rep("numeric", 113)))

ptTable = df %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    select(Hosp, ID, Name, Date)

df_selected = df[1,] %>%
    mutate_if(is.numeric, as.integer)


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
    select(side.nerve, param, value)
  
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
tab_motor_all

temp = tab_motor_all %>%
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
