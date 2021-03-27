if (!requireNamespace("shiny")){
  install.packages("shiny")
}

if (!requireNamespace("readxl")){
  install.packages("readxl")
}

if (!requireNamespace("DT")){
  install.packages("DT")
}
if (!requireNamespace("dplyr")){
  install.packages("dplyr")
}
if (!requireNamespace("tidyr")){
  install.packages("tidyr")
}
if (!requireNamespace("plotly")){
  install.packages("plotly")
}

if (!requireNamespace("mgcv")){
  install.packages("mgcv")
}

library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(mgcv)

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


ui <- fluidPage(
  titlePanel("NCViewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "ncsfile", 
                label = "Choose NCS File",
                accept = c(".xlsx")),
      hr(),
      DTOutput(outputId = "ptTable")
      ), 
    mainPanel(
      tabsetPanel(
        tabPanel("Tile view", br(), plotOutput("tileView")), 
        tabPanel("Parameter view", br(), br(), plotlyOutput("paramView")), 
        tabPanel("Nerve view", br(), br(), plotlyOutput("nerveView")), 
        tabPanel("FU view", br(), 
                 DTOutput(outputId = "dateTable"), br(),
                 plotlyOutput("fuview")), 
        tabPanel("GBS criteria sets", br(), 
                 plotOutput("hadden_tileView")
                 ), 
        tabPanel("CIDP", br(),
                 plotOutput("cidp_tileView"))
      )
    )
  )
)

server <- function(input, output) {
  
  input_file = reactive({
    inFile = input$ncsfile
    
    if (is.null(inFile)) 
      return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    read_excel(paste(inFile$datapath, ext, sep = "."), sheet = 1, 
               col_types = c("text", "text", rep("text", 3), 
                             rep("numeric", 113)))
  })
    
  output$ptTable <- renderDT({
    if (is.null(input_file())) return(NULL) 
    input_file() %>% 
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
      select(Hosp, ID, Name, Date)
    }, rownames = F, selection = "single", options = list(
      pageLength = 10))
  
  df_selected = reactive({
    if (length(input$ptTable_rows_selected) == 0) return(NULL)
    df_selected = input_file()[input$ptTable_rows_selected,]
    df_selected %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
      mutate_if(is.numeric, as.integer)
    })

  
  df_motor = reactive({
    if (is.null(df_selected())) return(NULL)
    df_motor = df_selected() %>%
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
    df_motor
    })
  
  df_motor_all = reactive({
    if (is.null(df_motor())) return(NULL)
    df_motor_all = df_motor() %>%
      filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4",  
                          "DML", "Dur1", "Dur2", "Dur3", "Dur4",
                          "NCV1", "NCV2", "NCV3", "FL")) %>%
      mutate(param = factor(param, 
                            levels = c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                                       "DML", "Dur1", "Dur2", "Dur3", "Dur4", 
                                       "NCV1", "NCV2", "NCV3", "FL"))) %>%
      select(side.nerve, param, value)
    df_motor_A = df_motor_all %>%
      filter(param %in% c("DML", "Dur1", "Dur2", "Dur3", "Dur4")) %>%
      mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
      mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
    df_motor_B = df_motor_all %>%
      filter(param == "FL") %>%
      mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
      mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
    df_motor_C = df_motor_all %>%
      filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                          "NCV1", "NCV2", "NCV3")) %>%
      mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
      mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL"))
    df_motor_all = rbind(df_motor_A, df_motor_B, df_motor_C)
    df_motor_all
  })
    
  output$tileView = renderPlot({
    if (is.null(df_motor_all())) return(NULL) 
    temp = df_motor_all() 
    temp$cutoff = factor(temp$cutoff)
    p <- ggplot(temp, aes(x=factor(side.nerve), y=param, 
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
  })
  
  # parameter View; angular axis = parameter, category = nerve
  df_motor_radial = reactive({
    if (is.null(df_motor_all())) return(NULL) 
    motor_radial <- df_motor_all() 
    motor_radial <- data.frame(motor_radial) %>%
      filter(param %in% c("CMAP1", "CMAP2",  
                          "DML", "Dur1", "Dur2",
                          "NCV1", "FL")) %>%
      mutate(param = factor(param)) %>%
      mutate(side.nerve = factor(side.nerve)) 
    motor_radial
  })
  
  output$paramView = renderPlotly({
    if (is.null(df_motor_radial())) return(NULL) 
    p <- df_motor_radial() %>%
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
                line = list(dash = "dot"), 
                fill = "toself") %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(-50, (round(
              max(df_motor_radial()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    p
  })

# nerve View; angular axis = nerve, category = parameter 
  output$nerveView = renderPlotly({
    if (is.null(df_motor_radial())) return(NULL) 
    p <- df_motor_radial() %>%
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
                line = list(dash = 'dot'), 
                fill = "toself")  %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(-50, (round(
              max(df_motor_radial()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    p
  })

  # fu view 
  
  fu_selected = reactive({
    if (is.null(df_selected())) return(NULL)
    fu_selected = input_file()[input_file()$ID == df_selected()$ID, ]
    fu_selected = fu_selected %>%
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
                                     "L.TM", "L.PM", "L.UM", "L.MM"))) %>%
      filter(param %in% c("CMAP1", "CMAP2", 
                          "DML", "Dur1", "Dur2", 
                          "NCV1", "FL")) %>%
      mutate(param = factor(param, 
                            levels = c("CMAP1", "CMAP2",  
                                       "DML", "Dur1", "Dur2", 
                                       "NCV1", "FL"))) %>%
      select(Date, side.nerve, param, value) %>%
      group_by(side.nerve) %>%
      mutate(all_na = all(is.na(value))) %>%
      filter(all_na == F) 
    fu_selected$side.nerve = factor(fu_selected$side.nerve)
    fu_selected
  })
  
  df_date = reactive({
    if (is.null(fu_selected())) return(NULL) 
    df_date = data.frame(date = sort(unique(fu_selected()$Date)))
    df_date
  })
  
  output$dateTable <- renderDT({
    if (is.null(df_date())) return(NULL) 
    df_date()
  }, rownames = T, selection = "multiple", options = list(
    pageLength = 5))  
  
  output$fuview = renderPlotly({
    if (length(input$dateTable_rows_selected) == 0) return(NULL)
    temp_date = data.frame(date = df_date()[input$dateTable_rows_selected,])
    fu_selected_sub = fu_selected() %>%
      filter(Date %in% temp_date$date)
    p = fu_selected_sub %>%
      group_by(side.nerve) %>%
      do(p = spg_plot_param(.)) %>%
      subplot(nrows = 1, 
              shareY = T, shareX = T, 
              titleX = F, titleY = T) 
    p1 = style(p, traces = 1:7, showlegend = T)
    layout(p1, legend = list(font = list(size = 15)))
  })
  
  feature_table = reactive({
    if (is.null(df_motor_radial())) return(NULL) 
    df <- df_motor_radial() %>%
      select(-cutoff)
    df_wide = spread(df, key = param, value = value)
    DML = df_wide %>%
      group_by(side.nerve) %>%
      summarize(dml = case_when(
        is.na(DML) ~ "NA",
        DML <=100 ~ "NL",
        (CMAP1 >=100 & DML >110)|(CMAP1 <100 & DML >120) ~ "PD",
        TRUE ~ "ND"))
    NCV = df_wide %>%
      group_by(side.nerve) %>%
      summarise(ncv = case_when(
        is.na(NCV1) ~ "NA",
        NCV1 >100 ~ "NL",
        (CMAP1 >=50 & NCV1 <90)|(CMAP1 <50 & NCV1 <85) ~ "PD",
        TRUE ~ "ND"))
    CB = df_wide %>%
      group_by(side.nerve) %>%
      summarize(cb = case_when(
        is.na(CMAP1)|CMAP1==0 ~ "NA",
        CMAP2/CMAP1 >=0.5 ~ "NL",
        CMAP2/CMAP1 <0.5 & CMAP1 >=20 ~ "PD",
        TRUE ~ "ND"))
    # exclude the tibial nerve
    CB$cb[4] = ifelse(CB$cb[4] == "NA", CB$cb[4], "ND") # R.TM
    CB$cb[5] = ifelse(CB$cb[5] == "NA", CB$cb[5], "ND") # L.TM
    FL = df_wide %>%
      group_by(side.nerve) %>%
      summarize(fl = case_when(
        is.na(CMAP1) ~ "NA",
        is.na(FL) & CMAP1 >0 ~ "FA",
        FL <=100 ~ "NL",
        FL >120 ~ "PD",
        TRUE ~ "ND"))
    df_table = data.frame(rbind(DML$dml, NCV$ncv, CB$cb, FL$fl))
    colnames(df_table) = df_wide$side.nerve
    df_table$param = c("DML", "NCV", "CB", "FL")
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
    df_table_long
  })

  output$hadden_tileView = renderPlot({
    if (is.null(feature_table())) return(NULL)
    # Tile view of demyelinating features (Hadden's criteria)
    temp <- feature_table() %>%
      group_by(side.nerve) %>%
      filter(feature == "Primary_demyelinating") %>%
      summarise(cnt = n()) 
    p <- ggplot(feature_table(), aes(x=side.nerve, y=param,
                                   fill = feature)) +
      geom_tile(color = "black") + theme_minimal() +
      labs(title = "Hadden's criteria", 
           subtitle = paste("Number of nerve with at least 1 primary demyelinating features:",
                            dim(temp)[1], sep = " ")) +
      theme(axis.text.x = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 14, face = "bold"),
            title = element_text(size = 16, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_blank(),
            legend.text = element_text(size = 14, face = "bold"))
    p <- p + scale_fill_manual(
      values = c("Normal" = "green", "Primary_demyelinating" = "red", 
                 "Not_determined" = "blue", "F_absence" = "orange", 
                 "Not_available" = "grey"),
      name = "")
    p
  })
  
  feature_table_cidp = reactive({
    if (is.null(df_motor_all())) return(NULL) 
    df <- df_motor_all() %>%
      select(-cutoff)
    df_cidp = spread(df, key = param, value = value)
    
    DML = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(dml = case_when(
        is.na(DML) ~ NA, 
        DML >=150 ~ T, 
        TRUE ~ F)) 
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
    # to plot Tile view of demyelinating features (CIDP EDX criteria)
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
    df_cidp_table_long
  })
  
  output$cidp_tileView = renderPlot({
    if (is.null(feature_table_cidp())) return(NULL)
    # Tile view of demyelinating features (Hadden's criteria)
    p <- ggplot(feature_table_cidp(), aes(x=side.nerve, y=param,
                                     fill = feature)) +
      geom_tile(color = "black") + theme_minimal() +
      labs(title = "2020 PNS/EFNS EDX criteria for CIDP") +
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
  })
}

shinyApp(ui = ui, server = server)


