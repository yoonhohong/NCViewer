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
        tabPanel("Nerve view", br(), br(), plotlyOutput("nerveView"))
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
               col_types = c("text", "date", rep("text", 3), 
                             rep("numeric", 113)))
  })
    
  output$ptTable <- renderDT({
    if (is.null(input_file())) return(NULL)
    input_file() %>% 
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
      select(Hosp, ID, Name, Date)
    }, rownames = F, selection = "single")
  
  df_selected = reactive({
    if (length(input$ptTable_rows_selected) == 0) return(NULL)
    df_selected = input_file()[input$ptTable_rows_selected,]
    df_selected <- df_selected %>%
      mutate_if(is.numeric, as.integer)
    })
  
  df_motor_all = reactive({
    if (is.null(df_selected())) return(NULL)
    tab_motor = df_selected() %>%
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
  })
  
  output$tileView = renderPlot({
    if (is.null(df_motor_all())) return(NULL)
    
    temp = df_motor_all() %>%
      group_by(side.nerve) %>%
      filter(!all(is.na(value)))
    
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
  
  df_motor_radial = reactive({
    if (is.null(df_motor_all())) return(NULL)
    motor_radial <- df_motor_all() %>%
      group_by(side.nerve) %>%
      mutate(all_na = all(is.na(value))) %>%
      filter(all_na == F) 
    motor_radial <- data.frame(motor_radial) %>%
      filter(param %in% c("CMAP1", "CMAP2",  
                          "DML", "Dur1", "Dur2",
                          "NCV1", "FL")) %>%
      mutate(param = factor(param)) %>%
      mutate(side.nerve = factor(side.nerve))
    motor_radial
  })
  
  # paramView; angular axis = parameter, category = nerve
  output$paramView = renderPlotly({
    if (is.null(df_motor_radial())) return(NULL)
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
            range = c(0, (round(
              max(df_motor_radial()$value, na.rm = T)/50)+1)*50)
            ), 
          angularaxis = list(
            tickfont = list(size = 20)
            )
          ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    for (i in 1:length(levels(df_motor_radial()$side.nerve))) {
      temp = df_motor_radial() %>%
        filter(side.nerve == levels(df_motor_radial()$side.nerve)[i]) %>%
        select(param, value)
      temp = temp[order(temp$param),]
      p <- p %>% add_trace(
        r = c(temp[,2],temp[1,2]), # r: values in r-axes
        theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
        name = levels(df_motor_radial()$side.nerve)[i] # name: record name 
      )}  
    p
  })

# nerveView; angular axis = nerve, category = parameter 
  output$nerveView = renderPlotly({
    if (is.null(df_motor_radial())) return(NULL)
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
            range = c(0, (round(
              max(df_motor_radial()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    for (i in 1:length(levels(df_motor_radial()$param))) {
      temp = df_motor_radial() %>%
        filter(param == levels(df_motor_radial()$param)[i]) %>%
        select(side.nerve, value)
      temp = temp[order(temp$side.nerve),]
      p <- p %>% add_trace(
        r = c(temp[,2],temp[1,2]), # r: values in r-axes
        theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
        name = levels(df_motor_radial()$param)[i] # name: record name 
      )}  
    p
  })
}

shinyApp(ui = ui, server = server)


