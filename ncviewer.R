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
        tabPanel("FU view", br(), br(),
                 plotlyOutput("fuview"))
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
      mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
      mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
    
    tab_motor_B = tab_motor %>%
      filter(param == "FL") %>%
      mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
      mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))

    tab_motor_C = tab_motor %>%
      filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                          "NCV1", "NCV2", "NCV3")) %>%
      mutate(cutoff = ifelse(is.na(value), NA, value)) %>%
      mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL"))
    
    tab_motor_all = rbind(tab_motor_A, tab_motor_B, tab_motor_C)
    tab_motor_all
  })
  
  output$tileView = renderPlot({
    if (is.null(df_motor_all())) return(NULL) 
    temp = df_motor_all() 
    # %>%
    #   group_by(side.nerve) %>%
    #   filter(!all(is.na(value)))
    # 
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
    # %>%
    #   group_by(side.nerve) %>%
    #   mutate(all_na = all(is.na(value))) %>%
    #   filter(all_na == F) 
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
                line = list(dash = "dot")) %>%
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
                line = list(dash = 'dot'))  %>%
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
    
  output$fuview = renderPlotly({
    if (is.null(df_selected())) return(NULL)
    p = fu_selected() %>%
      group_by(side.nerve) %>%
      do(p = spg_plot_param(.)) %>%
      subplot(nrows = round(length(levels(.$side.nerve))/4), 
              shareY = T, shareX = T, 
              titleX = F, titleY = T) 
    p1 = style(p, traces = 1:7, showlegend = T)
    layout(p1, legend = list(font = list(size = 15)))
  })
}

shinyApp(ui = ui, server = server)


