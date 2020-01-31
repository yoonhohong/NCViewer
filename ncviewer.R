if (!requireNamespace("shiny")){
  install.packages("shiny")
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

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)

ui <- fluidPage(
  titlePanel("NCViewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "ncsfile", 
                label = "Choose NCS File (csv format)",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      hr(),
      DTOutput(outputId = "ptTable")
      ), 
    mainPanel(
      tabsetPanel(
        tabPanel("Table", br(), DTOutput("ncsTable")), 
        tabPanel("Parameter view", br(), plotlyOutput("paramView")), 
        tabPanel("Nerve view", br(), plotlyOutput("nerveView"))
      )
    )
  )
)

server <- function(input, output) {
  
  input_file = reactive({
    if (is.null(input$ncsfile)) return(NULL)
    read.csv(input$ncsfile$datapath)
  })
    
  output$ptTable <- renderDT({
    if (is.null(input_file())) return(NULL)
      input_file() %>% select(Hosp, ID, Name, Date)  
    }, rownames = F, , selection = "single")
  
  df_tab = reactive({
    s = input$ptTable_rows_selected
    if (length(s) == 0) return(NULL)
    df_selected = input_file()[s,] %>%
      gather(key = "side.nerve.param", 
             value = "value", R.MM.DML:L.TM.FL) %>%
    # split side.nerve.param into side.nerve and param
      separate(side.nerve.param, 
               into = c("side", "nerve", "param"), 
               sep = "\\.") %>%
      mutate(side.nerve = paste(side, nerve, sep=".")) %>%
    # type conversion: chr to factor
      mutate(side.nerve = factor(side.nerve, 
                           levels = 
                             c("L.MM", "L.UM", "L.PM", "L.TM", 
                               "R.TM", "R.PM", "R.UM", "R.MM"))) %>%
      mutate(param = factor(param, 
                      levels = c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                                 "DML", "Dur1", "Dur2", "Dur3", "Dur4",
                                 "NCV1", "NCV2", "NCV3", "FL")))
    # ncs data table 
    df_tab = df_selected %>% 
      select(side.nerve, param, value) %>% 
      spread(key = side.nerve, value = value) %>% 
      select_if(function(x){!all(is.na(x))})
    df_tab
  })
  
  output$ncsTable <- renderDT({
    if (is.null(df_tab())) return(NULL)
    df_tab()
    }, rownames = F, 
    options = list(pageLength = 13))
  
  df_view = reactive({
    if (is.null(df_tab())) return(NULL)
    df_view = df_tab() %>%
      gather(key = "side.nerve", value = "value", 
                     colnames(df_tab())[-1], factor_key = T) %>%
      filter(param %in% c("CMAP1", "CMAP2", "DML", 
                          "DUR1", "DUR2", 
                         "NCV1", "FL")) %>%
      mutate(param = factor(param))
    df_view
  })
  
  output$paramView = renderPlotly({
    if (is.null(df_view())) return(NULL)
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
              max(df_view()$value, na.rm = T)/50)+1)*50)
            ), 
          angularaxis = list(
            tickfont = list(size = 20)
            )
          ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    for (i in 1:length(levels(df_view()$side.nerve))) {
      temp = df_view() %>%
        filter(side.nerve == levels(df_view()$side.nerve)[i]) %>%
        select(param, value)
      p <- p %>% add_trace(
        r = c(temp[,2],temp[1,2]), # r: values in r-axes
        theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
        name = levels(df_view()$side.nerve)[i] # name: record name 
      )}  
    p
  })

  output$nerveView = renderPlotly({
    if (is.null(df_view())) return(NULL)
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
              max(df_view()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    for (i in 1:length(levels(df_view()$param))) {
      temp = df_view() %>%
        filter(param == levels(df_view()$param)[i]) %>%
        select(side.nerve, value)
      p <- p %>% add_trace(
        r = c(temp[,2],temp[1,2]), # r: values in r-axes
        theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
        name = levels(df_view()$param)[i] # name: record name 
      )}  
    p
  })
}

shinyApp(ui = ui, server = server)


