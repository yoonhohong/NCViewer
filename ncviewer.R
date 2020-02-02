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
library(ggplot2)


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
        tabPanel("Tile view", br(), plotOutput("tileView")), 
        tabPanel("Parameter view", br(), br(), plotlyOutput("paramView")), 
        tabPanel("Nerve view", br(), br(), plotlyOutput("nerveView"))
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
  
  df_selected = reactive({
    if (length(input$ptTable_rows_selected) == 0) return(NULL)
    df_selected = input_file()[input$ptTable_rows_selected,]
    df_selected
    })
  
  
  df_tab_all = reactive({
    if (is.null(df_selected())) return(NULL)
    tab = df_selected() %>%
      gather(key = "side.nerve.param", 
             value = "value", R.MM.DML:L.TM.FL) %>%
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

    tab_A = tab %>%
      filter(param %in% c("DML", "Dur1", "Dur2", "Dur3", "Dur4", "FL")) %>%
      mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL"))
    
    tab_B = tab %>%
      filter(param %in% c("CMAP1", "CMAP2", "CMAP3", "CMAP4", 
                          "NCV1", "NCV2", "NCV3")) %>%
      mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL"))
    
    tab_all = rbind(tab_A, tab_B)
    tab_all
  })
  
  output$tileView = renderPlot({
    if (is.null(df_tab_all())) return(NULL)
    p <- ggplot(df_tab_all(), aes(x=side.nerve, y=param, 
                                   fill = factor(cutoff))) + 
      geom_tile(color = "black") + 
      scale_fill_manual(values = c("red", "green", "white"), 
                        name = "") + 
      geom_text(aes(label = value), size = 8) + theme_minimal() + 
      theme(axis.text.x = element_text(size = 16, face = "bold"), 
            axis.text.y = element_text(size = 16, face = "bold"), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(), 
            legend.text = element_text(size = 16, face = "bold"))
    p
  })
  
  df_tab_radial = reactive({
    if (is.null(df_tab_all())) return(NULL)
    tab_radial <- df_tab_all() %>%
      group_by(side.nerve) %>%
      mutate(all_na = all(is.na(value))) %>%
      filter(all_na == F) 
    tab_radial <- data.frame(tab_radial) %>%
      filter(param %in% c("CMAP1", "CMAP2",  
                          "DML", "Dur1", "Dur2",
                          "NCV1", "FL")) %>%
      mutate(param = factor(param)) %>%
      mutate(side.nerve = factor(side.nerve))
    tab_radial
  })
  
  # paramView; angular axis = parameter, category = nerve
  output$paramView = renderPlotly({
    if (is.null(df_tab_radial())) return(NULL)
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
              max(df_tab_radial()$value, na.rm = T)/50)+1)*50)
            ), 
          angularaxis = list(
            tickfont = list(size = 20)
            )
          ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    for (i in 1:length(levels(df_tab_radial()$side.nerve))) {
      temp = df_tab_radial() %>%
        filter(side.nerve == levels(df_tab_radial()$side.nerve)[i]) %>%
        select(param, value)
      temp = temp[order(temp$param),]
      p <- p %>% add_trace(
        r = c(temp[,2],temp[1,2]), # r: values in r-axes
        theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
        name = levels(df_tab_radial()$side.nerve)[i] # name: record name 
      )}  
    p
  })

# nerveView; angular axis = nerve, category = parameter 
  output$nerveView = renderPlotly({
    if (is.null(df_tab_radial())) return(NULL)
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
              max(df_tab_radial()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        legend = list(font = list(size = 20), x = 100, y = 0.5)
      )
    for (i in 1:length(levels(df_tab_radial()$param))) {
      temp = df_tab_radial() %>%
        filter(param == levels(df_tab_radial()$param)[i]) %>%
        select(side.nerve, value)
      temp = temp[order(temp$side.nerve),]
      p <- p %>% add_trace(
        r = c(temp[,2],temp[1,2]), # r: values in r-axes
        theta = c(as.character(temp[,1]), as.character(temp[1,1])),# theta: levels of r-axes
        name = levels(df_tab_radial()$param)[i] # name: record name 
      )}  
    p
  })
}

shinyApp(ui = ui, server = server)


