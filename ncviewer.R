if (!requireNamespace("shiny")){
  install.packages("shiny")
}
if (!requireNamespace("DT")){
  install.packages("DT")
}

library(shiny)
library(DT)

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
      radioButtons(inputId = "modeView", 
                   label = "Mode of View", 
                   choices = c("Nerve view", "Parameter view"))
    ), 
    mainPanel(
      DTOutput(outputId = "ncsTable"), 
      plotOutput(outputId = "radialPlot")
    )
  )
)


server <- function(input, output) {
  input_file = reactive({
    if (is.null(input$ncsfile)) {
      return(NULL)
    }
    read.csv(input$ncsfile$datapath)
  })
  
  output$ncsTable <- renderDT({
    input_file()})
}

shinyApp(ui = ui, server = server)


