GUI_Regress <- function(data, temp = NULL){
  
  # Modules
  # Define the home directory where you've stored the modules
  source(paste0(p.shiny.mods, "module_Regression_linear.R"), local = T)
  source(paste0(p.shiny.fcts, "data_select.R"), local = T)
  
  # --------------------------------------------------------------------------------------------------------------
  
  # Define UI
  
  ui <- fluidPage(
    tabsetPanel(
      # ============================================================
      tabPanel("data",
               FileUI("datafile", "User Data")
      ),
      # ============================================================
      tabPanel("lin reg",
               RegressionLinUI("linreg.pl", "simple regression")
      )
    )
  )
  
  # --------------------------------------------------------------------------------------------------------------
  # Define server logic
  
  server <- function(input, output, session) {
    
    dt <- callModule(File, "datafile", reactive(data))
    
    callModule(RegressionLin, "linreg.pl", reactive(dt()))
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

