GUI_2Danalysis <- function(data, temp = NULL){
  
  # Modules
  # Define the home directory where you've stored the modules
  source(paste0(p.shiny.fcts, "data_select.R"), local = T)
  source(paste0(p.shiny.mods, "module_Regression_linear.R"), local = T)
  source(paste0(p.shiny.mods, "module_Cluster_kmeans.R"), local = T)
  source("ggplot_themes.R")
  
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
      ),
      # ============================================================
      tabPanel("k means",
               ClusterKMUI("kmeans.pl", "km clustering")
      )
    )
  )
  
  # --------------------------------------------------------------------------------------------------------------
  # Define server logic
  
  server <- function(input, output, session) {
    
    dt <- callModule(File, "datafile", reactive(data))
    
    callModule(RegressionLin, "linreg.pl", reactive(dt()))
    
    callModule(ClusterKM, "kmeans.pl", reactive(dt()))
    
  }
  
  # Run the application 
  # shinyApp(ui = ui, server = server)
  runApp(list(ui = ui, server = server), launch.browser = T)
}

