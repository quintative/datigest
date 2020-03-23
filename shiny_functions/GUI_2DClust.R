GUI_2DClust <- function(data, temp = NULL){
  
  # Modules
  # Define the home directory where you've stored the modules
  source(paste0(p.shiny.mods, "module_Cluster_plot.R"), local = T)
  source(paste0(p.shiny.mods, "module_Cluster_kmeans.R"), local = T)
  source(paste0(p.shiny.mods, "module_Cluster_db.R"), local = T)
  source(paste0(p.shiny.mods, "module_Cluster_hc.R"), local = T)
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
      tabPanel("clust.pl",
               ClusterPlotUI("clust.pl", "Clustering")
      ),
      # ============================================================
      tabPanel("clust.km",
               ClusterKMUI("clust.km", "Clustering")
      ),
      # ============================================================
      tabPanel("clust.hc",
               ClusterHCUI("clust.hc", "Clustering")
      ),
      # ============================================================
      tabPanel("clust.db",
               ClusterDBUI("clust.db", "Clustering")
      )
    )
  )
  
  # --------------------------------------------------------------------------------------------------------------
  # Define server logic
  
  server <- function(input, output, session) {
    
    dt <- callModule(File, "datafile", reactive(data))

    callModule(ClusterPlot, "clust.pl", reactive(dt()))
    
    callModule(ClusterKM, "clust.km", reactive(dt()))
    
    callModule(ClusterHC, "clust.hc", reactive(dt()))
    
    callModule(ClusterDB, "clust.db", reactive(dt()))
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

