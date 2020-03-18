ClusterDBUI <- function(id, label = "Clustering") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        
        sliderInput(ns("sl.eps"), "Epsilon",
                    min = 0.001, max = 0.5, value = 0.1),
        
        numericInput(ns("ni.n"), "min N", value = 10),
        
        
        tags$hr(),
        
        actionButton(ns("do.clust.db"), "Density Based")
        
      ),
      mainPanel(
        plotOutput(ns("plt.clust.db"))
      )
      
    )
  )
}

# --------------------------------------------------------------------------------------------------------------
ClusterDB <- function(input, output, session, data){
  
  
  # ============================================================
  
  dbased <- eventReactive(input$do.clust.db, {
    dt.int <- data()
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    dt.int <- as.data.table(normalizeFeatures(as.data.frame(dt.int), cols = c("x", "y"), method = "standardize"))
    
    fpc.clust <- dbscan(dt.int, eps = input$sl.eps, MinPts = input$ni.n)
    dt.db <- dt.int[, .(x, y, cl = fpc.clust$cluster)]
    
    plt <- ggplot(dt.db, aes(x = x, y = y, colour = paste0(cl))) + geom_point()
    
    return(plt)
  })
  
  observe({
    output$plt.clust.db <- renderPlot({dbased()})
  })
  
}