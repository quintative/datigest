ClusterHCUI <- function(id, label = "Clust.hc") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
      
        sliderInput(ns("sl.num.clust"), "Number of clusters (where applicable)",
                    min = 2, max = 20, value = 2, step = 1),
        
        
        tags$hr(),
        
        
        selectInput(ns("sel.hclust.meth"), "h-clust method",
                    choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                    selected = "ward.D2"),
        
        
        tags$hr(),
        
        
        actionButton(ns("do.clust.hc"), "H-clustering")
        
      ),
      mainPanel(
        plotOutput(ns("plt.clust.hc"))
      )
      
    )
  )
}

# --------------------------------------------------------------------------------------------------------------
ClusterHC <- function(input, output, session, data){
  
  # ============================================================
  
  
  hcluster <- eventReactive(input$do.clust.hc, {
    dt.int <- data()
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    
    # 1 Standardization
    dt.int[, ":=" (x_std = (x - mean(x)) / sd(x),
                   y_std = (y - mean(y)) / sd(y))]
    
    # Creating eucledian distance matrix
    dt.d <- dist(dt.int)
    
    # Doing the clustering
    hc <- hclust(dt.d, method = input$sel.hclust.meth)
    
    # Choosing the number of clusters
    hc.cut <- cutree(hc, input$sl.num.clust)
    
    plt <- ggplot(data.table(dt.int, cl = hc.cut), aes(x = x, y = y, colour = paste0(cl))) +
      geom_point()
    
    return(plt)
  })
  
  observe({
    output$plt.clust.hc <- renderPlot({hcluster()})
  })
  
  
}