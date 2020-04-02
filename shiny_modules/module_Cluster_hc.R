ClusterHCUI <- function(id, label = "Clust.hc") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sliderInput(ns("sl.num.clust"), "Number of clusters (where applicable)",
                  min = 2, max = 20, value = 2, step = 1),
      tags$hr(),
      selectInput(ns("sel.hclust.meth"), "h-clust method",
                  choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                  selected = "ward.D2"),
      tags$hr(),
      actionButton(ns("do.clust.hc"), "H-clustering")
    ),
    
    dashboardBody(
      plotlyOutput(ns("plt.clust.hc"), width = "800px", height = "600px")
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
    
    dt.db <- data.table(dt.int, cl = hc.cut)
    dt.db[, cl := as.factor(cl)]
    
    if(nrow(dt.db) > 5000){
      dt.db <- as.data.table(sample_n(dt.db, 5000))
    }
    
    plt <- dt.db %>%
      plot_ly(x = ~x, y = ~y, color = ~cl) %>%
      add_markers(alpha = 0.4) %>%
      layout(showlegend = F)
    
    return(plt)
  })
  
  observe({
    output$plt.clust.hc <- renderPlotly({hcluster()})
  })
  
}