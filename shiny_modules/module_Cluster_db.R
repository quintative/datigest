ClusterDBUI <- function(id, label = "Clustering") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sliderInput(ns("sl.eps"), "Epsilon",
                  min = 0.001, max = 0.5, value = 0.1),
      numericInput(ns("ni.n"), "min N", value = 10),
      tags$hr(),
      actionButton(ns("do.clust.db"), "Density Based") 
    ),
    dashboardBody(
      plotlyOutput(ns("plt.clust.db"), width = "800px", height = "600px")
    )
  )
}

# --------------------------------------------------------------------------------------------------------------
ClusterDB <- function(input, output, session, data){
  
  # ============================================================
  dbased <- eventReactive(input$do.clust.db, {
    dt.int <- data()
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    # 1 Standardization
    dt.int[, ":=" (x_std = (x - mean(x)) / sd(x),
                   y_std = (y - mean(y)) / sd(y))]
    
    fpc.clust <- dbscan(dt.int, eps = input$sl.eps, MinPts = input$ni.n)
    dt.db <- dt.int[, .(x, y, cl = fpc.clust$cluster)]
    
    if(nrow(dt.db) > 5000){
      dt.db <- as.data.table(sample_n(dt.db, 5000))
    }
    
    dt.db[, cl := as.factor(cl)]
    
    plt <- dt.db %>%
      plot_ly(x = ~x, y = ~y, color = ~cl) %>%
      add_markers(alpha = 0.4) %>% 
      layout(showlegend = F)
    
    return(plt)
  })
  
  observe({
    output$plt.clust.db <- renderPlotly({dbased()})
  })
  
}