ClusterPlotUI <- function(id, label = "Clustering") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("do.plot"), "Plot!"),
        tags$hr()
      ),
      mainPanel(
        plotOutput(ns("plt.clust.plot")) 
      )
    )
  )
}

ClusterPlot <- function(input, output, session, data){
  
  # ============================================================
  
  doplot <- eventReactive(input$do.plot, {
    dt.int <- data()
  
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
  
    plt <- ggplot(dt.int, aes(x, y)) + geom_point()
    
    return(plt)
  })
  
  observe({
    output$plt.clust.plot <- renderPlot({doplot()})
  })
}
