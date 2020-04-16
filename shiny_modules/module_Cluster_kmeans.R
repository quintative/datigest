# Creating the "elbow" plot
ElbowPlot <- function(data_int, k_max = 10){
  ellbow <- data.table(k = 1:k_max, within_ss = sapply(1:k_max, function(x){kmeans(data_int[, 1:2], x)$tot.withinss}))
  plt <- ellbow %>% 
    plot_ly(x = ~k, y = ~within_ss) %>% 
    add_markers(color = I("black")) %>% 
    add_lines(color = I("black")) %>%
    layout(showlegend = F)
  return(plt)
}

# Run K-means and plot it
KmeansPlot <- function(data, k){
  
  colnames(data) <- c("x", "y")
  model <- kmeans(data[, 1:2], k)
  data[, cluster := as.factor(model$cluster)]
  centers <- as.data.table(model$centers)
  colnames(centers) <- c("x", "y")
  centers[, cluster := as.factor(1:.N)]
  
  if(nrow(data) > 5000){
    dt.int <- as.data.table(sample_n(data, 5000))
  } else{
    dt.int <- data
  }
  
  plt <- dt.int %>% 
    plot_ly(x = ~x, y = ~y, color = ~cluster) %>%
    add_markers(alpha = 0.4) %>%
    add_annotations(data = centers,
                    x = ~x,
                    y = ~y,
                    text = paste0("x: ", centers[cluster == cluster, signif(x, 3)], "\ny: ", centers[cluster == cluster, signif(y, 3)]))
  return(plt)
}

ClusterKMUI <- function(id, label = "Clustering") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dashboardPage(
    
    dashboardHeader(),
    
    dashboardSidebar(
      sliderInput(ns("sl.num.max.elb"), "Max number for Elbow Plot",
                  min = 2, max = 20, value = 10, step = 1),
      actionButton(ns("do.elb.km"), "K-means <Elbow>"),
      sliderInput(ns("sl.num.clust"), "Number of clusters (where applicable)",
                  min = 2, max = 10, value = 2, step = 1),
      actionButton(ns("do.km"), "K-means"),
      tags$hr()),
    
    dashboardBody(
        plotlyOutput(ns("plt.clust.km.elb"), width = "800px", height = "600px") ,
        plotlyOutput(ns("plt.clust.km"), width = "800px", height = "600px")
        )
  )
}

# --------------------------------------------------------------------------------------------------------------
ClusterKM <- function(input, output, session, data){
  
  # ============================================================
  
  kmeans.ellb <- eventReactive(input$do.elb.km, {
    dt.int <- data()
    # 1 Standardization
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    dt.int[, ":=" (x_std = (x - mean(x)) / sd(x),
                   y_std = (y - mean(y)) / sd(y))]
    
    plt <- ElbowPlot(dt.int, input$sl.num.max.elb)
  
    return(plt)
    })
  
  kmeans <- eventReactive(input$do.km, {
    dt.int <- data()
    # 1 Standardization
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    dt.int[, ":=" (x_std = (x - mean(x)) / sd(x),
                   y_std = (y - mean(y)) / sd(y))]
    
    plt <- KmeansPlot(dt.int[, .(x_std, y_std)], input$sl.num.clust)
    
    return(plt)
    })
  
  observe({
    output$plt.clust.km.elb <- renderPlotly({kmeans.ellb()})
    })
  
  observe({
    output$plt.clust.km <- renderPlotly({kmeans()})
    })
}


