# Creating the "elbow" plot
ElbowPlot <- function(data_int, k_max = 10){
  ellbow <- data.table(k = 1:k_max, within_ss = sapply(1:k_max, function(x){kmeans(data_int[, 1:2], x)$tot.withinss}))
  ellbow %>% ggplot(aes(k, within_ss)) + geom_point() + geom_line() + scale_x_continuous(breaks = 1:k_max)
}

# Run K-means and plot it
KmeansPlot <- function(data, k){
  colnames(data) <- c("x", "y")
  model <- kmeans(data[, 1:2], k)
  data[, cluster := as.factor(model$cluster)]
  centers <- as.data.table(model$centers)
  colnames(centers) <- c("x", "y")
  centers[, cluster := as.factor(1:.N)]
  data %>% ggplot(aes(x, y, colour = cluster)) + geom_point() +
    geom_label(data = centers, aes(x, y, label = paste0("x: ", centers[cluster == cluster, signif(x, 3)], "\ny: ", centers[cluster == cluster, signif(y, 3)])), alpha = 0.9) +
    theme(legend.position = "none")
}

ClusterKMUI <- function(id, label = "Clustering") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        sliderInput(ns("sl.num.max.elb"), "Max number for Elbow Plot",
                    min = 2, max = 20, value = 10, step = 1),
        
        actionButton(ns("do.elb.km"), "K-means <Elbow>"),
        
        sliderInput(ns("sl.num.clust"), "Number of clusters (where applicable)",
                    min = 2, max = 10, value = 2, step = 1),
        
        actionButton(ns("do.km"), "K-means"),
        
        tags$hr()
        
      ),
      mainPanel(
        plotOutput(ns("plt.clust.km.elb")) ,
        plotOutput(ns("plt.clust.km"))
      )
      
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
    output$plt.clust.km.elb <- renderPlot({kmeans.ellb()})
  })
  
  observe({
    output$plt.clust.km <- renderPlot({kmeans()})
  })
}


