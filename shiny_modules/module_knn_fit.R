# Run plot with regression (max number of points 1000)
knnReg <- function(data, k = 5){
  
  dt.int <- copy(data)
  
  # colnames(dt.int) <- c("x", "y")
  
  # Create the knn regression model
  model <- knnreg(y ~ x, dt.int, k = k)
  
  # Create fitting line
  dt.int[, y_fit := predict(model, dt.int)]
  
    if(nrow(dt.int) > 5000){
    dt.int <- as.data.table(sample_n(dt.int, 5000))
  } else{
    dt.int <- dt.int
  }
  
  plt <- dt.int %>%
    plot_ly() %>%
    add_markers(x = ~x, y = ~y, color = I("black"), alpha = 0.4)  %>% 
    add_lines(x = ~x, y = ~y_fit, color = I("red")) %>%
    layout(showlegend = F)
  
  return(plt)
}

# --------------------------------------------------------------------------------------------------------------
RegressionknnUI <- function(id, label = "knnRegression") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      numericInput(ns("num.k"), "k (average over #)",
                  min = 2, value = 10),
      actionButton(ns("do.knnreg"), "knn regression"),
      tags$hr()
    ),
    dashboardBody(plotlyOutput(ns("plt.knnregr"), width = "800px", height = "600px"))
  )
}

# --------------------------------------------------------------------------------------------------------------
Regressionknn <- function(input, output, session, data){
  
  # ============================================================
  observeEvent(input$do.knnreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- knnReg(dt.int, k = input$num.k)
               output$plt.knnregr <- renderPlotly(plt)
               })
}