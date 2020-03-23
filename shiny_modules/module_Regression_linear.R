# Run regression and plot (max number of points 1000)
LinRegPlot <- function(data){
  
  colnames(data) <- c("x", "y")
  
  x <- data[, x]
  y <- data[, y]
  n.l <- length(y)
  # Building the matrix of independent variable
  X <- matrix(c(rep(1, n.l), x), ncol = 2)
  # Getting the intercept and coefficient
  M.coefs <- solve(t(X) %*% X) %*% t(X) %*% y
  # Computing the sums of squares and variance
  y.hat <- X %*% M.coefs
  variance <- sum((y - y.hat)**2) / (n.l - 2)
  s.beta <- sqrt(variance / sum((x - mean(x))**2))
  
  if(length(data) > 1000){
    sample_n(data, 1000) %>% ggplot(aes(x, y)) + geom_point() +
      geom_abline(slope = M.coefs[2, 1], intercept = M.coefs[1, 1], color = "red", size = 2) +
      theme(legend.position = "none") +
      ggtitle(paste0("beta = ", signif(M.coefs[2, 1], 3), " se(beta) = ", signif(s.beta, 3)))
  } else{
    data %>% ggplot(aes(x, y)) + geom_point() +
      geom_abline(slope = M.coefs[2, 1], intercept = M.coefs[1, 1], color = "red", size = 2) +
      theme(legend.position = "none") +
      ggtitle(paste0("beta = ", signif(M.coefs[2, 1], 3), " se(beta) = ", signif(s.beta, 3)))
  }
}

RegressionLinUI <- function(id, label = "Regression") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
       
        actionButton(ns("do.linreg"), "Linear regression"),
        
        tags$hr()
        
      ),
      mainPanel(
        plotOutput(ns("plt.lin.reg")) 
      )
    )
  )
}

# --------------------------------------------------------------------------------------------------------------
RegressionLin <- function(input, output, session, data){
  
  # ============================================================
  
  linreg <- eventReactive(input$do.linreg, {
    dt.int <- data()
    # 1 Standardization
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    
    plt <- LinRegPlot(dt.int)
    
    return(plt)
  })
  
  observe({
    output$plt.lin.reg <- renderPlot({linreg()})
  })
  
}


