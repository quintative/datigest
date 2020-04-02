# Run plot with regression (max number of points 1000)
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
  
  if(nrow(data) > 5000){
    dt.int <- as.data.table(sample_n(data, 5000))
  } else{
    dt.int <- data
  }
  
  abline.trace <- data.table(x = seq(from = dt.int[, min(x)], to = dt.int[, max(x)], length.out = 10))
  abline.trace[, y := M.coefs[2, 1] * x + M.coefs[1, 1]]
  
  plt <- dt.int %>%
    plot_ly(x = ~x, y = ~y) %>%
    add_markers(color = I("black"), alpha = 0.4)  %>% 
    add_lines(data = abline.trace, x = ~x, y = ~y, color = I("red")) %>%
    layout(showlegend = F)
  
  return(plt)
}

# Run plot with regression (max number of points 1000)
LinRegRobPlot <- function(data){
  
  colnames(data) <- c("x", "y")
  
  x <- data[, x]
  y <- data[, y]
  n.l <- length(y)
  # Building the matrix of independent variable
  X <- matrix(c(rep(1, n.l), x), ncol = 2)
  
  # Computing the weight vecotor for the regression
  w <- HuberLossWins(y, inner.quantile = 0.9, sig.outlier = 5)
  
  coef.hub <- solve(t(X) %*% (w * X)) %*% t(X) %*% (w * y)
  var.hub <- var(x) * coef.hub[[2]] ** 2 + var(y * w) - 2 * coef.hub[[2]] * cov(x, y * w)
  se.hub <- sqrt(var.hub * solve(t(X) %*% (w * X))) %>% diag() %>% .[2]
  
  # Computing the sums of squares and variance
  y.hat <- X %*% coef.hub
  variance <- sum(((w * y) - y.hat)**2) / (n.l - 2)
  s.beta <- sqrt(variance / sum((x - mean(x))**2))

  if(nrow(data) > 5000){
    dt.int <- as.data.table(sample_n(data, 5000))
  } else{
    dt.int <- data
  }
  
  abline.trace <- data.table(x = seq(from = dt.int[, min(x)], to = dt.int[, max(x)], length.out = 10))
  abline.trace[, y := coef.hub[2, 1] * x + coef.hub[1, 1]]
  
  plt <- dt.int %>%
    plot_ly(x = ~x, y = ~y) %>%
    add_markers(color = I("black"), alpha = 0.4)  %>% 
    add_lines(data = abline.trace, x = ~x, y = ~y, color = I("red")) %>%
    layout(showlegend = F)
  
  return(plt)
}


RegressionLinUI <- function(id, label = "Regression") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      tags$hr(),
      actionButton(ns("do.linreg"), "OLS lin. regression"),
      tags$hr(),
      actionButton(ns("do.roblinreg"), "Huber OLS lin. regression"),
      tags$hr()
    ),
    dashboardBody(plotlyOutput(ns("plt.regr"), width = "800px", height = "600px"))
  )
}

# --------------------------------------------------------------------------------------------------------------
RegressionLin <- function(input, output, session, data){
  
  # ============================================================
  observeEvent(input$do.linreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- LinRegPlot(dt.int)
               output$plt.regr <- renderPlotly(plt)})
  # ============================================================
  observeEvent(input$do.roblinreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- LinRegRobPlot(dt.int)
               output$plt.regr <- renderPlotly(plt)})
}


