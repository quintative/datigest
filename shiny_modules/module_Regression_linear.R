# Run plot only
JustPlot <- function(data){
  colnames(data) <- c("x", "y")
  
  if(length(data) > 1000){
    sample_n(data, 1000) %>% ggplot(aes(x, y)) + geom_point(color = "white") +
      theme(legend.position = "none") + 
      ggtitle("Just the data - truncated to 1000") +
      theme_black()
  } else{
    data %>% ggplot(aes(x, y)) + geom_point(color = "white") +
      theme(legend.position = "none") +
      ggtitle("Just the data - all of it") +
      theme_black()
  }
}

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
  
  if(length(data) > 1000){
    sample_n(data, 1000) %>% ggplot(aes(x, y)) + geom_point(color = "white") +
      geom_abline(slope = M.coefs[2, 1], intercept = M.coefs[1, 1], color = "red", size = 2) +
      theme(legend.position = "none") +
      ggtitle(paste0("beta = ", signif(M.coefs[2, 1], 3), " se(beta) = ", signif(s.beta, 3))) +
      theme_black()
  } else{
    data %>% ggplot(aes(x, y)) + geom_point(color = "white") +
      geom_abline(slope = M.coefs[2, 1], intercept = M.coefs[1, 1], color = "red", size = 2) +
      theme(legend.position = "none") +
      ggtitle(paste0("beta = ", signif(M.coefs[2, 1], 3), " se(beta) = ", signif(s.beta, 3))) +
      theme_black()
  }
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
  
  if(length(data) > 1000){
    sample_n(data, 1000) %>% ggplot(aes(x, y)) + geom_point(color = "white") +
      geom_abline(slope = coef.hub[2, 1], intercept = coef.hub[1, 1], color = "red", size = 2) +
      theme(legend.position = "none") +
      ggtitle(paste0("beta = ", signif(coef.hub[2, 1], 3), " se(beta) = ", signif(s.beta, 3))) +
      theme_black()
  } else{
    data %>% ggplot(aes(x, y)) + geom_point(color = "white") +
      geom_abline(slope = coef.hub[2, 1], intercept = coef.hub[1, 1], color = "red", size = 2) +
      theme(legend.position = "none") +
      ggtitle(paste0("beta = ", signif(coef.hub[2, 1], 3), " se(beta) = ", signif(s.beta, 3))) +
      theme_black()
  }
}


RegressionLinUI <- function(id, label = "Regression") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      tags$hr(),
      actionButton(ns("do.justplot"), "Plot the data"),
      tags$hr(),
      actionButton(ns("do.linreg"), "OLS lin. regression"),
      tags$hr(),
      actionButton(ns("do.roblinreg"), "Huber OLS lin. regression"),
      tags$hr()
    ),
    dashboardBody(plotOutput(ns("plt.regr")))
  )
}

# --------------------------------------------------------------------------------------------------------------
RegressionLin <- function(input, output, session, data){
  
  # ============================================================
  
  # hublinreg <- eventReactive(input$do.justplot, {
  #   dt.int <- data()
  #   dt.int <- dt.int[(!is.na(x) & !is.na(y))]
  #   plt <- LinRegRobPlot(dt.int)
  #   return(plt)
  # })
  observeEvent(input$do.justplot, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- JustPlot(dt.int)
               output$plt.regr <- renderPlot(plt)})
  
  observeEvent(input$do.linreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- LinRegPlot(dt.int)
               output$plt.regr <- renderPlot(plt)})
  
  observeEvent(input$do.roblinreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- LinRegRobPlot(dt.int)
               output$plt.regr <- renderPlot(plt)})
}


