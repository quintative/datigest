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

LegReg <- function(data, m = 5){
  if(nrow(data) > 5000){dt.int <- as.data.table(sample_n(data, 5000))} 
  else{dt.int <- data}
  
  result <- FitLegendre(dt.int, n = m)

  # Find the sse and mse
  sse <- sum((dt.int[, y] - result$model$fitted.values)**2)
  mse <- sse / (nrow(dt.int) - 2)
  
  # Find the critical t-value
  t.val <- qt(0.975, nrow(dt.int) - 2)
  
  dt.fitline <- result$fitline
  
  dt.fitline[, twosig_h := y + t.val * sqrt(mse) * sqrt(1 / nrow(dt.int) + (x - dt.int[, mean(x)])**2 / sum((x - dt.int[, mean(x)])**2))]
  dt.fitline[, twosig_l := y - t.val * sqrt(mse) * sqrt(1 / nrow(dt.int) + (x - dt.int[, mean(x)])**2 / sum((x - dt.int[, mean(x)])**2))]
  
  plt <- dt.int %>%
    plot_ly() %>% 
    add_lines(data = dt.fitline, x = ~x, y = ~y, color = I("red")) %>%
    add_trace(data = dt.fitline, x = ~x, y = ~twosig_h, mode = "lines", type = "scatter", color = I("red")) %>%
    add_trace(data = dt.fitline, x = ~x, y = ~twosig_l, mode = "lines", type = "scatter", color = I("red"), fill = "tonexty", alpha = 0.3) %>%
    add_markers(data = dt.int, x = ~x, y = ~y, color = I("black"), alpha = 0.4) %>%
    layout(showlegend = F)
  
  return(list(plot = plt, model = result$model))
}

RegressionLinUI <- function(id, label = "Regression") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      tags$hr(),
      sliderInput(ns("sl.leg.order"), "Order of Leg. polynom",
                  min = 1, max = 5, value = 1, step = 1),
      actionButton(ns("do.legreg"), "OLS linar/Legendre link function"),
      tags$hr(),
      actionButton(ns("do.roblinreg"), "Huber OLS lin. regression"),
      tags$hr()
    ),
    dashboardBody(plotlyOutput(ns("plt.regr"), width = "800px", height = "600px"),
                  verbatimTextOutput(ns("summ.regr")))
  )
}

# --------------------------------------------------------------------------------------------------------------
RegressionLin <- function(input, output, session, data){
  
  # ============================================================
  observeEvent(input$do.roblinreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               plt <- LinRegRobPlot(dt.int)
               output$plt.regr <- renderPlotly(plt)
               output$summ.regr <- renderPrint(print(NULL))})
  # ============================================================
  observeEvent(input$do.legreg, 
               {dt.int <- data()
               dt.int <- dt.int[(!is.na(x) & !is.na(y))]
               result <- LegReg(dt.int, input$sl.leg.order)
               output$plt.regr <- renderPlotly(result$plot)
               output$summ.regr <- renderPrint(summary(result$model))})
}


