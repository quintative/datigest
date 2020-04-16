JustPlot <- function(data){
  if(nrow(data) > 5000){
    dt.int <- as.data.table(sample_n(data, 5000))
  } else{
    dt.int <- data
  }
  
  plt <- dt.int %>% 
    plot_ly(x = ~x, y = ~y) %>%
    add_markers(color = I("black"), alpha = 0.4)
  return(plt)
}

FileUI <- function(id, label = "fileselect") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      selectInput(inputId = ns("col.x"),
                  label   = "Chose x",
                  choices = NULL),
      
      selectInput(inputId = ns("col.y"),
                  label   = "Chose y",
                  choices = NULL),
      
      tags$hr(),
      
      actionButton(ns("do.justplot"), "Plot the data"),
      
      tags$hr(),
      
      checkboxInput(ns("do.rank.x"), "Rank x"),
      checkboxInput(ns("do.rank.y"), "Rank y")
    ),
    dashboardBody(dataTableOutput(ns("table")),
                  verbatimTextOutput(ns("txt.summ")),
                  plotlyOutput(ns("plt.data"), width = "800px", height = "600px"))
  )
}

File <- function(input, output, session, data){

  # ============================================================
  # Prepare data.table function for further use
  dt.main <- reactive({
    dt <- data()
    return(dt)
    })
  # ============================================================
  # Show head of data
  output$table <- renderDataTable({
    if(is.null(dt.main())) return(NULL)
    return(dt.main())
    }, options = list(pageLength = 10))
  # ============================================================
  # Get summary of the data
  output$txt.summ <- renderPrint({
    if(is.null(dt.main())) return(NULL)
    return(summary(dt.main()))
    })
  # ============================================================
  # Selection of the data.table columns
  observe({
    x <- colnames(dt.main())
    updateSelectInput(session, 
                      inputId  = "col.x", 
                      choices  = x,
                      selected = x[1])
    updateSelectInput(session, 
                      inputId  = "col.y", 
                      choices  = x,
                      selected = x[2])
    })
  # ============================================================
  # Create the selected sub-data.table
  dt.sel <- reactive({
    dt.int <- dt.main()[, .(x = eval(parse(text = input$col.x)),
                            y = eval(parse(text = input$col.y)))]
    
    if(input$do.rank.x)
      {dt.int <- TransfRank(dt.int, "x", mode = c(-1, 1))}
    
    if(input$do.rank.y)
      {dt.int <- TransfRank(dt.int, "y", mode = c(-1, 1))}
    
    return(dt.int)
    })
  # ============================================================
  # Plot the data
  observeEvent(input$do.justplot, {
    dt.int <- dt.sel()
    dt.int <- dt.int[(!is.na(x) & !is.na(y))]
    plt <- JustPlot(dt.int)
    output$plt.data <- renderPlotly(plt)
    })
  
  return(dt.sel)
}

