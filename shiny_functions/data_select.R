FileUI <- function(id, label = "fileselect") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    titlePanel("Load data"),
    sidebarLayout(
      sidebarPanel(

        selectInput(inputId = ns("col.x"),
                    label   = "Chose x",
                    choices = NULL),
        
        selectInput(inputId = ns("col.y"),
                    label   = "Chose y",
                    choices = NULL)
      ),
      mainPanel(
        tableOutput(ns("table"))
      )
    )
  )
}

File <- function(input, output, session, data){

  # ============================================================
  
  dt.main <- reactive({
    dt <- data()
    return(dt)
  })
  # ============================================================
  output$table <- renderTable({
    if(is.null(dt.main())) return(NULL)
    return(head(dt.main(), 10))
  })
  # ============================================================
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
  dt.sel <- reactive({
    dt.main()[, .(x = eval(parse(text = input$col.x)),
                  y = eval(parse(text = input$col.y)))]
  })
  # ============================================================
  # ============================================================
  
  return(dt.sel)
}

