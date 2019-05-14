library(shiny)

# Define UI for data upload app
ui <- fluidPage(
  
  # Page title
  titlePanel("File Uplader"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select a file
      fileInput(inputId = "file1", 
                label = "Choose File",
                multiple = FALSE),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Checkbox if file has header
      checkboxInput(inputId = "header", 
                    label = "Header", 
                    value = TRUE),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select separator
      radioButtons(inputId = "sep", 
                   label = "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select number of rows to display
      radioButtons(inputId = "disp", 
                   label = "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Checkbox for summary output
      checkboxInput(inputId = "summary", 
                    label = "Summary", 
                    value = FALSE)
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Summary
      verbatimTextOutput(outputId = "summary"),
      
      # Horizontal line
      tags$hr(),
      
      # Output: Data file
      tableOutput(outputId = "contents")
      
    )
    
  )
)


# Define server logic to read selected file
server <- function(input, output, session) {
  
  # Reactive: Read data
  df <- reactive({
    
    # Check "if" file is there. "if not" do not run the next lines
    req(input$file1)
    
    # Read data
    dat <- read.table(input$file1$datapath,
                      header = input$header,
                      sep = input$sep)
    return(dat)
    
  })
  
  # Output: Summary
  output$summary <- renderPrint({
    
    # Return summary
    if(input$summary) {
      return(summary(df()))
    }
    # # Instead we could have basically used 'req()' function
    # req(input$summary)
    # return(summary(df()))
    
  })
  
  # Output: Full table  
  output$contents <- renderTable({
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
    
  })

}

# Create Shiny app
shinyApp(ui, server)
