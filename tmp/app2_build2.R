library(shiny)
library(plotly)
library(ggplotify)

# Define UI for data upload app
ui <- fluidPage(
  
  # Page title
  titlePanel("Data Plotter"),
  
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
      
      # Input: Plotting parameters
      # "Empty inputs" - they will be updated after the data is uploaded
      
      # Input: X axis to plot
      selectInput(inputId = "xcol", label = "X variable", choices = ""),
      
      # Input: Y axis to plot
      selectInput(inputId = "ycol", label = "Y variable", choices = "", selected = ""),
      
      # Input: Column to color dots
      selectInput(inputId = "colorby", label = "Colour", choices = "", selected = "")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Plot data
      plotlyOutput('myplot')
      
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
    
    # Update: changes on the 'plot parameters'
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(dat), selected = names(dat)[1])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(dat), selected = names(dat)[2])
    updateSelectInput(session, inputId = 'colorby', label = 'Colour',
                      choices = names(dat), selected = names(dat)[5])
    
    return(dat)
    
  })
  
  # Output: Plot
  output$myplot <- renderPlotly({
    
    pbase <- ggplot(data = df(),
                    aes_string(x = input$xcol,
                               y = input$ycol,
                               colour = input$colorby)) + 
      geom_point()
    
    p <- ggplotly(pbase)
    
    return(p)
    
  })
  
}

# Create Shiny app
shinyApp(ui, server)