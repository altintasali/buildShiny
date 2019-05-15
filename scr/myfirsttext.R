library(shiny)

# Define UI 
ui <- fluidPage( 
  
  # Page title
  titlePanel("My app"), 
  
  # Sidebar layout 
  sidebarLayout( 
    
    # Sidebar panel for inputs 
    sidebarPanel( 
      
      # Input
      selectInput(
        inputId = "textin",
        label = "My first Text",
        choices = c("Hello World!",
                    "foo",
                    "bar"),
        selected = "Hello World!"              
      )
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      textOutput(outputId = "textout")
    ) 
  )
)



# Define server logic 
server <- function(input, output) { 
  
  output$textout <- renderText({
    paste("My first text is:", input$textin)
  })
  
} 

# Run the app 
shinyApp(ui = ui, server = server)

