# Define UI
ui <- fluidPage(
  titlePanel("My app"),
  
  # Tab panel set
  tabsetPanel(
    
    # Tab Panel for Tab 1 
    tabPanel("Tab 1",
             
             # Title for panel
             titlePanel("Options for Tab 1"),
             
             # Sidebar layout with input and output definitions
             sidebarLayout(
               
               # Sidebar panel for inputs
               sidebarPanel(),
               
               # Main panel for displaying outputs
               mainPanel()
               
             )
    ),
    
    # Tab Panel for Tab 2 
    tabPanel("Tab 2",
             
             titlePanel("Options for Tab 2"),
             
             # Sidebar layout with input and output definitions
             sidebarLayout(
               
               # Sidebar panel for inputs
               sidebarPanel(),
               
               # Main panel for displaying outputs
               mainPanel()
               
             )
             
    )
    
  )
  
)

# Define server logic
server <- function(input, output, session) {
 
}

# Run the app
shinyApp(ui = ui, server = server)