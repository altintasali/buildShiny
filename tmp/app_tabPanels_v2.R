library(shiny)
library(ggplot2)
library(plotly)
library(ggplotify)

#library(magrittr)
#library(dplyr)

# Define UI for data upload app
ui <- fluidPage(
  # Page title
  titlePanel("Read and plot"),
  
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
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
                 
                 # Input: Checkbox if file has header
                 checkboxInput(inputId = "summary", 
                               label = "Summary", 
                               value = FALSE)
                 
               ),
               
               
               # Sidebar layout with input and output definitions
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
             
    ),
    
    tabPanel("Generate Plot",
             pageWithSidebar(
               headerPanel('Plot options'),
               
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X variable', ""),
                 selectInput('ycol', 'Y variable', "", selected = ""),
                 selectInput('colorby', 'Colour', "", selected = ""),
                 radioButtons('facet', 'Facet',
                              c(No=FALSE,
                                Yes=TRUE),
                              FALSE),
                 selectInput('group', 'Facet Group', "", selected = ""),
                 textInput(inputId = "main", label = "Plot Title", value = "My plot"),
                 textInput(inputId = "xname", label = "X-axis title", value = ""),
                 textInput(inputId = "yname", label = "Y-axis title", value = ""),
                 checkboxGroupInput(inputId = "subset", label = "Subset", choices = "", selected = ""),
                 sliderInput(inputId = "dotsize", label = "Dot size",
                             value = 1, min = 0.5, max = 3, step = 0.5)
               ),
               
               mainPanel(
                 plotlyOutput('myplot')
               )
             )
    )
  )  
)


# Define server logic to read selected file
server <- function(input, output, session) {
  
  # Read data
  df <- reactive({
    
    req(input$file1)
    
    dat <- read.table(input$file1$datapath,
                      header = input$header,
                      sep = input$sep)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(dat), selected = names(dat)[1])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(dat), selected = names(dat)[2])
    updateSelectInput(session, inputId = 'colorby', label = 'Colour',
                      choices = names(dat), selected = names(dat)[5])
    updateSelectInput(session, inputId = 'group', label = 'Facet Group',
                      choices = names(dat), selected = names(dat)[5])
    
    return(dat)
    
  })
  
  # Output: Summary
  output$summary <- renderPrint({
    
    # Return summary
    if(input$summary) {
      return(summary(df()))
    }
    
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
  
  # # Observe: changes on the 'plot parameters'
  # observe({
  #   updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
  #                     choices = names(df()), selected = names(df())[1])
  #   updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
  #                     choices = names(df()), selected = names(df())[2])
  #   updateSelectInput(session, inputId = 'colorby', label = 'Colour',
  #                     choices = names(df()), selected = names(df())[5])
  #   updateSelectInput(session, inputId = 'group', label = 'Facet Group',
  #                     choices = names(df), selected = names(df())[5])
  # })

  # Observe: after input is updated, observe again
  observe({
    updateTextInput(session, inputId = "xname", label = "X-axis title",
                    value = input$xcol)
    updateTextInput(session, inputId = "yname", label = "Y-axis title",
                    value = input$ycol)
    
    if(class(df()[[input$group]]) == "numeric"){
      subgroup <- character(0)
    }else{
      subgroup <- sort(unique(df()[[input$group]]))
    }
    updateCheckboxGroupInput(session, inputId = "subset", label = paste("Subset by:", input$group),
                             choices = subgroup, selected = subgroup)
    
  })
  
  # Reactive: data for plotting (for subsetting)
  dfsub <- reactive({
    # subset df
    dfsub <- subset(df(), get(input$group) %in% input$subset)
    return(dfsub)
  })
  
  # Output: Plot
  output$myplot <- renderPlotly({
    
    pbase <- ggplot(data = dfsub(),
                    aes_string(x = input$xcol,
                               y = input$ycol,
                               colour = input$colorby)) + 
      geom_point(size=input$dotsize) +
      ggtitle(input$main) +
      xlab(input$xname) + 
      ylab(input$yname)
    
    if(input$facet){
      p <- pbase + facet_grid(input$group)
      p <- ggplotly(p)
    }else{
      p <- ggplotly(pbase)
    }
    
    return(p)
    
  })
}

# Create Shiny app
shinyApp(ui, server)
