library(shiny)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Naravni prirast po občinah"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("občina", "Občina:", 
                    choices=krajj),
        hr(),
        helpText("Izberi občino")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("naravni prirast")  
      )
      
    )
  )
)