library(shiny)
  shinyUI(
    fluidPage(    
      titlePanel("Naravni prirast po občinah"),
      sidebarLayout(      
        sidebarPanel(
                 selectInput("kraj", "Občina:", 
                                             choices = levels(tabela$kraj),
                                            selected = "Ljubljana"),
          hr(),
          helpText("Izberi občino")
        ),
        mainPanel(
               plotOutput("pprirast")  
        )
        
      )
    )
  )
