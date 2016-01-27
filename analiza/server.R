library(shiny)
pprirast<-tabela$naravni.prirast.na.1000.prebivalcev
lleto<- tabela$leto
kkraj<- tabela$kraj

shinyServer(function(input, output) {
  output$pprirast <- renderPlot({
   barplot(pprirast[,input$krajj], 
            main=input$krajj,
            ylab="Naravni prirast",
            xlab="Leto")
  })
})
