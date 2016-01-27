library(shiny)


if ("server.R" %in% dir()) {
     setwd("..")
    }
source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")

shinyServer(function(input, output) {
  output$pprirast <- renderPlot({
  
    ggplot(tabela %>% filter(kraj == input$kraj),
                        aes(x = leto, y = naravni.prirast.na.1000.prebivalcev)) +
    geom_bar(stat = "identity", fill=c("indianred1","indianred2","indianred3","indianred4","indianred"),color="indianred4") + xlab("Leto") + ylab("Naravni prirast na 1000 prebivalcev")
  })
})