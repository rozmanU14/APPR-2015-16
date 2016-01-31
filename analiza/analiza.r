library(MASS)
library(shiny)


kolonaLj<-c("leto","prirast")
uuvozi<-function(){
  return(read.csv2(file="podatki/ljubljana.csv",
                   col.names=kolonaLj,
                   header=FALSE,
                   na.strings = "-",
                   fileEncoding = "UTF-8",
                   as.is = FALSE))
}
tabelaLjubljana<-uuvozi()
tabelaLjubljana$prirast <- as.numeric(tabelaLjubljana$prirast)


kolonaSlo<-c("leto","rodnost na 1000","umrlivost na 1000")
uuvozi<-function(){
  return(read.csv2(file="podatki/rodnost;umrlivost(slo).csv",
                   col.names=kolonaSlo,
                   header=FALSE,
                   na.strings = "-",
                   fileEncoding = "UTF-8",
                   as.is = FALSE))
}
tabelaSlo<-uuvozi()
tabelaSlo$rodnost.na.1000 <- as.numeric(tabelaSlo$rodnost.na.1000)
tabelaSlo$umrlivost.na.1000 <- as.numeric(tabelaSlo$umrlivost.na.1000)


