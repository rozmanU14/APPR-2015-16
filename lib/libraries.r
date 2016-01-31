library(knitr)

# Uvozimo funkcije za delo z datotekami XML.
source("lib/xml.r", encoding = "UTF-8")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")


library(dplyr)
library(gsubfn)
library(ggplot2)
library(MASS)
library (shiny)
