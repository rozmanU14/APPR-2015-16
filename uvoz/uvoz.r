
library(dplyr)
library(gsubfn)

#Vektor, ki predstavlja imena stolpcev:
nova.kolona<-c("kraj", "leto","zivorojeni moski","zivorojene zenske","umrli moski","umrle zenske","naravni prirast moski","naravni prirast zenske")
#Funkcija, ki uvozi podatke iz datoteke podatki.csv

uvozi<-function(){
    return(read.csv2(file="podatki/prirastek.csv",
                    col.names=nova.kolona,
                    header=FALSE,
                    fileEncoding = "UTF-8"))
                }
#Zapisemo podatke v razpredelnivo tabela
cat("Uvazam podatke o naravnem prirastku...\n")
tabela<-uvozi()

#Naredimo tabelo v kateri so vse občine
obcine <- tabela[seq(1, nrow(tabela), 6), 1]
#Naredimo tabelo kjer so podatki za vsako leto posebaj
podatki <- list()
for (i in 2:6) {
  podatki[[paste(tabela[i, "leto"])]] <- data.frame(tabela[seq(i, nrow(tabela), 6), c(-1, -2)], row.names = obcine)
}

#Naredimo tabelo, kjer so podatki za vsaki leto posebaj:
tabela2010<-podatki[["2010"]]
tabela2011<-podatki[["2011"]]
tabela2012<-podatki[["2012"]]
tabela2013<-podatki[["2013"]]
tabela2014<-podatki[["2014"]]





#poskrbimo, da so stevilske spremenljivke res stevilske
cat("Pretvorba stolpcev v stevilske spremenljivke...\n")
tabela$zivorojeni.moski <- as.integer(tabela$zivorojeni.moski)
tabela$zivorojene.zenske <- as.integer(tabela$zivorojene.zenske)
tabela$umrle.zenske <- as.integer(tabela$umrle.zenske)
tabela$umrli.moski <- as.integer(tabela$umrli.moski)
tabela$naravni.prirast.zenske <- as.integer(tabela$naravni.prirast.zenske)
tabela$naravni.prirast.moski <- as.integer(tabela$naravni.prirast.moski)

#Naredimo novo kolono "skupni naravni prirastek"
tabela["skupni prirast"]<-tabela$naravni.prirast.moski + tabela$naravni.prirast.zenske

#Okenca, za katere ni podatka in so oznacena z "-", zamenjamo z "NA":
tabela$zivorojeni.moski[tabela$zivorojeni.moski == "-"] <- NA
tabela$zivorojene.zenske[tabela$zivorojene.zenske == "-"] <- NA
tabela$umrli.moski[tabela$umrli.moski == "-"] <- NA
tabela$umrle.zenske[tabela$umrle.zenske == "-"] <- NA
tabela$naravni.prirast.moski[tabela$naravni.prirast.moski == "-"] <- NA
tabela$naravni.prirast.zenske[tabela$naravni.prirast.zenske == "-"] <- NA


#Uvozimo podatke iz datoteke evropa.html

html <- file("podatki/evropa.html") %>% readLines()
podatkiHTML <- grep("var dataValues", html, value = TRUE) %>%
  strapplyc('var dataValues="([^"]+)"') %>% .[[1]] %>%
  strsplit("|", fixed=TRUE) %>% unlist() %>%
  matrix(ncol=10, byrow=TRUE)

#Znake "-","(b):" zamenjamo z NA:
podatkiHTML[podatkiHTML == ":"] <- NA
podatkiHTML[podatkiHTML == "(b):"] <- NA

#izbrišemo del črk,ki se držijo številskih podatkov:
podatkiHTML <- gsub("[(b)]", " ", podatkiHTML)
podatkiHTML <- gsub("[(ep)]", " ", podatkiHTML)


#Filtriramo podatke za leta
podatkiLETA<- grep("var xValues", html, value=TRUE) %>%
  strapplyc('var xValues="([^"]+)"') %>% .[[1]] %>%
  strsplit("|", fixed=TRUE) %>% unlist() %>%
  matrix(nrow=1)

#vektor vseh let:
novipodatkiLETA<-podatkiLETA[seq(7,91,9)]

#Filtriramo podatke za države
podatkiDRZAVE <- grep("yValues", html, value = TRUE) %>%
  strapplyc('yValues="([^"]+)"') %>% .[[1]] %>%
  strsplit("|", fixed=TRUE) %>% unlist() %>%
  matrix(ncol=1)
#vekror vseh držav 
novipodatkiDRZAVE<-podatkiDRZAVE[seq(8,514,9)]


