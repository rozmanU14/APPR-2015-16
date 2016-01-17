#Vektor, ki predstavlja imena stolpcev:
nova.kolona<-c("kraj", "leto","zivorojeni moski","zivorojene zenske","umrli moski","umrle zenske","naravni prirast moski","naravni prirast zenske","zivorojeni.na.1000","umrli.na.1000","naravni.prirast.na.1000.prebivalcev")
#Funkcija, ki uvozi podatke iz datoteke podatki.csv

uvozi<-function(){
  return(read.csv2(file="podatki/prirastek.csv",
                   col.names=nova.kolona,
                   header=FALSE,
                   na.strings = "-",
                   fileEncoding = "UTF-8",
                   as.is = FALSE))
}
#Zapisemo podatke v razpredelnivo tabela
tabela<-uvozi()

#funkcija za zapis vrstic
uredi <- function(tabela, x, y, z, max = nrow(tabela)) {
  s <- seq(x, max, z+1)
  tabela[t(matrix(x:max, ncol=length(s))), y] <- tabela[s, y]
  tabela <-tabela[-s,]
  return(tabela)
}

tabela <- uredi(tabela, 1, 1, 5)

#Naredimo tabelo v kateri so vse občine
obcine <- tabela[seq(1, nrow(tabela), 5), 1]
#Naredimo tabelo kjer so podatki za vsako leto posebaj
podatki <- list()
for (i in 1:5) {
  podatki[[paste(tabela[i, "leto"])]] <- data.frame(tabela[seq(i, nrow(tabela), 5), c(-2)], row.names = NULL, stringsAsFactors = FALSE)
}

#nariše za kraj
tabela[tabela[["kraj"]] == "Beltinci",]

#poskrbimo, da so stevilske spremenljivke res stevilske
tabela$zivorojeni.moski <- as.numeric(tabela$zivorojeni.moski)
tabela$zivorojene.zenske <- as.numeric(tabela$zivorojene.zenske)
tabela$umrle.zenske <- as.numeric(tabela$umrle.zenske)
tabela$umrli.moski <- as.numeric(tabela$umrli.moski)
tabela$naravni.prirast.zenske <- as.numeric(tabela$naravni.prirast.zenske)
tabela$naravni.prirast.moski <- as.numeric(tabela$naravni.prirast.moski)
tabela$naravni.prirast.na.1000.prebivalcev <- as.numeric(tabela$naravni.prirast.na.1000.prebivalcev)
tabela$zivorojeni.na.1000 <- as.numeric(tabela$zivorojeni.na.1000)
tabela$umrli.na.1000 <- as.numeric(tabela$umrli.na.1000)


#seštevanje dveh stolpcev in ustvarjanje novega
tabela["skupni.prirast"]<-tabela$naravni.prirast.moski+ tabela$naravni.prirast.zenske
tabela$skupni.prirast <- as.numeric(tabela$skupni.prirast)

#naredimo nov stolpec v katerem skupni naravni prirastek kategoriziramo
attach(tabela)
kategorije<-c('pozitiven','negativen','ni prirastka')
velikost<-"skupni.prirast"
velikost[skupni.prirast==0]<-'ni prirastka'
velikost[skupni.prirast < 0]<-'negativen'
velikost[skupni.prirast>0]<-'pozitiven'
velikost<-factor(velikost,levels=kategorije,ordered=TRUE)
detach(tabela)
dodatenstolpec<-data.frame(velikost)
tabela<-data.frame(tabela,velikost)

#Naredimo tabelo, kjer so podatki za vsaki leto posebaj:

tabela2010<- tabela[tabela[["leto"]] == "2010",]
tabela2011<- tabela[tabela[["leto"]] == "2011",]
tabela2012<- tabela[tabela[["leto"]] == "2012",]
tabela2013<- tabela[tabela[["leto"]] == "2013",]
tabela2014<- tabela[tabela[["leto"]] == "2014",]

#Okenca, za katere ni podatka in so oznacena z "-", zamenjamo z "NA":
tabela[tabela == "-"] <- NA


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
podatkiHTML <- apply(podatkiHTML, 2, as.numeric)




#Filtriramo podatke za leta
podatkiLETA<- grep("var xValues", html, value=TRUE) %>%
  strapplyc('var xValues="([^"]+)"') %>% .[[1]] %>%
  strsplit("|", fixed=TRUE) %>% unlist() %>%
  matrix(nrow=1)

#vektor vseh let:
novipodatkiLETA<-podatkiLETA[seq(7,91,9)]

#Filtriramo podatke za države (če vnesem vrednost "colnames" vrne vse države)
podatkiDRZAVE <- grep("yValues", html, value = TRUE) %>%
  strapplyc('yValues="([^"]+)"') %>% .[[1]] %>%
  strsplit("|", fixed=TRUE) %>% unlist() %>%
  matrix(ncol=1)

#vekror vseh držav 
novipodatkiDRZAVE<-podatkiDRZAVE[seq(8,514,9)]

#podatke o letih, državah in prirastku združimo v tabelo
podatkiHTML <- data.frame(podatkiHTML, row.names = novipodatkiDRZAVE)
names(podatkiHTML) <- novipodatkiLETA

#izbriše prvih 7 stolpcev
podatkiHTML<-podatkiHTML[-(1:7),]

#izbrišemo nepotrebne vrstice
podatkiHTML<-podatkiHTML[,-(1:6)]
podatkiHTML<-podatkiHTML[-4,]
podatkiHTML<-podatkiHTML[-(29:31),]

