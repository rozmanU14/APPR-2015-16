
library(dplyr)
library(gsubfn)
library(ggplot2)

#Vektor, ki predstavlja imena stolpcev:
nova.kolona<-c("kraj", "leto","zivorojeni moski","zivorojene zenske","umrli moski","umrle zenske","naravni prirast moski","naravni prirast zenske")
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
cat("Uvazam podatke o naravnem prirastku...\n")
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
cat("Pretvorba stolpcev v stevilske spremenljivke...\n")
tabela$zivorojeni.moski <- as.numeric(tabela$zivorojeni.moski)
tabela$zivorojene.zenske <- as.numeric(tabela$zivorojene.zenske)
tabela$umrle.zenske <- as.numeric(tabela$umrle.zenske)
tabela$umrli.moski <- as.numeric(tabela$umrli.moski)
tabela$naravni.prirast.zenske <- as.numeric(tabela$naravni.prirast.zenske)
tabela$naravni.prirast.moski <- as.numeric(tabela$naravni.prirast.moski)


#seštevanje dveh stolpcev in ustvarjanje novega
tabela["skupni.prirast"]<-tabela$naravni.prirast.moski+ tabela$naravni.prirast.zenske


#Naredimo tabelo, kjer so podatki za vsaki leto posebaj:


tabela2010<-podatki[["2010"]]
tabela2011<-podatki[["2011"]]
tabela2012<-podatki[["2012"]]
tabela2013<-podatki[["2013"]]
tabela2014<-podatki[["2014"]]


tabela2010["skupni.prirast"]<-tabela2010$naravni.prirast.moski+ tabela2010$naravni.prirast.zenske
tabela2011["skupni.prirast"]<-tabela2011$naravni.prirast.moski+ tabela2011$naravni.prirast.zenske
tabela2012["skupni.prirast"]<-tabela2012$naravni.prirast.moski+ tabela2012$naravni.prirast.zenske
tabela2013["skupni.prirast"]<-tabela2013$naravni.prirast.moski+ tabela2013$naravni.prirast.zenske
tabela2014["skupni.prirast"]<-tabela2014$naravni.prirast.moski+ tabela2014$naravni.prirast.zenske

tabela2010$skupni.prirast<-as.numeric(tabela2010$skupni.prirast)
tabela2011$skupni.prirast<-as.numeric(tabela2011$skupni.prirast)
tabela2012$skupni.prirast<-as.numeric(tabela2012$skupni.prirast)
tabela2013$skupni.prirast<-as.numeric(tabela2013$skupni.prirast)
tabela2014$skupni.prirast<-as.numeric(tabela2014$skupni.prirast)

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


##########################################################
#grafi za leto 2010 pri katerih je prikazan naravni prirast, ki je ločen glede na velikost
tabela2010["skupni.prirast"]<-tabela2010$naravni.prirast.moski+ tabela2010$naravni.prirast.zenske


attach(tabela2010)
kategorije<-c('pozitiven','negativen','ni prirastka')
velikost<-"skupni.prirast"
velikost[skupni.prirast==0]<-'ni prirastka'
velikost[skupni.prirast < 0]<-'negativen'
velikost[skupni.prirast>0]<-'pozitiven'
velikost<-factor(velikost,levels=kategorije,ordered=TRUE)
detach(tabela2010)
dodatenstolpec<-data.frame(velikost)
tabela2010<-data.frame(tabela,velikost)
tabela2010<-tabela2010[,-11]


ggplot(data=tabela2010 %>% filter(velikost=="negativen"), aes(x=kraj, y=skupni.prirast)) + geom_point()
ggplot(data=tabela2010 %>% filter(velikost=="pozitiven"), aes(x=kraj, y=skupni.prirast)) + geom_point()
ggplot(data=tabela2010 %>% filter(velikost=="ni prirastka"), aes(x=kraj, y=skupni.prirast)) + geom_point()

#tabela ki prikaže kraje s skupnim prirastkom več kot 100
ggplot(data=tabela %>% filter(skupni.prirast>100), aes(x=kraj, y=skupni.prirast,color=leto)) + geom_point()
ggplot(data=tabela %>% filter(skupni.prirast>100), aes(x=kraj, y=skupni.prirast,color=leto)) + geom_point()


#grafi
ptuj<-tabela[tabela[["kraj"]] == "Ptuj",]
ggplot(data=ptuj, aes(y=umrle.zenske,x=leto)) + geom_point() 


#graf prikazuje naravni prirastek po krajih, barve pik razlikujejo leta
p<-ggplot(tabela) + aes(x = kraj, y = naravni.prirast.moski) + geom_point()
p + aes(x = kraj, y = naravni.prirast.moski, color = leto) + geom_point()
#####################################################################
#ZEMLJEVIDI

source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
library(ggplot2)
library(dplyr)

pretvori.zemljevid <- function(zemljevid) {
  fo <- fortify(zemljevid)
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}
obc <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
                          "OB/OB", encoding = "Windows-1250")
obc <- obc[order(as.character(obc$OB_UIME)),]
rownames(tabela2011) <- tabela2011$kraj
tabela2011["Ankaran",] <- rep(NA, ncol(tabela2011))
tabela2011$kraj <- rownames(tabela2011)
tabela2011 <- tabela2011[order(tabela2011$kraj),]
obc$PRIRAST<-tabela2011$skupni.prirast
obc$RODNOST<-tabela2011$zivorojeni.moski + tabela2011$zivorojene.zenske7
obc$UMRLIVOST<-tabela2011$umrli.moski + tabela2011$umrle.zenske
#obc <- pretvori.zemljevid(obc)

zem <- ggplot() + geom_polygon(data = obc, aes(x=long, y=lat, group=group,
                                              fill="RODNOST"),
                               color = "grey") +
  scale_fill_gradient(low="white", high="#00FF00") +
  guides(fill = guide_colorbar(title = "Rodnost"))+
print(zem)




