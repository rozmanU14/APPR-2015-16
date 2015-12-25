
library(dplyr)
library(gsubfn)
library(ggplot2)
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
library(ggplot2)
library(dplyr)

#GRAFI

negativen.prirast2010<-ggplot(data=tabela2010 %>% filter(velikost=="negativen"), aes(x=kraj, y=skupni.prirast)) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+geom_point()
pozitiven.prirast2010<-ggplot(data=tabela2010 %>% filter(velikost=="pozitiven"), aes(x=kraj, y=skupni.prirast)) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+geom_point()
ni.prirasta2010<-ggplot(data=tabela2010 %>% filter(velikost=="ni prirastka"), aes(x=kraj, y=skupni.prirast)) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+geom_point()
#grafi prikazujejo občine, ki so ločene glede na velikost prirasta. Ločeni so glede na tri kategorije, poztiven prirast, negativen prirast ter občine brez prirasta.

#tabela ki prikaže kraje s skupnim prirastkom več kot 100
ggplot(data=tabela %>% filter(skupni.prirast>100), aes(x=kraj, y=skupni.prirast,color=leto)) + geom_point()
ggplot(data=tabela %>% filter(skupni.prirast>100), aes(x=kraj, y=skupni.prirast,color=leto)) + geom_point()


#grafi
ptuj<-tabela[tabela[["kraj"]] == "Ptuj",]
ggplot(data=ptuj, aes(y=umrle.zenske,x=leto)) + geom_point() 


#graf prikazuje naravni prirastek po krajih, barve pik razlikujejo leta
p<-ggplot(tabela) + aes(x = kraj, y = naravni.prirast.moski) + geom_point()
p + aes(x = kraj, y = naravni.prirast.moski, color = leto) + geom_point()


ggplot(data=tabela2014,aes(y=umrli.moski,x =kraj),color="blue")+geom_point()+geom_point(aes(y=umrle.zenske,x=kraj),color="yellow")+geom_point()+geom_point(aes(y=skupni.prirast,x=kraj),color="orange")
#ZEMLJEVIDI


#tabelam dodamo Ankaran ki je na novo nastala občina
rownames(tabela2010) <- tabela2010$kraj
tabela2010["Ankaran",] <- rep(NA, ncol(tabela2010))
tabela2010$kraj <- rownames(tabela2010)
tabela2010 <- tabela2011[order(tabela2010$kraj),]

rownames(tabela2011) <- tabela2011$kraj
tabela2011["Ankaran",] <- rep(NA, ncol(tabela2011))
tabela2011$kraj <- rownames(tabela2011)
tabela2011 <- tabela2011[order(tabela2011$kraj),]

rownames(tabela2012) <- tabela2012$kraj
tabela2012["Ankaran",] <- rep(NA, ncol(tabela2012))
tabela2012$kraj <- rownames(tabela2012)
tabela2012 <- tabela2012[order(tabela2012$kraj),]

rownames(tabela2013) <- tabela2013$kraj
tabela2013["Ankaran",] <- rep(NA, ncol(tabela2013))
tabela2013$kraj <- rownames(tabela2013)
tabela2013<-tabela2013[order(tabela2013$kraj),]

rownames(tabela2014) <- tabela2014$kraj
tabela2014["Ankaran",] <- rep(NA, ncol(tabela2014))
tabela2014$kraj <- rownames(tabela2014)
tabela2014<-tabela2013[order(tabela2014$kraj),]
pretvori.zemljevid <- function(zemljevid) {
  fo <- fortify(zemljevid)
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}
obc <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
                       "OB/OB", encoding = "Windows-1250")
obc <- obc[order(as.character(obc$OB_UIME)),]




obc$PRIRAST2011<-tabela2011$skupni.prirast
obc$RODNOST2011<-tabela2011$zivorojeni.moski + tabela2011$zivorojene.zenske
obc$UMRLIVOST2011<-tabela2011$umrli.moski + tabela2011$umrle.zenske
obc$VELIKOST<- tabela2011$velikost
obc <- pretvori.zemljevid(obc)

#Zemljevid prikazuje naravni prirast v letu 2011. 
zem1<- ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = PRIRAST2011),color = "grey") +
  scale_fill_gradient(low="#d6b6ac", high="#090604") +
  guides(fill = guide_colorbar(title = "Naravni prirast 2011"))


#zemljevid prikazuje v katerih občinah je prirast pozitiven, v katerih negativen in v katerih ni prirastka
zem2<-zem1+ geom_point(data = obc , aes(x = Y_C, y = X_C, color = VELIKOST)) +
  scale_color_manual(name="Tip", breaks = c("POZITIVEN", "NEGATIVEN","NI PRIRASTKA"),
                     labels = c("POZITIVEN", "NEGATIVEN","NI PRIRASTKA"),
                     values = c("blue", "red","orange")) 



ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = UMRLIVOST2011),color = "grey") +
  scale_fill_gradient(low="#fded75", high= "#100f00") +
  guides(fill = guide_colorbar(title = "Umrlivost 2011"))
#Zemljevid prikazuje umrljivost v letu 2011 po občinah

ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = RODNOST2011),color = "grey") +
  scale_fill_gradient(low="#a65353", high= "#582b2b") +
  guides(fill = guide_colorbar(title = "Rodnost 2011"))
#Zemljevid prikazuje rodnost v letu 2011 po občinah. 

#Analiza največjega naravnega prirastka po letih:
#2010:
max(tabela2010$skupni.prirast, na.rm=TRUE)
tabela2010[tabela2010[["skupni.prirast"]] == "925",] #Ljubljana
min(tabela2010$skupni.prirast, na.rm=TRUE)
tabela2010[tabela2010[["skupni.prirast"]] == "-212",] #Maribor
#2011
max(tabela2011$skupni.prirast, na.rm=TRUE)
tabela2011[tabela2011[["skupni.prirast"]] == "792",] #Ljubljana 
min(tabela2011$skupni.prirast, na.rm=TRUE)
tabela2011[tabela2010[["skupni.prirast"]] == "-220",] #Maribor
#2012
max(tabela2012$skupni.prirast, na.rm=TRUE)
tabela2012[tabela2012[["skupni.prirast"]] == "812",]  #Ljubljana
min(tabela2012$skupni.prirast, na.rm=TRUE)
tabela2012[tabela2012[["skupni.prirast"]] == "-319",] #Maribor
#2013
max(tabela2013$skupni.prirast, na.rm=TRUE)
tabela2013[tabela2012[["skupni.prirast"]] == "740",] #Ljubljana
min(tabela2013$skupni.prirast, na.rm=TRUE)
tabela2013[tabela2013[["skupni.prirast"]] == "-272",] #Maribor
#2014
max(tabela2014$skupni.prirast, na.rm=TRUE)
tabela2014[tabela2014[["skupni.prirast"]] == "702",] #Ljubljana
min(tabela2013$skupni.prirast, na.rm=TRUE)
tabela2013[tabela2013[["skupni.prirast"]] == "-231",] #Maribor
#Ugotovila sem da je med leto 2010 in 2014 naarvni prirast največji v Ljubljani in najmanjši v Mariboru.

tabela2010[tabela2010$velikost == "pozitiven" , c("kraj")]
