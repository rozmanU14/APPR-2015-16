
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

obc$PRIRAST2011<-tabela2011$naravni.prirast.na.1000.prebivalcev
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



zem3<-ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = UMRLIVOST2011),color = "grey") +
  scale_fill_gradient(low="#fded75", high= "#100f00") +
  guides(fill = guide_colorbar(title = "Umrlivost 2011")) +
  geom_text(data = obc %>% filter(OB_IME == c("BELTINCI","BRDA","LJUBLJANA","MARIBOR")),
          aes(x = Y_C, y = X_C, label = OB_IME),          size = 3, vjust = 2)
#Zemljevid prikazuje umrljivost v letu 2011 po občinah
filter(tabela2011, umrli.na.1000 == max(umrli.na.1000, na.rm=TRUE)) 
filter(tabela2011, umrli.na.1000 == min(umrli.na.1000, na.rm=TRUE)) 
#Največjo absolutno umrljivost ima Maribor, največjo relativno pa Brda. 
#Najmanjšo absolutno umrlivost ima Ljubljana, najmanjšo relativno pa Beltinci.

zem4<-ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = RODNOST2011),color = "grey") +
  scale_fill_gradient(low="#a65353", high= "#582b2b") +
  guides(fill = guide_colorbar(title = "Rodnost 2011")) +
  geom_text(data = obc %>% filter(OB_IME == c("LJUTOMER","HODOŠ")),
                                                                   aes(x = Y_C, y = X_C, label = OB_IME),
                                                                   size = 3, vjust = 2)
#Zemljevid prikazuje rodnost v letu 2011 po občinah. Leta 2011 je bila največja 
#rodnost v Ljutomerju, najmanjša pa v Hodošu.
filter(tabela2011, zivorojeni.na.1000 == max(zivorojeni.na.1000, na.rm=TRUE)) 
filter(tabela2011, zivorojeni.na.1000 == min(zivorojeni.na.1000, na.rm=TRUE)) 
 


#Analiza največjega relativnega naravnega prirastka po letih:
#2010:
filter(tabela2010, naravni.prirast.na.1000.prebivalcev == max(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #moravče
filter(tabela2010, naravni.prirast.na.1000.prebivalcev == min(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #šmarje pri jelšah
#2011
filter(tabela2011, naravni.prirast.na.1000.prebivalcev == max(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #dol pri ljubljani
filter(tabela2011, naravni.prirast.na.1000.prebivalcev == min(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #dravograd
#2012
filter(tabela2012, naravni.prirast.na.1000.prebivalcev == max(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #gorenja vas
filter(tabela2012, naravni.prirast.na.1000.prebivalcev == min(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE))  # črnomelj
#2013
filter(tabela2013, naravni.prirast.na.1000.prebivalcev == max(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #sončava
filter(tabela2013, naravni.prirast.na.1000.prebivalcev == min(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #radlje ob dravi
#2014
filter(tabela2014, naravni.prirast.na.1000.prebivalcev == max(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #Škofljica
filter(tabela2014, naravni.prirast.na.1000.prebivalcev == min(naravni.prirast.na.1000.prebivalcev, na.rm=TRUE)) #ruše

#Ugotovila sem, da je v letih od 2010 do 2014 relatven prirast največji v občinah 
#Moravče, Dol pri Ljubljani, Gorenja vas Sončava in Škofljica,
#najmanši pa v občinah Šmarje pri Jelšah, Dravograd, Črnomelj, Radlje ob Dravi in Ruše.

#Povprečen naravni prirast po letih:
#2010 = 1.532
povp.pr.2010<-mean(tabela2010$naravni.prirast.na.1000.prebivalcev,na.rm=TRUE) 
#2011 = 1.539
povp.pr.2011<-mean(tabela2011$naravni.prirast.na.1000.prebivalcev,na.rm=TRUE) 
#2012 = 1.264
povp.pr.2012<- mean(tabela2012$naravni.prirast.na.1000.prebivalcev,na.rm=TRUE)
#2013 = 0.838
povp.pr.2013<-mean(tabela2013$naravni.prirast.na.1000.prebivalcev,na.rm=TRUE)
#2014 = 0.842
povp.pr.2014<-mean(tabela2014$naravni.prirast.na.1000.prebivalcev,na.rm=TRUE)

#Ugotovila sem, da je naravni prirast po letih od 2010 do 2013 padal,
#nato se 2014 spet narastel.

#Povprečno število živorojenih moških in žensk po letih ločeno glede na spol.
#2010:
ziv.z.2010<-mean(tabela2010$zivorojene.zenske,na.rm=TRUE)  #51.37
ziv.m.2010<-mean(tabela2010$zivorojeni.moski,na.rm=TRUE) #54.52

#2011
ziv.z.2011<-mean(tabela2011$zivorojene.zenske,na.rm=TRUE)  #50.71
ziv.m.2011<-mean(tabela2011$zivorojeni.moski,na.rm=TRUE)  #53.31

#2012
ziv.z.2012<-mean(tabela2012$zivorojene.zenske,na.rm=TRUE)  #50.33
ziv.m.2012<-mean(tabela2012$zivorojeni.moski,na.rm=TRUE) #53.64

#2013
ziv.z.2013<-mean(tabela2013$zivorojene.zenske,na.rm=TRUE)  #48.85
ziv.m.2013<-mean(tabela2013$zivorojeni.moski,na.rm=TRUE)  #51.20

#2014
ziv.z.2014<-mean(tabela2014$zivorojene.zenske,na.rm=TRUE) #48.52
ziv.m.2014<-mean(tabela2014$zivorojeni.moski,na.rm=TRUE)  #51.79

#Ugotovila sem, da poprečnono število živorojenih žensk pada, prav tako tudi 
#število živorojenih moških. Vsako leto pa je povprečno število živorojenih 
#moških večje, od števila živorojenih žensk.
zz10<-sum(tabela2010$zivorojene.zenske,na.rm=TRUE)
zz11<-sum(tabela2011$zivorojene.zenske,na.rm=TRUE)
zz12<-sum(tabela2012$zivorojene.zenske,na.rm=TRUE)
zz13<-sum(tabela2013$zivorojene.zenske,na.rm=TRUE)
zz14<-sum(tabela2014$zivorojene.zenske,na.rm=TRUE)
zzz10<-sum(tabela2010$zivorojeni.moski,na.rm=TRUE)
zzz11<-sum(tabela2010$zivorojeni.moski,na.rm=TRUE)
zzz12<-sum(tabela2010$zivorojeni.moski,na.rm=TRUE)
zzz13<-sum(tabela2010$zivorojeni.moski,na.rm=TRUE)
zzz14<-sum(tabela2010$zivorojeni.moski,na.rm=TRUE)

pppodatki<-c("leto","zenske","moski")
pppodatki$leto<- c("2010","2011","2012","2013","2014")
pppodatki$zenske<- (c(zz10,zz11,zz12,zz13,zz14))
pppodatki$moski<- (c(zzz10,zzz11,zzz12,zzz13,zzz14))
rodnost=data.frame(pppodatki)
rodnost<-rodnost[-(1:3)]


sum(tabela$zivorojene.zenske,na.rm=TRUE) #52701
sum(tabela$zivorojeni.moski,na.rm=TRUE) #55803
#v povprečju se na leto rodi več moških kot žensk

#Povprečno število umrlih moških in žensk po letih ločeno glede na spol.
#2010:
umr.z.2010<-mean(tabela2010$umrle.zenske,na.rm=TRUE)  #44.16
umr.m.2010<-mean(tabela2010$umrli.moski,na.rm=TRUE)  #44.04

#2011:
umr.z.2011<-mean(tabela2011$umrle.zenske,na.rm=TRUE)  #44.85
umr.m.2011<-mean(tabela2011$umrli.moski,na.rm=TRUE)  #43.77

#2012:
umr.z.2012<-mean(tabela2012$umrle.zenske,na.rm=TRUE)  #46.66
umr.m.2012<-mean(tabela2012$umrli.moski,na.rm=TRUE) #44.61

#2013:
umr.z.2013<-mean(tabela2013$umrle.zenske,na.rm=TRUE)  #46.35
umr.m.2013<-mean(tabela2013$umrli.moski,na.rm=TRUE)  #45.28

#2014:
umr.z.2014<-mean(tabela2014$umrle.zenske,na.rm=TRUE)  #45.88
umr.m.2014<-mean(tabela2014$umrli.moski,na.rm=TRUE) #43.63
 
#Ugotovila sem, da v povprečju vsako leto umre več žensk kot moških. 
#Podatki po letih so približno konstantni.
sum(tabela$umrli.moski,na.rm=TRUE) #46702
sum(tabela$umrle.zenske,na.rm=TRUE) #48083

uu10<-sum(tabela2010$umrle.zenske,na.rm=TRUE)
uu11<-sum(tabela2011$umrle.zenske,na.rm=TRUE)
uu12<-sum(tabela2012$umrle.zenske,na.rm=TRUE)
uu13<-sum(tabela2013$umrle.zenske,na.rm=TRUE)
uu14<-sum(tabela2014$umrle.zenske,na.rm=TRUE)
uuu10<-sum(tabela2010$umrli.moski,na.rm=TRUE)
uuu11<-sum(tabela2011$umrli.moski,na.rm=TRUE)
uuu12<-sum(tabela2012$umrli.moski,na.rm=TRUE)
uuu13<-sum(tabela2013$umrli.moski,na.rm=TRUE)
uuu14<-sum(tabela2014$umrli.moski,na.rm=TRUE)
ppodatki<-c("leto","zenske","moski")

ppodatki$leto<- c("2010","2011","2012","2013","2014")
ppodatki$zenske<- (c(uu10,uu11,uu12,uu13,uu14))
ppodatki$moski<- (c(uuu10,uuu11,uuu12,uuu13,uuu14))
umrli=data.frame(ppodatki)
umrli<-umrli[-(1:3)]


#Primerjava Slovenskega prirasta z ostalimi Evropskimi državami:

#države z največjim in najmanjšim prirastkom:
#2010:
max(podatkiHTML$"2010",na.rm=TRUE) #2.19661
podatkiHTML[podatkiHTML[["2010"]] == "2.19661",] #Islandija
min(podatkiHTML$"2010",na.rm=TRUE) #1.24982
podatkiHTML[podatkiHTML[["2010"]] == "1.24982",] #Madžarska
#2011
max(podatkiHTML$"2011",na.rm=TRUE) #2.03052
podatkiHTML[podatkiHTML[["2011"]] == "2.03052",] #Turčija
min(podatkiHTML$"2010",na.rm=TRUE) #1.23322
podatkiHTML[podatkiHTML[["2010"]] == "1.23322",] #Madžarska
#2012
max(podatkiHTML$"2012",na.rm=TRUE) #2.08648
podatkiHTML[podatkiHTML[["2012"]] == "2.08648",] #Turčija
max(podatkiHTML$"2012",na.rm=TRUE) #2.03943
podatkiHTML[podatkiHTML[["2011"]] == "2.03943",] #Turčija
min(podatkiHTML$"2012",na.rm=TRUE) #1.25775
podatkiHTML[podatkiHTML[["2012"]] == "1.25775",] #San marino
#2013
max(podatkiHTML$"2013",na.rm=TRUE) #2.08498
podatkiHTML[podatkiHTML[["2013"]] == "2.08498",] #Turčija
min(podatkiHTML$"2013",na.rm=TRUE) #1.20925
podatkiHTML[podatkiHTML[["2013"]] == "1.20925",] #Portugalska
#Ugotovila sem, da je imela največji povp.naravni prirast v letu 2010 Islandija,
#v letih 2011-2013 pa Turčija"
#Najmanjši navani prirast izmed Evropskih držav je imela v letih 2010 in 2011 Madžarska, 
#v letu 2012 San marino, v letu 2013 pa Portugalska.

#Povprečen prirast v evropskih državah po letih:
#2010
mean(podatkiHTML$"2010", na.rm=TRUE)  #1.48
#2011
mean(podatkiHTML$"2011", na.rm=TRUE)  #1.34
#2012
mean(podatkiHTML$"2012", na.rm=TRUE)  #1.42
#2013
mean(podatkiHTML$"2013", na.rm=TRUE)  #1.26

#Ugotovila sem, da je povprečni slovenski prirast večji od povprečnega evropskega po letih.




