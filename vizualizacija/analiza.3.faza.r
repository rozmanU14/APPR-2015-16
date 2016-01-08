
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



zem3<-ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = UMRLIVOST2011),color = "grey") +
  scale_fill_gradient(low="#fded75", high= "#100f00") +
  guides(fill = guide_colorbar(title = "Umrlivost 2011"))
#Zemljevid prikazuje umrljivost v letu 2011 po občinah

zem4<-ggplot() + geom_polygon(data = obc, aes(x = long, y = lat, group = group, fill = RODNOST2011),color = "grey") +
  scale_fill_gradient(low="#a65353", high= "#582b2b") +
  guides(fill = guide_colorbar(title = "Rodnost 2011"))
#Zemljevid prikazuje rodnost v letu 2011 po občinah. 

#Analiza največjega naravnega prirastka po letih:
#2010:
filter(tabela2010, skupni.prirast == max(skupni.prirast, na.rm=TRUE)) #Lj
filter(tabela2010, skupni.prirast == min(skupni.prirast, na.rm=TRUE)) #Mb
#2011
filter(tabela2011, skupni.prirast == max(skupni.prirast, na.rm=TRUE)) #Lj
filter(tabela2011, skupni.prirast == min(skupni.prirast, na.rm=TRUE)) #Mb
#2012
filter(tabela2012, skupni.prirast == max(skupni.prirast, na.rm=TRUE)) #Lj
filter(tabela2012, skupni.prirast == min(skupni.prirast, na.rm=TRUE)) #Mb
#2013
filter(tabela2013, skupni.prirast == max(skupni.prirast, na.rm=TRUE)) #Lj
filter(tabela2013, skupni.prirast == min(skupni.prirast, na.rm=TRUE)) #Mb
#2014
filter(tabela2014, skupni.prirast == max(skupni.prirast, na.rm=TRUE)) #Lj
filter(tabela2014, skupni.prirast == min(skupni.prirast, na.rm=TRUE)) #Mb


#Ugotovila sem da je med leto 2010 in 2014 naarvni
#prirast največji v Ljubljani in najmanjši v Mariboru.

#Povprečen naravni prirast po letih:
#2010 = 1.532
povp.pr.2010<-sum(tabela2010$skupni.prirast,na.rm=TRUE) / nrow(tabela2010)/10
#2011 = 1.539
povp.pr.2011<-sum(tabela2011$skupni.prirast,na.rm=TRUE) /nrow(tabela2011)/10
#2012 = 1.264
povp.pr.2012<- sum(tabela2012$skupni.prirast,na.rm=TRUE) /nrow(tabela2012)/10
#2013 = 0.838
povp.pr.2013<-sum(tabela2013$skupni.prirast,na.rm=TRUE) /nrow(tabela2013)/10
#2014 = 0.842
povp.pr.2014<-sum(tabela2014$skupni.prirast,na.rm=TRUE) /nrow(tabela2014)/10

ggplot(tabela) + aes(x = kraj, y = skupni.prirast) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + 
  geom_hline(yintercept = c(povp.pr.2014,povp.pr.2013), color = c("red", "blue"))

#Grafi prikazujejo naravni prirastek občin po letih, črta prikazuje povprečen prirast.
ggplot(tabela2010) + aes(x = kraj, y = skupni.prirast) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =povp.pr.2010,color="red")
ggplot(tabela2011) + aes(x = kraj, y = skupni.prirast) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_point() + geom_hline(yintercept =povp.pr.2011,color="blue")
ggplot(tabela2012) + aes(x = kraj, y = skupni.prirast) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =povp.pr.2012,color="yellow")
ggplot(tabela2013) + aes(x = kraj, y = skupni.prirast) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =povp.pr.2013,color="orange")

#Ugotovila sem, da je naravni prirast po letih od 2010 do 2013 padal,
#nato se 2014 spet narastel.

#Povprečno število živorojenih moških in žensk po letih ločeno glede na spol.
#2010:
ziv.z.2010<-sum(tabela2010$zivorojene.zenske,na.rm=TRUE) /211 #51.37
ziv.m.2010<-sum(tabela2010$zivorojeni.moski,na.rm=TRUE) /211 #54.52
#Grafa prikazujeta število živorojenih žensk in moških v občinah,
#črta prikazuje povprečno število živorojenih.
ggplot(tabela2010) + aes(x = kraj, y = zivorojene.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.z.2010,color="orange")
ggplot(tabela2010) + aes(x = kraj, y = zivorojeni.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.m.2010,color="purple")

#2011
ziv.z.2011<-sum(tabela2011$zivorojene.zenske,na.rm=TRUE) /211 #50.71
ziv.m.2011<-sum(tabela2011$zivorojeni.moski,na.rm=TRUE) /211 #53.31
#Grafa prikazujeta število živorojenih žensk in moških v občinah,
#črta prikazuje povprečno število živorojenih.
ggplot(tabela2011) + aes(x = kraj, y = zivorojene.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.z.2011,color="pink")
ggplot(tabela2011) + aes(x = kraj, y = zivorojeni.moski) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.m.2011,color="red")

#2012
ziv.z.2012<-sum(tabela2012$zivorojene.zenske,na.rm=TRUE) /211 #50.33
ziv.m.2012<-sum(tabela2012$zivorojeni.moski,na.rm=TRUE) /211 #53.64
#Grafa prikazujeta število živorojenih žensk in moških v občinah,
#črta prikazuje povprečno število živorojenih.
ggplot(tabela2012) + aes(x = kraj, y = zivorojene.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.z.2012,color="yellow")
ggplot(tabela2012) + aes(x = kraj, y = zivorojeni.moski) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.m.2012,color="blue")

#2013
ziv.z.2013<-sum(tabela2013$zivorojene.zenske,na.rm=TRUE) /211 #48.85
ziv.m.2013<-sum(tabela2013$zivorojeni.moski,na.rm=TRUE) /211 #51.20
#Grafa prikazujeta število živorojenih žensk in moških v občinah,
#črta prikazuje povprečno število živorojenih.
ggplot(tabela2013) + aes(x = kraj, y = zivorojene.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.z.2013,color="green")
ggplot(tabela2013) + aes(x = kraj, y = zivorojeni.moski) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.m.2013,color="brown")

#2014
ziv.z.2014<-sum(tabela2014$zivorojene.zenske,na.rm=TRUE) /211 #48.52
ziv.m.2014<-sum(tabela2014$zivorojeni.moski,na.rm=TRUE) /211 #51.79
#Grafa prikazujeta število živorojenih žensk in moških v občinah,
#črta prikazuje povprečno število živorojenih.
ggplot(tabela2014) + aes(x = kraj, y = zivorojene.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.z.2014,color="gold")
ggplot(tabela2014) + aes(x = kraj, y = zivorojeni.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =ziv.m.2014,color="red")
#Ugotovila sem, da poprečnono število živorojenih žensk pada, prav tako tudi 
#število živorojenih moških. Vsako leto pa je povprečno število živorojenih 
#moških večje, od števila živorojenih žensk.

sum(tabela$zivorojene.zenske,na.rm=TRUE) #52701
sum(tabela$zivorojeni.moski,na.rm=TRUE) #55803
#v povprečju se na leto rodi več moških kot žensk

#Povprečno število umrlih moških in žensk po letih ločeno glede na spol.
#2010:
umr.z.2010<-sum(tabela2010$umrle.zenske,na.rm=TRUE) /211 #44.16
umr.m.2010<-sum(tabela2010$umrli.moski,na.rm=TRUE) /211 #44.04
#Grafa prikazujeta število umrlih žensk in moških v občinah,
#črta prikazuje povprečno število umrlih.
ggplot(tabela2010) + aes(x = kraj, y = umrle.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.z.2010,color="red")
ggplot(tabela2010) + aes(x = kraj, y = umrli.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.m.2010,color="blue")

#2011:
umr.z.2011<-sum(tabela2011$umrle.zenske,na.rm=TRUE) /211 #44.85
umr.m.2011<-sum(tabela2011$umrli.moski,na.rm=TRUE) /211 #43.77
#Grafa prikazujeta število umrlih žensk in moških v občinah,
#črta prikazuje povprečno število umrlih.
ggplot(tabela2011) + aes(x = kraj, y = umrle.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.z.2011,color="yellow")
ggplot(tabela2011) + aes(x = kraj, y = umrli.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.m.2011,color="green")

#2012:
umr.z.2012<-sum(tabela2012$umrle.zenske,na.rm=TRUE) /211 #46.66
umr.m.2012<-sum(tabela2012$umrli.moski,na.rm=TRUE) /211 #44.61
#Grafa prikazujeta število umrlih žensk in moških v občinah,
#črta prikazuje povprečno število umrlih.
ggplot(tabela2012) + aes(x = kraj, y = umrle.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.z.2012,color="pink")
ggplot(tabela2012) + aes(x = kraj, y = umrli.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.m.2012,color="purple")

#2013:
umr.z.2013<-sum(tabela2013$umrle.zenske,na.rm=TRUE) /211 #46.35
umr.m.2013<-sum(tabela2013$umrli.moski,na.rm=TRUE) /211 #45.28
#Grafa prikazujeta število umrlih žensk in moških v občinah,
#črta prikazuje povprečno število umrlih.
ggplot(tabela2013) + aes(x = kraj, y = umrle.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.z.2013,color="red")
ggplot(tabela2013) + aes(x = kraj, y = umrli.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.m.2013,color="blue")

#2014:
umr.z.2014<-sum(tabela2014$umrle.zenske,na.rm=TRUE) /211 #45.88
umr.m.2014<-sum(tabela2014$umrli.moski,na.rm=TRUE) /211 #43.63
#Grafa prikazujeta število umrlih žensk in moških v občinah,
#črta prikazuje povprečno število umrlih.
ggplot(tabela2014) + aes(x = kraj, y = umrle.zenske) + 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.z.2014,color="yellow")
ggplot(tabela2014) + aes(x = kraj, y = umrli.moski)+ 
  geom_point() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_point() + geom_hline(yintercept =umr.m.2014,color="green") 
#Ugotovila sem, da v povprečju vsako leto umre več žensk kot moških. 
#Podatki po letih so približno konstantni.
sum(tabela$umrli.moski,na.rm=TRUE) #46702
sum(tabela$umrle.zenske,na.rm=TRUE) #48083

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
sum(podatkiHTML$"2010", na.rm=TRUE) / nrow(podatkiHTML) #1.48
#2011
sum(podatkiHTML$"2011", na.rm=TRUE) / nrow(podatkiHTML) #1.34
#2012
sum(podatkiHTML$"2012", na.rm=TRUE) / nrow(podatkiHTML) #1.42
#2013
sum(podatkiHTML$"2013", na.rm=TRUE) / nrow(podatkiHTML) #1.26

#Ugotovila sem, da je povprečni slovenski prirast večji od povprečnega evropskega po letih.

