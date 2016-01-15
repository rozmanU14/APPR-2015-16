# 4. faza: Analiza podatkov
library(MASS)
j<- ggplot(tabela, aes(x = naravni.prirast.moski, y = naravni.prirast.zenske)) + geom_point()
j + geom_smooth(method = "lm")

u<- ggplot(tabela, aes(x = zivorojeni.moski, y = zivorojene.zenske)) + geom_point()
u + geom_smooth(method = "lm")

v<- ggplot(tabela, aes(x = umrli.moski, y = umrle.zenske)) + geom_point()
v + geom_smooth(method = "lm")


#prikazuje razmerje med zivorojenimi ženskami/moški.

#NAPOVED PRIRASTA
nnaravni.prirast<-tabela$skupni.prirast
lleto<-tabela$leto
zzivorojeni<- tabela$zivorojeni.moski + tabela$zivorojene.zenske
uumrli<- tabela$umrle.zenske + tabela$umrli.moski
plot(tabela$leto,tabela$skupni.prirast, xlim=c(2010,2020),ylim=c(-100,300),
     xlab="Leto",ylab="Skupni naravni prirast",
     main="Napoved naravnega prirasta za Slovenijo",pch=20,col="lightblue",type="p",lwd=3.5)
#linearna
linearna<-lm(nnaravni.prirast~lleto)
abline(ln,col="red")

#kvadratna
kvadratna<-lm(nnaravni.prirast~I(lleto^2)+lleto)
curve(predict(kvp, data.frame(lleto=x)), add = TRUE, col = "green")

#loess
loep<-loess(nnaravni.prirast~lleto)
curve(predict(loep, data.frame(lleto=x)),add=TRUE,col="blue")
#legenda
legend("topright", c("Linerana metoda", "Kvadratna metoda","Loess"),lty=c(1,1,1), col = c("red","green","blue"))
#ocenimo kako dobro je prileganje
ostp<-sapply(list(linearna, kvadratna, loep), function(x) sum(x$residuals^2))
sapply(list(linearna, kvadratna, loep), function(x) sum(x$residuals^2))
#najmanjši ostanek je najbolj natančna napoved (loes)

#NAPOVED ŠTEVILA ŽIVOROJENIH
plot(lleto,zzivorojeni, xlim=c(2010,2020),ylim=c(0,700),
     xlab="Leto",ylab="Število živorojenih",
     main="Napoved rasti živorojenih",pch=20,col="lightblue",type="p",lwd=3.5)
#linearna
linearna<-lm(zzivorojeni~lleto)
abline(ln,col="red")

#kvadratna
kvadratna<-lm(zzivorojeni~I(lleto^2)+lleto)
curve(predict(kvp, data.frame(lleto=x)), add = TRUE, col = "green")

#loess
loep<-loess(zzivorojeni~lleto)
curve(predict(loep, data.frame(lleto=x)),add=TRUE,col="blue")
legend("topright", c("Linerana metoda", "Kvadratna metoda","Loess"),lty=c(1,1,1), col = c("red","green","blue"))

#NAPOVED RASTI UMRLIH
plot(lleto,uumrli, xlim=c(2010,2020),ylim=c(0,700),
     xlab="Leto",ylab="Število umrlih",
     main="Napoved rasti umrlih",pch=20,col="lightblue",type="p",lwd=3.5)
linearna<-lm(uumrli~lleto)
abline(ln,col="red")
kvadratna<-lm(uumrli~I(lleto^2)+lleto)
curve(predict(kvp, data.frame(lleto=x)), add = TRUE, col = "green")

#loess
loep<-loess(uumrli~lleto)
curve(predict(loep, data.frame(lleto=x)),add=TRUE,col="blue")
legend("topright", c("Linerana metoda", "Kvadratna metoda","Loess"),lty=c(1,1,1), col = c("red","green","blue"))

