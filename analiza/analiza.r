# 4. faza: Analiza podatkov
library(MASS)
j<- ggplot(tabela, aes(x = naravni.prirast.moski, y = naravni.prirast.zenske)) + geom_point()
j + geom_smooth(method = "lm")

u<- ggplot(tabela, aes(x = zivorojeni.moski, y = zivorojene.zenske)) + geom_point()
u + geom_smooth(method = "lm")

v<- ggplot(tabela, aes(x = umrli.moski, y = umrle.zenske)) + geom_point()
v + geom_smooth(method = "lm")

w<- ggplot(tabela, aes(x = naravni.prirast.zenske, y =naravni.prirast.moski)) + geom_point()
w + geom_smooth(method = "lm")

k<- ggplot(tabela, aes(x = zivorojeni.na.1000, y =zivorojeni.moski)) + geom_point()
k + geom_smooth(method = "lm")

#prikazuje razmerje med zivorojenimi ženskami/moški.