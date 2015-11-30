# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function() {
  return(read.table("podatki/druzine.csv", sep = ";", as.is = TRUE,
                      row.names = 1,
                      col.names = c("obcina", "en", "dva", "tri", "stiri"),
                      fileEncoding = "Windows-1250"))
}

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine()

obcine <- uvozi.obcine()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

#Vektor, ki predstavlja imena stolpcev:
nova.kolona<-c("kraj", "leto","živorojeni moški","živorojene ženske","umrli moški","umrle ženske","naravni prirast moški","naravni prirast ženske")
^#Funkcija, ki uvozi podatke iz datoteke podatki.csv

uvozi<-function(){
    return(read.csv2(file="podatki/prirastek.csv",
                    col.names=nova.kolona,
                    header=FALSE,
                    fileEncoding = "UTF-8"))
                }
#Zapišemo podatke v razpredelnivo tabela
cat("Uvažam podatke o naravnem prirastku...\n")
tabela<-uvozi()

#poskrbimo, da so številske spremenljivke res številske
cat("Pretvorba stolpcev v številske spremenljivke...\n")
> tabela$živorojeni.moški <- as.integer(tabela$živorojeni.moški)
> tabela$živorojene.ženske <- as.integer(tabela$živorojene.ženske)
> tabela$umrle.ženske <- as.integer(tabela$umrle.ženske)
> tabela$umrli.moški <- as.integer(tabela$umrli.moški)
> tabela$naravni.prirast.ženske <- as.integer(tabela$naravni.prirast.ženske)
> tabela$naravni.prirast.moški <- as.integer(tabela$naravni.prirast.moški)

#Naredimo novo kolono "skupni naravni prirastek"
tabela["skupni prirast"]<-tabela$naravni.prirast.moški + tabela$naravni.prirast.ženske
