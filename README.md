# Analiza podatkov s programom R, 2015/16

Avtor: Urška Rožman


Repozitorij z gradivi pri predmetu APPR v študijskem letu 2015/16.

## Tematika: 

Izbrala sem si temo z naslovom Analiza naravnega prirastka v Sloveniji in primerjava naravnega prirastka z drugimi Evropskimi državami. Najprej bom natančneje analizirala rodnost, umrljivost in naravni prirastek v Sloveniji po regijah in spolu. V tabeli bom navedla naslednje podatke za 10 let (od 2005-2014):
 - ime regije (imenska spremenljivka)
 - leto (številska spremenljivka)
 - število živorojenih moških (številska spremenljivka)
 - število živorojenih ženske (številska spremenljivka)
 - število umrlih moških (številska spremenljivka)
 - število umrlih žensk (številska spremenljivka)
 - naravni prirastek moških (številska spremenljivka)
 - naravni prirastek žensk (številska spremenljivka)
 - skupni naravni prirastek (urejenostna spremenljivka)

Kasneje bom analizirala še naravni prirastek Evropskih držav. V tabeli bom navedla naslednje podatke, prav tako za obdobje desetih let):
- ime države (imenska spremenljivka)
- skupni naravni prirastek(številska spremenljivka)


## Cilj:

V projektu bom na podlagi zgornjih podatkov najprej ugotovila, v katerem kraju je največja rodnost glede na podano leto ter primerjala podatke med različnimi leti ter regijami. Izračunala bom povprečno rodnost in smrtnost za posamezno regijo glede na leta. Izračun bom ločila med izračun za moške, ženske in skupno. 
Izračunala bom maksimalno in minimalno rodnost po regijah ter jih med sabo primerjala.
Nato bom s pomočjo uvoženih podatkov za naravni prirastek žensk in moških izračunala skupni naravni prirastek, ki ga bom nato primerjala z naravnim prirastkom drugih Evropskih držav po telih. Dobljene rezultate bom prikazala tudi na zemljevidu.
 
## Podatki:
Podatke za projekt sem dobila na spletni strani EUROSTAT (druga tabela) in spletni strani Statističnega urada Republike Slovenije (prva tabela) . Da se bom ob izdelavi projekta naučila čim več, bom prvo tablo uzovila v CSV obliki, drugo pa v HTML obliki. 
Povezave na podatke:
- http://ec.europa.eu/eurostat/web/population-demography-migration-projections/births-fertitily-data/database
- http://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=05I1002S&ti=&path=../Database/Dem_soc/05_prebivalstvo/25_selitveno_gibanje/05_05I10_naravno_gibanje/&lang=2

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Spletni vmesnik

Spletni vmesnik se nahaja v datotekah v mapi `shiny/`. Poženemo ga tako, da v
RStudiu odpremo datoteko `server.R` ali `ui.R` ter kliknemo na gumb *Run App*.
Alternativno ga lahko poženemo tudi tako, da poženemo program `shiny.r`.

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `httr` - za pobiranje spletnih strani
* `XML` - za branje spletnih strani
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
