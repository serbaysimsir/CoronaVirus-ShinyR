library(shiny)
library(readxl)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(sqldf)
library(lubridate)
library(tidyverse) 
library(sp)
library(mapproj)
library(jpeg)
TUR <- readRDS("TUR_adm1.rds")
TUR_for <- fortify(TUR)
haritaverileri <- read_csv("database/vakalar.csv")
cols(
  il = col_integer(),
  vaka_sayisi = col_integer()
)
idliveriler <<- data_frame(id = rownames(TUR@data),
                               il = TUR@data$NAME_1) %>% 
    left_join(haritaverileri, by = "il")
final_ist_map <<- left_join(TUR_for, idliveriler, by = "id")
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Adıyaman' where il = 'Adiyaman'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Ağrı' where il = 'Agri'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Aydın' where il = 'Aydin'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Balıkesir' where il = 'Balikesir'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Çankırı' where il = 'Çankiri'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Diyarbakır' where il = 'Diyarbakir'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Eskişehir' where il = 'Eskisehir'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Gümüşhane' where il = 'Gümüshane'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'İstanbul' where il = 'Istanbul'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'İzmir' where il = 'Izmir'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Kahramanmaraş' where il = 'K. Maras'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Kırıkkale' where il = 'Kinkkale'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Kırklareli' where il = 'Kirklareli'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Kırşehir' where il = 'Kirsehir'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Muğla' where il = 'Mugla'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Muş' where il = 'Mus'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Nevşehir' where il = 'Nevsehir'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Niğde' where il = 'Nigde'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Şanlıurfa' where il = 'Sanliurfa'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Şırnak' where il = 'Sirnak'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Tekirdağ' where il = 'Tekirdag'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Uşak' where il = 'Usak'", "select * from final_ist_map"))
final_ist_map <<- sqldf(c("update final_ist_map set il = 'Zonguldak' where il = 'Zinguldak'", "select * from final_ist_map"))
Sys.setlocale( category =  "LC_ALL", locale = "Turkish")
yas <<- read.csv("database/yas.csv", sep = ",")
colnames(yas) <<- c("Yas","Oran")
cinsiyet <<- read.csv("database/cinsiyet.csv", sep = ",")
colnames(cinsiyet) <<- c("Cinsiyet","Oran")
cinsiyet <<- sqldf(c("update cinsiyet set Cinsiyet = 'Kadın' where Cinsiyet = 'KadÄ±n'", "select * from Cinsiyet"))
confirmed <<- read.csv("database/Confirmed.csv", sep = ",")[2:22177,c('Country.Region','Date', 'Value')]
colnames(confirmed) <<- c("Ulke","Tarih","Deger")
deaths <<- read.csv("database/Deaths.csv", sep = ",")[2:22177,c('Country.Region','Date', 'Value')]
colnames(deaths) <<- c("Ulke","Tarih","Deger")
recovered <<- read.csv("database/Recovered.csv", sep = ",")[2:21001,c('Country.Region','Date', 'Value')]
colnames(recovered) <<- c("Ulke","Tarih","Deger")
bulasmis <<- sqldf("SELECT Ulke, Tarih, SUM(Deger) FROM confirmed GROUP BY Ulke,Tarih")
colnames(bulasmis) <<- c("Ulke","Tarih","Deger")
olmus <<- sqldf("SELECT Tarih, SUM(Deger) FROM deaths GROUP BY Ulke,Tarih")
colnames(olmus) <<- c("Tarih","Deger")
kurtulmus <<- sqldf("SELECT Tarih, SUM(Deger) FROM recovered GROUP BY Ulke,Tarih")
colnames(kurtulmus) <<- c("Tarih","Deger")
aktif <<- cbind.data.frame(bulasmis$Tarih, bulasmis$Deger - olmus$Deger - kurtulmus$Deger)
colnames(aktif) <<- c("Tarih", "Deger")
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Afganistan' where Ulke = 'Afghanistan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Arnavutluk' where Ulke = 'Albania'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Cezayir' where Ulke = 'Algeria'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Andora' where Ulke = 'Andorra'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Antigua ve Barbuda' where Ulke = 'Antigua and Barbuda'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Arjantin' where Ulke = 'Argentina'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Ermenistan' where Ulke = 'Armenia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Aruba' where Ulke = 'Aruba'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Avustralya' where Ulke = 'Australia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Avusturya' where Ulke = 'Austria'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Azerbeycan' where Ulke = 'Azerbaijan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Bahreyn' where Ulke = 'Bahrain'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Bangladeş' where Ulke = 'Bangladesh'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Belarus' where Ulke = 'Belarus'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Belçika' where Ulke = 'Belgium'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Butan' where Ulke = 'Bhutan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Bolivya' where Ulke = 'Bolivia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Bosna Hersek' where Ulke = 'Bosnia and Herzegovina'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Brezilya' where Ulke = 'Brazil'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Brunei' where Ulke = 'Brunei'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Bulgaristan' where Ulke = 'Bulgaria'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Burkina Faso' where Ulke = 'Burkina Faso'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kamboçya' where Ulke = 'Cambodia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kamerun' where Ulke = 'Cameroon'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kanada' where Ulke = 'Canada'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Cayman Adaları' where Ulke = 'Cayman Islands'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Şili' where Ulke = 'Chile'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Çin' where Ulke = 'China'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kolombiya' where Ulke = 'Colombia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Demokratik Kongo Cumhuriyeti' where Ulke = 'Congo (Kinshasa)'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kosta Rika' where Ulke = 'Costa Rica'", "select * from bulasmis"))
fildisisql <- paste('update bulasmis set Ulke = "Fildişi Sahili" where Ulke = "',"Cote d'Ivoire",'"', sep = "")
bulasmis <<- sqldf(c(fildisisql, "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Hırvatistan' where Ulke = 'Croatia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Gezi Gemisi' where Ulke = 'Cruise Ship'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Küba' where Ulke = 'Cuba'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Curaçao' where Ulke = 'Curacao'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kıbrıs' where Ulke = 'Cyprus'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Çekya' where Ulke = 'Czechia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Danimarka' where Ulke = 'Denmark'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Dominik Cumhuriyeti' where Ulke = 'Dominican Republic'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Ekvador' where Ulke = 'Ecuador'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Mısır' where Ulke = 'Egypt'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Estonya' where Ulke = 'Estonia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Esvatini̇' where Ulke = 'Eswatini'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Etiyopya' where Ulke = 'Ethiopia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Finlandiya' where Ulke = 'Finland'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Fransa' where Ulke = 'France'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Fransız Guyanası' where Ulke = 'French Guiana'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Gabon' where Ulke = 'Gabon'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Gürcistan' where Ulke = 'Georgia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Almanya' where Ulke = 'Germany'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Gana' where Ulke = 'Ghana'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Yunanistan' where Ulke = 'Greece'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Guadeloupe' where Ulke = 'Guadeloupe'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Guatemala' where Ulke = 'Guatemala'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Guernsey' where Ulke = 'Guernsey'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Gine' where Ulke = 'Guinea'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Guyana' where Ulke = 'Guyana'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Vatikan' where Ulke = 'Holy See'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Honduras' where Ulke = 'Honduras'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Macaristan' where Ulke = 'Hungary'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İzlanda' where Ulke = 'Iceland'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Hindistan' where Ulke = 'India'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Endonezya' where Ulke = 'Indonesia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İran' where Ulke = 'Iran'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Irak' where Ulke = 'Iraq'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İrlanda' where Ulke = 'Ireland'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İsrail' where Ulke = 'Israel'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İtalya' where Ulke = 'Italy'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Jamaika' where Ulke = 'Jamaica'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Japonya' where Ulke = 'Japan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Jersey' where Ulke = 'Jersey'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Ürdün' where Ulke = 'Jordan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kazakistan' where Ulke = 'Kazakhstan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kenya' where Ulke = 'Kenya'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Güney Kore' where Ulke = 'Korea, South'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kuveyt' where Ulke = 'Kuwait'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Letonya' where Ulke = 'Latvia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Lübnan' where Ulke = 'Lebanon'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Lihtenştayn' where Ulke = 'Liechtenstein'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Litvanya' where Ulke = 'Lithuania'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Lüksemburg' where Ulke = 'Luxembourg'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Malezya' where Ulke = 'Malaysia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Maldivler' where Ulke = 'Maldives'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Malta' where Ulke = 'Malta'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Martinik' where Ulke = 'Martinique'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Moritanya' where Ulke = 'Mauritania'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Meksika' where Ulke = 'Mexico'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Moldova' where Ulke = 'Moldova'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Monako' where Ulke = 'Monaco'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Moğolistan' where Ulke = 'Mongolia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Fas' where Ulke = 'Morocco'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Namibya' where Ulke = 'Namibia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Nepal' where Ulke = 'Nepal'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Hollanda' where Ulke = 'Netherlands'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Yeni Zelanda' where Ulke = 'New Zealand'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Nijerya' where Ulke = 'Nigeria'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kuzey Makedonya' where Ulke = 'North Macedonia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Norveç' where Ulke = 'Norway'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Umman' where Ulke = 'Oman'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Pakistan' where Ulke = 'Pakistan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Panama' where Ulke = 'Panama'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Paraguay' where Ulke = 'Paraguay'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Peru' where Ulke = 'Peru'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Filipinler' where Ulke = 'Philippines'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Polonya' where Ulke = 'Poland'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Portekiz' where Ulke = 'Portugal'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Katar' where Ulke = 'Qatar'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Reunion Adası' where Ulke = 'Reunion'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Romanya' where Ulke = 'Romania'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Rusya' where Ulke = 'Russia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Ruanda' where Ulke = 'Rwanda'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Saint Lucia' where Ulke = 'Saint Lucia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Saint Vincent ve Grenadine Adaları' where Ulke = 'Saint Vincent and the Grenadines'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'San Marino' where Ulke = 'San Marino'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Suudi Arabistan' where Ulke = 'Saudi Arabia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Senegal' where Ulke = 'Senegal'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Sırbistan' where Ulke = 'Serbia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Seyşeller Cumhuriyeti' where Ulke = 'Seychelles'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Singapur' where Ulke = 'Singapore'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Slovakya' where Ulke = 'Slovakia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Slovenya' where Ulke = 'Slovenia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Güney Afrika' where Ulke = 'South Africa'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İspanya' where Ulke = 'Spain'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Sri Lanka' where Ulke = 'Sri Lanka'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Sudan' where Ulke = 'Sudan'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Surinam Cumhuriyeti' where Ulke = 'Suriname'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İsveç' where Ulke = 'Sweden'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İsviçre' where Ulke = 'Switzerland'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Tayvan' where Ulke = 'Taiwan*'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Tayland' where Ulke = 'Thailand'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Togo' where Ulke = 'Togo'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Trinidad ve Tobago' where Ulke = 'Trinidad and Tobago'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Tunus' where Ulke = 'Tunisia'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Türkiye' where Ulke = 'Turkey'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Amerika Birleşik Devletleri' where Ulke = 'US'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Ukrayna' where Ulke = 'Ukraine'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Birleşik Arap Emirlikleri' where Ulke = 'United Arab Emirates'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Birleşik Krallık' where Ulke = 'United Kingdom'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Uruguay' where Ulke = 'Uruguay'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Venezuela' where Ulke = 'Venezuela'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Vietnam' where Ulke = 'Vietnam'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'İşgal Altındaki Filistin Toprakları' where Ulke = 'occupied Palestinian territory'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Orta Afrika Cumhuriyeti' where Ulke = 'Central African Republic'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Kongo Cumhuriyeti' where Ulke = 'Congo (Brazzaville)'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Ekvator Ginesi' where Ulke = 'Equatorial Guinea'", "select * from bulasmis"))
bulasmis <<- sqldf(c("update bulasmis set Ulke = 'Vietnam' where Ulke = 'Vietnam'", "select * from bulasmis"))
ulkeler <<- sqldf("SELECT Ulke FROM bulasmis GROUP BY Ulke")
kurtulmus$Ulke <<- bulasmis$Ulke
olmus$Ulke <<- bulasmis$Ulke
aktif$Ulke <<- bulasmis$Ulke
mutate(bulasmis, Tarih= as.Date(Tarih))
mutate(olmus, Tarih= as.Date(Tarih))
mutate(aktif, Tarih= as.Date(Tarih))
mutate(kurtulmus, Tarih= as.Date(Tarih))
olmussqlsorgu <<- paste("SELECT Deger FROM olmus WHERE Tarih = '", max(as.Date(olmus$Tarih)) ,"' GROUP BY Ulke", sep="")
olmussondurum <<- sqldf(olmussqlsorgu)
bulasmissqlsorgu <<- paste("SELECT Deger FROM bulasmis WHERE Tarih = '", max(as.Date(bulasmis$Tarih)), "' GROUP BY Ulke", sep="")
bulasmissondurum <<- sqldf(bulasmissqlsorgu)
kurtulmussqlsorgu <<- paste("SELECT Deger FROM kurtulmus WHERE Tarih = '", max(as.Date(kurtulmus$Tarih)), "' GROUP BY Ulke", sep="")
kurtulmussondurum <<- sqldf(kurtulmussqlsorgu)
aktifsqlsorgu <<- paste("SELECT Deger FROM aktif WHERE Tarih = '", max(as.Date(aktif$Tarih)), "' GROUP BY Ulke", sep="")
aktifsondurum <<- sqldf(aktifsqlsorgu)
sondurumlar <<- cbind.data.frame(ulkeler$Ulke, bulasmissondurum$Deger, olmussondurum$Deger, kurtulmussondurum$Deger, aktifsondurum$Deger)
colnames(sondurumlar) <<- c("Ülke", "Vaka Sayısı", "Vefat Sayısı", "İyileşmiş Hasta Sayısı", "Aktif Hasta Sayısı")
dunyabazindaolmus <<- sqldf("SELECT Tarih, SUM(Deger) FROM deaths GROUP BY Tarih")
colnames(dunyabazindaolmus) <<- c("Tarih", "Deger")
dunyabazindakurtulmus <<- sqldf("SELECT Tarih, SUM(Deger) FROM recovered GROUP BY Tarih")
colnames(dunyabazindakurtulmus) <<- c("Tarih", "Deger")
dunyabazindabulasmis <<- sqldf("SELECT Tarih, SUM(Deger) FROM bulasmis GROUP BY Tarih")
colnames(dunyabazindabulasmis) <<- c("Tarih", "Deger")
dunyabazindaaktif <<- cbind.data.frame(dunyabazindaolmus$Tarih, dunyabazindabulasmis$Deger - dunyabazindaolmus$Deger - dunyabazindakurtulmus$Deger)
colnames(dunyabazindaaktif) <<- c("Tarih", "Deger")
agirhastalik <<- read.csv("database/agirhastalik.csv", sep = ",")
sikayetler <<- read_excel("database/sikayet.xlsx")
hastalikturleri <<- read_excel("database/hastalikturu.xlsx")
turkiye <<- read.csv("database/turkiye.csv", sep = ",")
mutate(turkiye, Tarih= as.Date(Tarih))
bulasmisturkiye <<- sqldf(paste("SELECT Tarih, Deger FROM bulasmis WHERE Ulke ='Türkiye' AND Tarih>='2020-03-11'",sep=""))
kurtulmusturkiye <<- sqldf(paste("SELECT Deger FROM kurtulmus WHERE Ulke ='Türkiye' AND Tarih>='2020-03-11'",sep=""))
olmusturkiye <<- sqldf(paste("SELECT Deger FROM olmus WHERE Ulke ='Türkiye' AND Tarih>='2020-03-11'",sep=""))
aktifturkiye <<- sqldf(paste("SELECT Deger FROM aktif WHERE Ulke ='Türkiye' AND Tarih>='2020-03-11'",sep=""))
turkiyebirlesik <<- cbind.data.frame(bulasmisturkiye$Tarih, bulasmisturkiye$Deger, kurtulmusturkiye$Deger, olmusturkiye$Deger, aktifturkiye$Deger,turkiye$Test, turkiye$YogunBakim,turkiye$Entube)
colnames(turkiyebirlesik) <<- c("Tarih", "Toplam Vaka Sayısı","Toplam İyileşen Hasta Sayısı", "Toplam Vefat Sayısı", "Aktif Hasta Sayısı","Toplam Test Sayısı", "Toplam Yoğun Bakım Hasta Sayısı", "Toplam Entübe Hasta Sayısı")
ui <- dashboardPage( title = "Korona Virüsünün Zamansal Değişimleri",  skin = "blue",
    dashboardHeader(title = img(src="logo.png", width="100%", height="100%")),
    dashboardSidebar( sidebarMenu(
        menuItem("Dünya Genelindeki Değişim", tabName = "dunyabazinda", icon = icon("globe-americas")),
        menuItem("Ülke Genelindeki Değişim", tabName = "ulkebazinda", icon = icon("flag")),
        menuItem("Yaş & Cinsiyet & Hastalık Analizi", tabName = "yascinsiyet", icon = icon("birthday-cake")),
        menuItem("Türkiye İstatistikleri", tabName = "turkiye", icon = icon("flag"))
    )),
    dashboardBody(
       tags$head(tags$style(HTML("
                                #final_text {
                                  text-align: center;
                                }
                                div.box-header {
                                  text-align: center;
                                }
                                "))),
        tabItems(
            # First tab content
            tabItem(tabName = "dunyabazinda",
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("dunyaGeneli", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("dunyaGeneliBuyume", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki Toplam Vaka Sayısı", align = "center", style = "color:white")), htmlOutput("dunyabulasmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki İyileşen Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("dunyaiyilesmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki Vefat Sayısı", align = "center", style = "color:white")), htmlOutput("dunyaolmusSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki Aktif Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("dunyaaktifSayi"))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", h3("Kıyaslamak istediğiniz ülkeler ile özniteliği seçiniz.", align="center")), 
                        box(width = 3, title = "Birinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("birinciUlke", label = NULL, choices = ulkeler$Ulke, selected = "Amerika Birleşik Devletleri", width='100%')),
                        box(width = 3, title = "İkinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("ikinciUlke", label= NULL, choices = ulkeler$Ulke, selected = "İtalya", width='100%') ), 
                        box(width = 3, title = "Üçüncü Ülkeyi Seçiniz.", background = "light-blue", selectInput("ucuncuUlke", label= NULL, choices = c(ulkeler$Ulke, "Yok"), selected = "İspanya", width='100%')),
                        box(width = 3, title = "Özniteliği Seçiniz.", background = "light-blue", selectInput("oznitelik", label= NULL, choices = c("Vaka Sayısı","İyileşen Hasta Sayısı","Vefat Sayısı","Aktif Hasta Sayısı","Vaka Sayısının Büyüme Faktörü"), width='100%')) 
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("kiyaslama", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", h3("Tarihsel Olarak Kıyaslamak istediğiniz ülkeler ile özniteliği seçiniz.", align="center")), 
                        box(width = 3, height = "140px", title = "Birinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("birinciUlke2", label = NULL, choices = ulkeler$Ulke, selected = "Türkiye", width='100%')),
                        box(width = 3, height = "140px", title = "İkinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("ikinciUlke2", label= NULL, choices = ulkeler$Ulke, selected = "İtalya", width='100%') ), 
                        box(width = 3, height = "140px", title = "Üçüncü Ülkeyi Seçiniz.", background = "light-blue", selectInput("ucuncuUlke2", label= NULL, choices = c(ulkeler$Ulke, "Yok"), selected = "İran", width='100%')),
                        box(width = 3, height = "140px", title = "Özniteliği Seçiniz.", background = "light-blue", selectInput("oznitelik2", label= NULL, choices = c("Vaka Sayısı","İyileşen Hasta Sayısı","Vefat Sayısı","Aktif Hasta Sayısı","Vaka Sayısının Büyüme Faktörü"), width='100%')),
                        box(width = 3, height = "140px", title = "Birinci Ülke için Başlangıç Tarihi Seçiniz.", background = "light-blue", dateInput("date1", NULL ,language = "tr", min = "2020-01-22" , value = "2020-03-12")),
                        box(width = 3, height = "140px", title = "İkinci Ülke için Başlangıç Tarihi Seçiniz.", background = "light-blue", dateInput("date2", NULL , language = "tr", min = "2020-01-22" , value = "2020-02-20")),
                        box(width = 3, height = "140px", title = "Üçüncü Ülke için Başlangıç Tarihi Seçiniz.", background = "light-blue", dateInput("date3", NULL , language = "tr",min = "2020-01-22" , value = "2020-02-19")),
                        box(width = 3, height = "140px", title = "Gün Sayısı Seçiniz.", background = "light-blue", selectInput("gunSayisi", NULL, choices = c("1", "5", "10", "20", "30", "40", "50"), selected = "30", width='100%'))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("tarihKiyaslama", height = "400px"))
                    )            ),
            tabItem(tabName = "ulkebazinda",
                    fluidRow(
                        box(width = 4, title = "Ülke Seçiniz.", background = "light-blue", selectInput("Ulke",  label= NULL, choices = ulkeler$Ulke, selected="Çin", width='100%'))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("grafik", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("ulkeGeneliBuyume", height = "400px"))
                    ),  
                    fluidRow(
                        box(width = 3, height = "250px", background = "black", strong(h3("Toplam Vaka Sayısı", align = "center", style = "color:white")), htmlOutput("bulasmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("İyileşen Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("iyilesmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Vefat Sayısı", align = "center", style = "color:white")), htmlOutput("olmusSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Aktif Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("aktifSayi"))
                    ),
                    fluidRow(
                        box(width=12, solidHeader = TRUE, status = "primary", dataTableOutput("tablo"))
                    )
            ),
            tabItem(tabName = "yascinsiyet",
                    fluidRow(
                        box(width = 6, title="Yaşa Göre Görülen Vakalarda Vefat Oranı",solidHeader = TRUE, status = "primary", plotlyOutput("yasAnalizi", height = "400px")),
                        box(width = 6, title="Cinsiyete Göre Görülen Vakalarda Vefat Oranı",solidHeader = TRUE, status = "primary", plotlyOutput("cinsiyetAnalizi", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 6, title ="Sahip Olduğu Ağır Hastalık Sayısına Göre Hayatını Kaybedenlerin Yüzdesel Dağılımı",solidHeader = TRUE, status = "primary", plotlyOutput("agirHastalik", height = "400px")),
                        box(width = 6, title ="Hayatını Kaybedenlerin Başvurduğu Belirtiler ve Oranları", solidHeader = TRUE, status = "primary", plotlyOutput("sikayetler", height = "400px"))
                    ),
                    fluidRow(
                        box(width=6, title ="Hayatını Kaybedenlerde Görülen Hastalıklar ve Oranları", solidHeader = TRUE, status = "primary", dataTableOutput("hastalikTurleri"))
                    )
            ),
            tabItem(tabName = "turkiye",
                    fluidRow(
                      box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("turkiyeGrafik", height = "400px"))
                    ),
                    fluidRow(
                      box(width=12, solidHeader = TRUE, status = "primary", dataTableOutput("turkiyetablo"))
                    ),
                    fluidRow(
                      box(width=12, solidHeader = TRUE, status = "primary", plotlyOutput("harita"))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de Türkiyedeki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="tryogunluk1.jpg" style="width: 80%; align:center;"> <img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de İstanbuldaki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="istanbul.jpg" style="width: 80%; align:center;"><img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de Ankaradaki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="ankara.jpg" style="width: 80%; align:center;"><img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de İzmirdeki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="izmir.jpg" style="width: 80%; align:center;"><img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de Kocaelideki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="kocaeli.jpg" style="width: 80%; align:center;"><img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de Sakaryadaki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="sakarya.jpg" style="width: 80%; align:center;"><img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    ),
                    fluidRow(
                      box(width = 12, title="7 Nisan 2020'de Zonguldaktaki Korona Vakalarının Dağılımı", solidHeader = TRUE, status = "primary", HTML('<center><img src="zonguldak.jpg" style="width: 80%; align:center;"><img src="lejant.png" style="width: 10%; align:center;"></center>'))
                    )
            )
        )
    )
)