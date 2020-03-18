library(shiny)
library(readxl)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(sqldf)
library(lubridate)
Sys.setlocale( category =  "LC_ALL", locale = "Turkish")
yas <<- read.csv("database/yas.csv", sep = ",")
colnames(yas) <<- c("Yas","Oran")
cinsiyet <<- read.csv("database/cinsiyet.csv", sep = ",")
colnames(cinsiyet) <<- c("Cinsiyet","Oran")
cinsiyet <<- sqldf(c("update cinsiyet set Cinsiyet = 'Kadın' where Cinsiyet = 'KadÄ±n'", "select * from Cinsiyet"))
confirmed <<- read.csv("database/Confirmed.csv", sep = ",")[2:25761,c('Country.Region','Date', 'Value')]
colnames(confirmed) <<- c("Ulke","Tarih","Deger")
confirmed <<- sqldf(c("update confirmed set Ulke = 'Afganistan' where Ulke = 'Afghanistan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Arnavutluk' where Ulke = 'Albania'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Cezayir' where Ulke = 'Algeria'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Andora' where Ulke = 'Andorra'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Antigua ve Barbuda' where Ulke = 'Antigua and Barbuda'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Arjantin' where Ulke = 'Argentina'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Ermenistan' where Ulke = 'Armenia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Aruba' where Ulke = 'Aruba'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Avustralya' where Ulke = 'Australia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Avusturya' where Ulke = 'Austria'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Azerbeycan' where Ulke = 'Azerbaijan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Bahreyn' where Ulke = 'Bahrain'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Bangladeş' where Ulke = 'Bangladesh'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Belarus' where Ulke = 'Belarus'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Belçika' where Ulke = 'Belgium'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Butan' where Ulke = 'Bhutan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Bolivya' where Ulke = 'Bolivia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Bosna Hersek' where Ulke = 'Bosnia and Herzegovina'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Brezilya' where Ulke = 'Brazil'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Brunei' where Ulke = 'Brunei'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Bulgaristan' where Ulke = 'Bulgaria'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Burkina Faso' where Ulke = 'Burkina Faso'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kamboçya' where Ulke = 'Cambodia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kamerun' where Ulke = 'Cameroon'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kanada' where Ulke = 'Canada'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Cayman Adaları' where Ulke = 'Cayman Islands'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Şili' where Ulke = 'Chile'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Çin' where Ulke = 'China'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kolombiya' where Ulke = 'Colombia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Demokratik Kongo Cumhuriyeti' where Ulke = 'Congo (Kinshasa)'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kosta Rika' where Ulke = 'Costa Rica'", "select * from confirmed"))
fildisisql <- paste('update confirmed set Ulke = "Fildişi Sahili" where Ulke = "',"Cote d'Ivoire",'"', sep = "")
confirmed <<- sqldf(c(fildisisql, "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Hırvatistan' where Ulke = 'Croatia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Gezi Gemisi' where Ulke = 'Cruise Ship'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Küba' where Ulke = 'Cuba'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Curaçao' where Ulke = 'Curacao'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kıbrıs' where Ulke = 'Cyprus'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Çekya' where Ulke = 'Czechia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Danimarka' where Ulke = 'Denmark'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Dominik Cumhuriyeti' where Ulke = 'Dominican Republic'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Ekvador' where Ulke = 'Ecuador'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Mısır' where Ulke = 'Egypt'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Estonya' where Ulke = 'Estonia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Esvatini̇' where Ulke = 'Eswatini'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Etiyopya' where Ulke = 'Ethiopia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Finlandiya' where Ulke = 'Finland'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Fransa' where Ulke = 'France'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Fransız Guyanası' where Ulke = 'French Guiana'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Gabon' where Ulke = 'Gabon'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Gürcistan' where Ulke = 'Georgia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Almanya' where Ulke = 'Germany'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Gana' where Ulke = 'Ghana'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Yunanistan' where Ulke = 'Greece'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Guadeloupe' where Ulke = 'Guadeloupe'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Guatemala' where Ulke = 'Guatemala'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Guernsey' where Ulke = 'Guernsey'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Gine' where Ulke = 'Guinea'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Guyana' where Ulke = 'Guyana'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Vatikan' where Ulke = 'Holy See'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Honduras' where Ulke = 'Honduras'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Macaristan' where Ulke = 'Hungary'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İzlanda' where Ulke = 'Iceland'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Hindistan' where Ulke = 'India'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Endonezya' where Ulke = 'Indonesia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İran' where Ulke = 'Iran'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Irak' where Ulke = 'Iraq'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İrlanda' where Ulke = 'Ireland'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İsrail' where Ulke = 'Israel'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İtalya' where Ulke = 'Italy'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Jamaika' where Ulke = 'Jamaica'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Japonya' where Ulke = 'Japan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Jersey' where Ulke = 'Jersey'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Ürdün' where Ulke = 'Jordan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kazakistan' where Ulke = 'Kazakhstan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kenya' where Ulke = 'Kenya'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Güney Kore' where Ulke = 'Korea, South'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kuveyt' where Ulke = 'Kuwait'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Letonya' where Ulke = 'Latvia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Lübnan' where Ulke = 'Lebanon'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Lihtenştayn' where Ulke = 'Liechtenstein'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Litvanya' where Ulke = 'Lithuania'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Lüksemburg' where Ulke = 'Luxembourg'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Malezya' where Ulke = 'Malaysia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Maldivler' where Ulke = 'Maldives'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Malta' where Ulke = 'Malta'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Martinik' where Ulke = 'Martinique'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Moritanya' where Ulke = 'Mauritania'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Meksika' where Ulke = 'Mexico'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Moldova' where Ulke = 'Moldova'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Monako' where Ulke = 'Monaco'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Moğolistan' where Ulke = 'Mongolia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Fas' where Ulke = 'Morocco'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Namibya' where Ulke = 'Namibia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Nepal' where Ulke = 'Nepal'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Hollanda' where Ulke = 'Netherlands'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Yeni Zelanda' where Ulke = 'New Zealand'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Nijerya' where Ulke = 'Nigeria'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kuzey Makedonya' where Ulke = 'North Macedonia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Norveç' where Ulke = 'Norway'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Umman' where Ulke = 'Oman'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Pakistan' where Ulke = 'Pakistan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Panama' where Ulke = 'Panama'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Paraguay' where Ulke = 'Paraguay'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Peru' where Ulke = 'Peru'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Filipinler' where Ulke = 'Philippines'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Polonya' where Ulke = 'Poland'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Portekiz' where Ulke = 'Portugal'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Katar' where Ulke = 'Qatar'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Reunion Adası' where Ulke = 'Reunion'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Romanya' where Ulke = 'Romania'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Rusya' where Ulke = 'Russia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Ruanda' where Ulke = 'Rwanda'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Saint Lucia' where Ulke = 'Saint Lucia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Saint Vincent ve Grenadine Adaları' where Ulke = 'Saint Vincent and the Grenadines'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'San Marino' where Ulke = 'San Marino'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Suudi Arabistan' where Ulke = 'Saudi Arabia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Senegal' where Ulke = 'Senegal'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Sırbistan' where Ulke = 'Serbia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Seyşeller Cumhuriyeti' where Ulke = 'Seychelles'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Singapur' where Ulke = 'Singapore'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Slovakya' where Ulke = 'Slovakia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Slovenya' where Ulke = 'Slovenia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Güney Afrika' where Ulke = 'South Africa'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İspanya' where Ulke = 'Spain'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Sri Lanka' where Ulke = 'Sri Lanka'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Sudan' where Ulke = 'Sudan'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Surinam Cumhuriyeti' where Ulke = 'Suriname'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İsveç' where Ulke = 'Sweden'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İsviçre' where Ulke = 'Switzerland'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Tayvan' where Ulke = 'Taiwan*'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Tayland' where Ulke = 'Thailand'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Togo' where Ulke = 'Togo'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Trinidad ve Tobago' where Ulke = 'Trinidad and Tobago'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Tunus' where Ulke = 'Tunisia'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Türkiye' where Ulke = 'Turkey'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Amerika Birleşik Devletleri' where Ulke = 'US'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Ukrayna' where Ulke = 'Ukraine'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Birleşik Arap Emirlikleri' where Ulke = 'United Arab Emirates'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Birleşik Krallık' where Ulke = 'United Kingdom'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Uruguay' where Ulke = 'Uruguay'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Venezuela' where Ulke = 'Venezuela'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Vietnam' where Ulke = 'Vietnam'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'İşgal Altındaki Filistin Toprakları' where Ulke = 'occupied Palestinian territory'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Orta Afrika Cumhuriyeti' where Ulke = 'Central African Republic'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Kongo Cumhuriyeti' where Ulke = 'Congo (Brazzaville)'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Ekvator Ginesi' where Ulke = 'Equatorial Guinea'", "select * from confirmed"))
confirmed <<- sqldf(c("update confirmed set Ulke = 'Vietnam' where Ulke = 'Vietnam'", "select * from confirmed"))
deaths <<- read.csv("database/Deaths.csv", sep = ",")[2:25761,c('Date', 'Value')]
colnames(deaths) <<- c("Tarih","Deger")
deaths['Ulke'] <<- confirmed$Ulke
recovered <<- read.csv("database/Recovered.csv", sep = ",")[2:25761,c('Date', 'Value')]
colnames(recovered) <<- c("Tarih","Deger")
recovered['Ulke'] <<- confirmed$Ulke
bulasmis <<- sqldf("SELECT Ulke, Tarih, SUM(Deger) FROM confirmed GROUP BY Ulke,Tarih")
colnames(bulasmis) <<- c("Ulke","Tarih","Deger")
olmus <<- sqldf("SELECT Ulke, Tarih, SUM(Deger) FROM deaths GROUP BY Ulke,Tarih")
colnames(olmus) <<- c("Ulke","Tarih","Deger")
kurtulmus <<- sqldf("SELECT Ulke, Tarih, SUM(Deger) FROM recovered GROUP BY Ulke,Tarih")
colnames(kurtulmus) <<- c("Ulke","Tarih","Deger")
aktif <<- cbind.data.frame(bulasmis$Ulke, bulasmis$Tarih, bulasmis$Deger - olmus$Deger - kurtulmus$Deger)
colnames(aktif) <<- c("Ulke", "Tarih", "Deger")
ulkeler <<- sqldf("SELECT Ulke FROM confirmed GROUP BY Ulke")
mutate(bulasmis, Tarih= as.Date(Tarih))
mutate(olmus, Tarih= as.Date(Tarih))
mutate(aktif, Tarih= as.Date(Tarih))
mutate(kurtulmus, Tarih= as.Date(Tarih))
olmussqlsorgu <<- paste("SELECT Deger FROM olmus WHERE Tarih = '", as.Date("2020-03-17") ,"' GROUP BY Ulke", sep="")
olmussondurum <<- sqldf(olmussqlsorgu)
bulasmissqlsorgu <<- paste("SELECT Deger FROM bulasmis WHERE Tarih = '", as.Date("2020-03-17"), "' GROUP BY Ulke", sep="")
bulasmissondurum <<- sqldf(bulasmissqlsorgu)
kurtulmussqlsorgu <<- paste("SELECT Deger FROM kurtulmus WHERE Tarih = '", as.Date("2020-03-17"), "' GROUP BY Ulke", sep="")
kurtulmussondurum <<- sqldf(kurtulmussqlsorgu)
aktifsqlsorgu <<- paste("SELECT Deger FROM aktif WHERE Tarih = '", as.Date("2020-03-17"), "' GROUP BY Ulke", sep="")
aktifsondurum <<- sqldf(aktifsqlsorgu)
sondurumlar <<- cbind.data.frame(ulkeler$Ulke, bulasmissondurum$Deger, olmussondurum$Deger, kurtulmussondurum$Deger, aktifsondurum$Deger)
colnames(sondurumlar) <<- c("Ülke", "Vaka Sayısı", "Ölü Sayısı", "İyileşmiş Hasta Sayısı", "Aktif Hasta Sayısı")
dunyabazindaolmus <<- sqldf("SELECT Tarih, SUM(Deger) FROM deaths GROUP BY Tarih")
colnames(dunyabazindaolmus) <<- c("Tarih", "Deger")
dunyabazindakurtulmus <<- sqldf("SELECT Tarih, SUM(Deger) FROM recovered GROUP BY Tarih")
colnames(dunyabazindakurtulmus) <<- c("Tarih", "Deger")
dunyabazindabulasmis <<- sqldf("SELECT Tarih, SUM(Deger) FROM confirmed GROUP BY Tarih")
colnames(dunyabazindabulasmis) <<- c("Tarih", "Deger")
dunyabazindaaktif <<- cbind.data.frame(dunyabazindaolmus$Tarih, dunyabazindabulasmis$Deger - dunyabazindaolmus$Deger - dunyabazindakurtulmus$Deger)
colnames(dunyabazindaaktif) <<- c("Tarih", "Deger")
agirhastalik <<- read.csv("database/agirhastalik.csv", sep = ",")
sikayetler <<- read_excel("database/sikayet.xlsx")
hastalikturleri <<- read_excel("database/hastalikturu.xlsx") 
ui <- dashboardPage( title = "Korona Virüsünün Zamansal Değişimleri",  skin = "blue",
    dashboardHeader(title = img(src="logo.png", width="100%", height="100%")),
    dashboardSidebar( sidebarMenu(
        menuItem("Dünya Genelindeki Değişim", tabName = "dunyabazinda", icon = icon("globe-americas")),
        menuItem("Ülke Genelindeki Değişim", tabName = "ulkebazinda", icon = icon("flag")),
        menuItem("Yaş & Cinsiyet & Hatalık Analizi", tabName = "yascinsiyet", icon = icon("birthday-cake"))
    )),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dunyabazinda",
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("dunyaGeneli", height = "450px"))
                    ),
                    fluidRow(
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki Toplam Vaka Sayısı", align = "center", style = "color:white")), htmlOutput("dunyabulasmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki İyileşen Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("dunyaiyilesmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki Ölü Sayısı", align = "center", style = "color:white")), htmlOutput("dunyaolmusSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Dünya Genelindeki Aktif Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("dunyaaktifSayi"))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", h3("Kıyaslamak istediğiniz ülkeler ile özniteliği seçiniz.", align="center")), 
                        box(width = 3, title = "Birinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("birinciUlke", label = NULL, choices = ulkeler$Ulke, selected = "Çin", width='100%')),
                        box(width = 3, title = "İkinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("ikinciUlke", label= NULL, choices = ulkeler$Ulke, selected = "İtalya", width='100%') ), 
                        box(width = 3, title = "Üçüncü Ülkeyi Seçiniz.", background = "light-blue", selectInput("ucuncuUlke", label= NULL, choices = ulkeler$Ulke, selected = "İran", width='100%')),
                        box(width = 3, title = "Özniteliği Seçiniz.", background = "light-blue", selectInput("oznitelik", label= NULL, choices = c("Vaka Sayısı","İyileşen Hasta Sayısı","Ölü Sayısı","Aktif Hasta Sayısı"), width='100%')) 
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("kiyaslama", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", h3("Tarihsel Olarak Kıyaslamak istediğiniz ülkeler ile özniteliği seçiniz.", align="center")), 
                        box(width = 3, height = "140px", title = "Birinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("birinciUlke2", label = NULL, choices = ulkeler$Ulke, selected = "Türkiye", width='100%')),
                        box(width = 3, height = "140px", title = "İkinci Ülkeyi Seçiniz.", background = "light-blue", selectInput("ikinciUlke2", label= NULL, choices = ulkeler$Ulke, selected = "İtalya", width='100%') ), 
                        box(width = 3, height = "140px", title = "Üçüncü Ülkeyi Seçiniz.", background = "light-blue", selectInput("ucuncuUlke2", label= NULL, choices = ulkeler$Ulke, selected = "İran", width='100%')),
                        box(width = 3, height = "140px", title = "Özniteliği Seçiniz.", background = "light-blue", selectInput("oznitelik2", label= NULL, choices = c("Vaka Sayısı","İyileşen Hasta Sayısı","Ölü Sayısı","Aktif Hasta Sayısı"), width='100%')),
                        box(width = 3, height = "140px", title = "Birinci Ülke için Başlangıç Tarihi Seçiniz.", background = "light-blue", dateInput("date1", NULL ,language = "tr", min = "2020-01-22" , value = "2020-03-12")),
                        box(width = 3, height = "140px", title = "İkinci Ülke için Başlangıç Tarihi Seçiniz.", background = "light-blue", dateInput("date2", NULL , language = "tr", min = "2020-01-22" , value = "2020-01-30")),
                        box(width = 3, height = "140px", title = "Üçüncü Ülke için Başlangıç Tarihi Seçiniz.", background = "light-blue", dateInput("date3", NULL , language = "tr",min = "2020-01-22" , value = "2020-02-19")),
                        box(width = 3, height = "140px", title = "Gün Sayısı Seçiniz.", background = "light-blue", selectInput("gunSayisi", NULL, choices = c("1", "5", "10", "20"), selected = "10", width='100%'))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("tarihKiyaslama", height = "400px"))
                    )            ),
            tabItem(tabName = "ulkebazinda",
                    fluidRow(
                        box(width = 4, title = "Ülke Seçiniz.", background = "light-blue", selectInput("Ulke",  label= NULL, choices = ulkeler$Ulke, width='100%'))
                    ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, status = "primary", plotlyOutput("grafik", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 3, height = "250px", background = "black", strong(h3("Toplam Vaka Sayısı", align = "center", style = "color:white")), htmlOutput("bulasmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("İyileşen Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("iyilesmisSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Ölü Sayısı", align = "center", style = "color:white")), htmlOutput("olmusSayi")),
                        box(width = 3, height = "250px", background = "black", strong(h3("Aktif Hasta Sayısı", align = "center", style = "color:white")), htmlOutput("aktifSayi"))
                    ),
                    fluidRow(
                        box(width=12, solidHeader = TRUE, status = "primary", dataTableOutput("tablo"))
                    )
            ),
            tabItem(tabName = "yascinsiyet",
                    fluidRow(
                        box(width = 6, title="Yaşa Göre Görülen Vakalarda Ölüm Oranı",solidHeader = TRUE, status = "primary", plotlyOutput("yasAnalizi", height = "400px")),
                        box(width = 6, title="Cinsiyete Göre Görülen Vakalarda Ölüm Oranı",solidHeader = TRUE, status = "primary", plotlyOutput("cinsiyetAnalizi", height = "400px"))
                    ),
                    fluidRow(
                        box(width = 6, title ="Sahip Olduğu Ağır Hastalık Sayısına Göre Hayatını Kaybedenlerin Yüzdesel Dağılımı",solidHeader = TRUE, status = "primary", plotlyOutput("agirHastalik", height = "400px")),
                        box(width = 6, title ="Hayatını Kaybedenlerin Başvurduğu Belirtiler ve Oranları", solidHeader = TRUE, status = "primary", plotlyOutput("sikayetler", height = "400px"))
                    ),
                    fluidRow(
                        box(width=6, title ="Hayatını Kaybedenlerde Görülen Hastalıklar ve Oranları", solidHeader = TRUE, status = "primary", dataTableOutput("hastalikTurleri"))
                    )
            )
        )
                   
        
    )
)