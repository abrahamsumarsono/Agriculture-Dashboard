####library####
library(shiny)
library(shinydashboard)
library(summarytools)
library(readxl)
library(DT)
library(ggplot2)
library(plotly)
library(rhandsontable)
library(datasets)
library(gganimate)
library(ggthemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(plyr)
library(dplyr)
library(ggExtra)

####data####
setwd("C:/Users/Abraham/Documents/abraham/final project sim")
datapadi <- read.csv("finpro.csv")
datapadi

databiofarmaka <- read.csv("biofarmaka.csv")
databiofarmaka
summary(databiofarmaka)

databiofix <- read.csv("biofarmakafix.csv", sep = ";")
databiofix
View(databiofix)

databuah <- read.csv("buah.csv")
databuah

datalahan <- read.csv("lahan ladang.csv")
datalahan

datasayuran <- read.csv("sayuran.csv")
datasayuran

buahfix <- read.csv("buahfix.csv", sep = ";")
buahfix
tail(buahfix)

#surabaya
buahsby = buahfix[buahfix$Kota.Kabupaten=="Surabaya",]
buahsby
buahsby21 = buahsby[buahsby$Tahun==2021,]
buahsby21

#pacitan
pacitan = buahfix[buahfix$Kota.Kabupaten=="Pacitan",]
datapac = pacitan[pacitan$Tahun==2021,]
datapac

#ponorogo
pnrg = buahfix[buahfix$Kota.Kabupaten=="Ponorogo",]
datapnrg = pnrg[pnrg$Tahun==2021,]
datapnrg

#trenggalek
trng = buahfix[buahfix$Kota.Kabupaten=="Trenggalek",]
datatrng = trng[trng$Tahun==2021,]
datatrng

#tulungagung
agung = buahfix[buahfix$Kota.Kabupaten=="Trenggalek",]
dataagung = agung[agung$Tahun==2021,]

#blitar
blitar = buahfix[buahfix$Kota.Kabupaten=="Blitar",]
databl = blitar[blitar$Tahun==2021,]

#kediri
kediri = buahfix[buahfix$Kota.Kabupaten=="Kediri",]
datakd = kediri[kediri$Tahun==2021,]

#malang
malang = buahfix[buahfix$Kota.Kabupaten=="Malang",]
datamlg = malang[malang$Tahun==2021,]

#lumajang
lumajang = buahfix[buahfix$Kota.Kabupaten=="Lumajang",]
datalum = lumajang[lumajang$Tahun==2021,]

#jember
jember = buahfix[buahfix$Kota.Kabupaten=="Jember",]
datajem = jember[jember$Tahun==2021,]

#banyuwangi
banyu = buahfix[buahfix$Kota.Kabupaten=="Banyuwangi",]
databan = banyu[banyu$Tahun==2021,]

#bondowoso
bondo = buahfix[buahfix$Kota.Kabupaten=="Bondowoso",]
databon = bondo[bondo$Tahun==2021,]

#situbondo
situ = buahfix[buahfix$Kota.Kabupaten=="Situbondo",]
datasitu = situ[situ$Tahun==2021,]

#probolinggo
probo = buahfix[buahfix$Kota.Kabupaten=="Probolinggo",]
datapr = probo[probo$Tahun==2021,]

#pasuruan
pasu = buahfix[buahfix$Kota.Kabupaten=="Pasuruan",]
datapas = pasu[pasu$Tahun==2021,]

#sidoarjo
sido = buahfix[buahfix$Kota.Kabupaten=="Sidoarjo",]
datasid = sido[sido$Tahun==2021,]

#mojokerto
mojo = buahfix[buahfix$Kota.Kabupaten=="Mojokerto",]
datamj = mojo[mojo$Tahun==2021,]

#jombang
jomb = buahfix[buahfix$Kota.Kabupaten=="Jombang",]
datajom = jomb[jomb$Tahun==2021,]

#nganjuk
nganj = buahfix[buahfix$Kota.Kabupaten=="Nganjuk",]
datangan = nganj[nganj$Tahun==2021,]

#madiun
mad = buahfix[buahfix$Kota.Kabupaten=="Madiun",]
datamad = mad[mad$Tahun==2021,]

#magetan
mag = buahfix[buahfix$Kota.Kabupaten=="Magetan",]
datamag = mag[mag$Tahun==2021,]

#ngawi
ngawi = buahfix[buahfix$Kota.Kabupaten=="Ngawi",]
datangawi = ngawi[ngawi$Tahun==2021,]

#bojonegoro
bojon = buahfix[buahfix$Kota.Kabupaten=="Bojonegoro",]
databjn = bojon[bojon$Tahun==2021,]

#tuban
tuban = buahfix[buahfix$Kota.Kabupaten=="Tuban",]
datatbn = tuban[tuban$Tahun==2021,]

#lamongan
lam = buahfix[buahfix$Kota.Kabupaten=="Lamongan",]
datalam = lam[lam$Tahun==2021,]

#gresik
gres = buahfix[buahfix$Kota.Kabupaten=="Gresik",]
datagres = gres[gres$Tahun==2021,]

#bangkalan
bangk = buahfix[buahfix$Kota.Kabupaten=="Bangkalan",]
databang = bangk[bangk$Tahun==2021,]

#sampang
sampang = buahfix[buahfix$Kota.Kabupaten=="Sampang",]
datasam = sampang[sampang$Tahun==2021,]

#pamekasan
pamek = buahfix[buahfix$Kota.Kabupaten=="Pamekasan",]
datapam = pamek[pamek$Tahun==2021,]

#sumenep
sumenep = buahfix[buahfix$Kota.Kabupaten=="Sumenep",]
datasum = sumenep[sumenep$Tahun==2021,]

#batu
batu = buahfix[buahfix$Kota.Kabupaten=="Batu",]
databatu = batu[batu$Tahun==2021,]

####descriptive statistics for crops####

#data padi 2018
padi2018 = datapadi[datapadi$Year==2018,]
padi2018
median(padi2018$Produktivitas)
panenpadi2018 = mean(padi2018$Luas_Panen)
panenpadi2018
produktivitaspadi2018 = mean(padi2018$Produktivitas)
produktivitaspadi2018
produksipadi2018 = mean(padi2018$Produksi)
produksipadi2018

#data padi 2019
padi2019 = datapadi[datapadi$Year==2019,]
padi2019
panenpadi2019 = mean(padi2019$Luas_Panen)
panenpadi2019
produktivitaspadi2019 = mean(padi2019$Produktivitas)
produktivitaspadi2019
produksipadi2019 = mean(padi2019$Produksi)
produksipadi2019

#data padi 2020
padi2020 = datapadi[datapadi$Year==2020,]
padi2020
panenpadi2020 = mean(padi2020$Luas_Panen)
panenpadi2020
produktivitaspadi2020 = mean(padi2020$Produktivitas)
produktivitaspadi2020
produksipadi2020 = mean(padi2020$Produksi)
produksipadi2020
summary(padi2020$Produksi)
summary(padi2021$Produksi)
summary(padi2020$Luas_Panen)
summary(padi2021$Luas_Panen)


#data padi 2021
padi2021 = datapadi[datapadi$Year==2021,]
padi2021
panenpadi2021 = mean(padi2021$Luas_Panen)
panenpadi2021
produktivitaspadi2021 = mean(padi2021$Produktivitas)
produktivitaspadi2021
produksipadi2021 = mean(padi2021$Produksi)
produksipadi2021

#padi based on year
summary(padi2018)
min(padi2018$Luas_Panen)

####descriptive statistics for biopharmaceutical plants####
#biofarmaka
bio2021 = databiofarmaka[databiofarmaka$Tahun==2021,]
summary(bio2021)
bio2020 = databiofarmaka[databiofarmaka$Tahun == 2020,]
summary(bio2020)
bio2019 = databiofarmaka[databiofarmaka$Tahun == 2019,]
summary(bio2019)
bio2018 = databiofarmaka[databiofarmaka$Tahun == 2018,]
summary(bio2019)

####descriptive statistics for plantations####
buah2020 = databuah[databuah$Tahun==2020,]
summary(buah2020)
buah2021 = databuah[databuah$Tahun==2021,]
summary(buah2021)
buah2019 = databuah[databuah$Tahun==2019,]
summary(buah2019)
buah2018 = databuah[databuah$Tahun==2018,]
summary(buah2018)
buah2018

####ui####
headerItem<-dashboardHeader(title="dashboard final task")

sidebarItem<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home", icon = icon("asterisk") ,tabName = "home"),
    menuItem("About", icon = icon("book"), tabName = "about"),
    menuItem("Descriptive Statistic", icon = icon("chart-line"), tabName = "statdes"),
    menuItem("Visualization", icon = icon("chart-pie"), tabName = "vis",
             menuSubItem("Tanaman Pangan",tabName = "pangan"),
             menuSubItem("Tanaman Biofarmaka",tabName = "biofarmaka"),
             menuSubItem("Perkebunan",tabName = "kebun")
    ),
    menuItem("Grow Your Business", icon = icon("search"), tabName = "grow"),
    menuItem("Author", icon = icon("user"), tabName = "Penyusun")))

bodies=dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("Welcome!"),
            infoBox(title = "Number of Datas", p("3"),
                    icon = icon("file"), color = "purple", width = 4,
                    fill = TRUE
            ),
            infoBox(title = "Number of Topics", p("3"),
                    icon = icon("file"), color = "aqua", width = 4,
                    fill = TRUE
            ),
            infoBox(title = "Number of Plants", p("10"),
                    icon = icon("file"), color = "olive", width = 4,
                    fill = TRUE
            ),
            column(width = 12,
                   tabsetPanel(
                     tabPanel("Description",
                              box(
                                title = strong("Brief Description"), width = NULL, solidHeader = TRUE, style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                                style="text-align: center;",style = "height:180px;",
                                "This final project dashboard is made for Informations System Management course in the even semester of 2021 - 2022. 
                                Hopefully, this dashboard can help people working in the related field - or people in general - understand 
                                both the enhancement and degradation of our data. Bringing the topic of agriculture, we hope that this dashboard can be used as 
                                a media to further enhance our agricultural sector. "
                              )),
                     tabPanel("Our Data",
                              box(
                                title = strong("Our Data"), width = NULL, solidHeader = TRUE, style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                                style="text-align: center;",style = "height:180px;",
                                "Bringing the topic of agriculture, we gathered data from East Java Province In Number from 2018 to 2021. 
                                Not only that, we expanded our data from crops plants, horticultural and biopharmaceutical plants, and plantations fruit. 
                                Our data shows a wide variety of range in production, harvested area, worker productivity, and more. 
                                With vast data to offer, we choose 7 variables that is, year, cities, harvested area, worker productivity, productions, 
                                type of plants, and type of fruit. With these variables of our datas, hopefully people can understand more about the importance of our agricultural sector."
                              )),
                     tabPanel("The Visualization",
                              box(
                                title = strong("Data Visualization and Why We Choose Them"), width = NULL, solidHeader = TRUE, style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                                style="text-align: center;",style = "height:180px;",
                                "With a vast variety of our datas, we choose some visualization that might help people to understand the data better. These data visuals are aimed 
                                to be a media to show our progress on the agricultural sector and to make a decision, hopefully to enhance our sector. We have visualized both 
                                numerical and categorical datas such as worker's productivity, production, year, et cetera. With this, we use some visuals such as line chart, 
                                bar chart, histogram, and many more. With these visuals, we believe that anyone can help East Java to enhance its sectors, especially the agricultural sector."))),
                   tabName = "var",div(h1("Variables"),style="text-align: center;"),
                   "For our data and visualization, we use some variables such as year, cities, kind of plants, and more!", br(),
                   actionBttn(
                     inputId = "var1",
                     label = "Year",
                     color = "primary",
                     style = "jelly",
                     block = TRUE
                   ),br(),
                   actionBttn(
                     inputId = "var2",
                     label = "Cities",
                     color = "warning",
                     style = "jelly",
                     block = TRUE
                   ),br(),
                   actionBttn(
                     inputId = "var3",
                     label = "Harvested Area",
                     color = "danger",
                     style = "jelly",
                     block = TRUE
                   ),br(),
                   actionBttn(
                     inputId = "var4",
                     label = "Productivity",
                     color = "success",
                     style = "jelly",
                     block = TRUE
                   ),br(),
                   actionBttn(
                     inputId = "var5",
                     label = "Production",
                     color = "royal",
                     style = "jelly",
                     block = TRUE
                   ),br(),
                   actionBttn(
                     inputId = "var6",
                     label = "Type of Plant",
                     color = "warning",
                     style = "jelly",
                     block = TRUE
                   ),br(),
                   actionBttn(
                     inputId = "var7",
                     label = "Type of Fruit",
                     color = "primary",
                     style = "jelly",
                     block = TRUE
                   ),br(),)),
    tabItem(tabName = "about",
            h2("Explore More About Us!"),
            box(title = "Latar Belakang", solidHeader = TRUE, collapsible = TRUE,
                p("Pertanian, kehutanan, peternakan, dan perikanan merupakan aspek atau bidang yang penting bagi suatu negara 
                terutama Indonesia yang merupakan negara agraris. Berdasarkan hasil Survei Angkatan Kerja Nasional (Sakernas) Februari 2022, 
                lapangan pekerjaan di Jawa Timur yang menyerap tenaga kerja paling banyak adalah Pertanian, Kehutanan, dan Perikanan yaitu sebesar 34,38 persen.  
                Dengan angka yang besar ini, tentu pemerintah mengawasi dan mencatat berbagai data di bidang ini, terutama BPS (Badan Pusat Statistik) Jawa Timur"),
                p("Mengingat pentingnya aspek ini terhadap Provinsi Jawa Timur bahkan negara, tentunya diperlukan pengelolaan dan pembukuan catatan kenaikan 
                dan/atau penurunan data setiap periode tertentu. Hal ini sendiri dilakukan oleh salah satunya BPS (Badan Pusat Statistik) Jawa Timur dengan menerbitkan buku berjudul
                “PROVINSI JAWA TIMUR DALAM ANGKA 2022” dan edisi lainnya tiap tahun yang dapat diakses oleh semua masyarakat. Ini semua dilakukan agar kondisi terbaru bidang pertanian, 
                kehutanan, peternakan, dan perikanan dapat diamati dengan baik.
"),
                p("Namun, tentunya hanya sedikit lapisan masyarakat yang bahkan mengenal dan paham akan isi buku ini. Oleh karena itu, penulis ingin membuat suatu dashboard yang berjudul 
                  “Dashboard Pertanian di Jawa Timur” menggunakan bantuan R Shiny dengan tujuan mengedukasi, meneliti, dan memantau data pada bidang pertanian, kehutanan, peternakan, 
                  dan perikanan di Jawa Timur. Dengan pembuatan dasbor ini diharapkan masyarakat yang ingin mengamati kondisi terbaru dapat lebih mengenal kondisi bidang ini di Jawa Timur.")
),
            box(title = "Tujuan", solidHeader = TRUE, collapsible = TRUE,
                p("1. Merancang dashboard dengan tujuan untuk mengetahui kinerja para petani serta field-related worker 
                  selama proses pemroduksian hasil pertanian dan perkebunan, mulai dari tanaman pangan hingga buah-buahan"),
                p("2. Memonitor kondisi dan mengetahui tingkat produksi dan produktivitas petani selama empat tahun"),
                p("3. Memantau perkembangan hasil produksi guna membantu pemerintah agar dapat memberi bantuan fasilitas 
                  sarana dan prasarana bagi wilayah yang membutuhkan"),
                p("4. Mengedukasi segala elemen masyarakat agar mengenal data terbaru di bidang pertanian, kehutanan, peternakan, dan perikanan di Jawa Timur
")
            ),
            box(title = "Manfaat", solidHeader = TRUE, collapsible = TRUE,
                p("1. Sebagai dasar pengambilan keputusan bagi Dinas Pertanian dan Ketahanan Pangan Jawa Timur"),
                p("2. Sebagai media informasi yang dapat menyajikan informasi secara efisien kepada Bidang Pengajian 
                untuk mengetahui faktor-faktor produksi dan produktivitas menurut jenis tanaman dan kabupaten/kota"),
                p("3. Sebagai media untuk menganalisis kinerja produksi pada sektor pertanian dan perkebunan di Jawa Timur"),
                p("4. Sebagai media edukatif untuk segala elemen masyarakat agar dapat mengamati data terbaru di bidang pertanian, 
                  kehutanan, peternakan, dan perikanan di Jawa Timur")
            )),
    tabItem(tabName = "statdes",
            h2("Here are some descriptive statistics to help you understand the data better!"),
            box(title = "Choose The Plant Data You'd Like To See !",
                width = 6,
                selectInput("statchoice", "Tanaman",
                            choices = c("Padi", "Jahe", "Kencur", "Mengkudu", "Temukunci", "Alpukat",
                                        "Belimbing", "Jambu Air", "Mangga", "Pepaya"))),
            box(title = "Choose The Year Data You'd Like To See !",
                width = 6,
                selectInput("yearstat", "Tahun",
                            choices = c("2018", "2019", "2020", "2021"))),
            infoBoxOutput(width = 4, "minstat"),
            infoBoxOutput(width = 4, "medstat"),
            infoBoxOutput(width = 4, "maxstat"),
            infoBoxOutput(width = 12, "meanstat")),
    tabItem(tabName = "vis",
            h2("Welcome! In here, there will be some visuals to help you understand the data better!")
    ),
    tabItem(tabName = "pangan",
            h2("More About Crops!"),
            h2(""),
            box(width=8,
                mainPanel(plotOutput("sesuatu"))),
            box(width=4,
                title = "Crops In Indonesia",
                "Although Indonesia has a wide variety of range in crops plants, we have choosen paddy as our 
                visualization of choice. Based on the data that we have, the production of paddy in East Java Province has 
                decreased by 8,31%. Meanwhile, the harvested area also decreased by 6,62%"),
            infoBox(title = "Production", p("▼ 8.31 %"),
                    icon = icon("bar-chart"), color = "red", width = 4,
                    fill = TRUE
                    ),
            infoBox(title = "Harvested Area", p("▼ 6.62 %"),
                    icon = icon("percent"), color = "red", width = 4,
                    fill = TRUE
            ),
            box(width=6,
              selectInput("tahun", "Tahun",
                          choices = unique(datapadi$Year))),
            box(width=6,
                selectInput("kota", "Kota/Kabupaten",
                          choices = unique(datapadi$Cities))),
            box(width=6,
              mainPanel(plotOutput("histgraph"))),
            box(width=6,
              mainPanel(plotOutput("graphtrial"))
            )
            ),
    tabItem(tabName = "biofarmaka",
            h2("More About Biopharmaceutical Plants!"),
            h2(" "),
            box(width = 4, height = 6,
                title = "Biopharmaceutical Plants In Indonesia",
                "Not only Indonesia has a wide variety of range in crop plants, 
                but also in biopharmaceuticals plants. Given that there are still many of herbal products consumed, 
                biopharmaceuticals plants might be one of the leading commodities in Indonesia.
                With a vast variety of biopharmaceutical plants in Indonesia, we choose ginger, east indian galangal, 
                indian mulberry, and chinese keys as our plants of choice"),
            infoBox(title = "Production", p("▼ 51.01 %"),
                    icon = icon("percent"), color = "red", width = 4,
                    fill = TRUE
            ),
            infoBox(title = "Harvested Area", p("▼ 42.48 %"),
                    icon = icon("bar-chart"), color = "red", width = 4,
                    fill = TRUE),
            box(width=4,
                title = "Produksi",
                selectInput("bioyear", "Tahun",
                            choices = unique(databiofix$Tahun)),
                selectInput("bioplants", "Tanaman",
                            choices = unique(databiofix$Tanaman))),
            box(width=4,
                title = "Learn More About The Plants!",
                selectInput("bioplants2", "Tanaman",
                            choices = unique(databiofix$Tanaman))),
            box(width = 8,
                title = "Let's Take A Look!",
                mainPanel(plotOutput("scatteridk"))),
            box(width=4,
                title = "Our Biopharmaceutical Plants",
                mainPanel(textOutput("biodef"))
                )
            ),
            
    tabItem(tabName = "kebun",
            h2("More About Plantations!"),
            box(width=2, 
                title = "Learn More About Our Fruits !",
                selectInput("fruitdef", "Buah",
                            choices = c("Alpukat", "Belimbing", "Jambu Air", "Mangga", "Pepaya"))
            ),
            box(width = 6, title = "Plantations In Indonesia",
                "Like many other countries, Indonesia uses plantations as one of the 
                subsectors in the agricultural field. From being the people's favourite 
                snacks to the most treasured stuff, fruit plantations have done a lot in 
                international economic growth, especially in Indonesia. With all datas being fruits, 
                we have choosen orchid, star fruit, water apple, mango, and papaya."),
            infoBox(title = "Production", p("▼ 7.44 %"),
                    icon = shiny::icon("bar-chart"), color = "red", width = 4,
                    fill = TRUE
            ),
            box(width = 4, title = "Fruits In Comparison",
                "Would you like to compare the data?"),
            box(width = 4, title = "Our Fruits",
                mainPanel(textOutput("fruittext"), style="text-align: justify;")),
            box(width=4, title = "Fruit 1",
              selectInput("yearfruit", "Tahun",
                         choices = unique(databuah$Tahun)),
              selectInput("cityfruit", "Kota / Kabupaten",
                         choices = unique(databuah$Kota.Kabupaten)),
              selectInput("opsibuah", "Buah 1",
                          choices = c("Produksi Alpukat", "Produksi Belimbing", "Produksi Jambu Air", "Produksi Mangga", "Produksi Pepaya"))),
            box(width=4, title = "Fruit 2",
                selectInput("yearfruit2", "Tahun",
                            choices = unique(databuah$Tahun)),
                selectInput("cityfruit2", "Kota / Kabupaten",
                            choices = unique(databuah$Kota.Kabupaten)),
                selectInput("opsibuah2", "Buah 2",
                            choices = c("Produksi Alpukat", "Produksi Belimbing", "Produksi Jambu Air", "Produksi Mangga", "Produksi Pepaya"))),
            box(width=4,
              mainPanel(plotOutput("fruitgraph"))),
            box(width=4,
              mainPanel(plotOutput("fruitgraph2"))),
            box(title = "Fruit Data In Density",
                width = 6,
                selectInput("complexyear", "Tahun",
                            choices = unique(databuah$Tahun)),
                selectInput("complexfruit", "Buah",
                            choices = c("Alpukat", "Belimbing", "Jambu Air", "Mangga", "Pepaya"))),
            box(title = "Go Take A Look!",
                width = 6,
                mainPanel(plotOutput("densitytrial")))
            ),
    tabItem(tabName = "grow",
            h2("Start Your Business With Us!"),
            box(title = "Select The Plant You'd Like To Do Business About!", width = 6,
              selectInput("growplant", "Plant",
                choices = c("Padi", "Jahe", "Kencur", "Mengkudu", "Temukunci", "Alpukat",
                          "Belimbing", "Jambu Air", "Mangga", "Pepaya"))),
            box(title = "Or select the city you'd like to do business in !",
                width = 6,
                selectInput("growcity", "City",
                            choices = c("Pacitan", "Ponorogo", "Trenggalek", "Tulungagung",
                                        "Blitar", "Kediri", "Malang", "Lumajang", "Jember",
                                        "Banyuwangi", "Bondowoso", "Situbondo", "Probolinggo", "Pasuruan",
                                        "Sidoarjo", "Mojokerto", "Jombang","Nganjuk","Madiun", "Magetan",
                                        "Ngawi", "Bojonegoro", "Tuban", "Lamongan", "Gresik", "Bangkalan",
                                        "Sampang", "Sumenep", "Batu", "Surabaya"))),
            infoBoxOutput(width = 6, "cityresult"),
            infoBoxOutput(width = 6, "plantresult")),
    tabItem(tabName = "Penyusun",
                box(
                  title = strong("Meet Abraham!"), width = 12, solidHeader = TRUE, style="text-align: left;",style = "font-family: 'times'; font-si16pt",
                  style="text-align: center;",style = "height:250px;",
                  mainPanel(imageOutput("fotoabrhm")),
                  p(" "),
                  p(" "),
                  p("Nama     = Abraham M. Prayitno S."),
                  p("Nrp      = 5003201099"),
                  p("Email    = abraham.205003@mhs.its.ac.id"),
                  p("LinkedIn = https://www.linkedin.com/in/abraham-muhammad/")),
              box(
                title = strong("Meet Ben!"), width = 12, solidHeader = TRUE, style="text-align: left;",style = "font-family: 'times'; font-si16pt",
                style="text-align: center;",style = "height:250px;",
                mainPanel(imageOutput("fotoben")),
                p("Nama     = Ben Hugo"),
                p("Nrp      = 5003201070"),
                p("Email    = benhugo002@gmail.com"),
                p("LinkedIn = linkedin nya ben")
              )
            ))
           )

####server####
server <- function(input, output) {
  output$graph1 <- renderPlotly({
    graph2 =   ggplot(datapadi, aes(x=Luas_Panen, y=Produksi, color = Note)) +
      geom_point(alpha = 0.7, stroke = 0) +
      theme_fivethirtyeight() +
      scale_size(range=c(2,12), guide="none") +
      scale_x_log10() +
      labs(title = "Luas Panen dan Produksi Padi ",
           x = "Luas Panen",
           y = "Produksi",
           color = "Continent",
           caption = "Source: Gapminder") +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            legend.text=element_text(size=10)) +
      scale_color_brewer(palette = "Set2")
    graph2
  })
  
  choices_trial <- reactive({
    datapadi %>% filter(Cities==input$kota)
  })
  output$graphtrial=renderPlot({
    ggplot(choices_trial(), aes(x=Year, y=Produksi)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette="Dark2") +
      ggtitle("Production Trend of Paddy Based on The City")
  }, height = 400, width = 400)
  
  choices_year <- reactive({
    datapadi %>% filter(Year==input$tahun)
  })
  output$histgraph=renderPlot({
    ggplot(choices_year(), aes(x=Produksi))+
      geom_histogram(color="darkblue", fill="lightblue") +
      ggtitle("Production of Paddy Based on The Year")
  }, height = 400, width = 400)
  
  inputforfruit <- reactive({
  if(input$opsibuah == "Produksi Alpukat"){
     data = databuah %>%
       select(Kota.Kabupaten,Tahun,Produksi.Alpukat)%>%
       mutate(produksi=Produksi.Alpukat)}
  else if (input$opsibuah == "Produksi Belimbing"){
    data = databuah %>%
      select(Kota.Kabupaten,Tahun,Produksi.Belimbing)%>%
      mutate(produksi=Produksi.Belimbing)}
  else if (input$opsibuah == "Produksi Jambu Air"){
    data = databuah %>%
      select(Kota.Kabupaten,Tahun,Produksi.Jambu.Air)%>%
      mutate(produksi=Produksi.Jambu.Air)}
  else if (input$opsibuah == "Produksi Mangga"){
    data = databuah %>%
      select(Kota.Kabupaten,Tahun,Produksi.Mangga)%>%
      mutate(produksi=Produksi.Mangga)}
  else if (input$opsibuah == "Produksi Pepaya"){
    data = databuah %>%
      select(Kota.Kabupaten,Tahun,Produksi.Pepaya)%>%
      mutate(produksi=Produksi.Pepaya)}})
  
  output$fruitgraph=renderPlot({
    data1=inputforfruit()%>%filter(Kota.Kabupaten==input$cityfruit, Tahun==input$yearfruit)
    
    ggplot(data1, aes(x="", y=produksi)) +
      geom_bar(stat="identity", fill="steelblue", width = 0.2)+
      ylim(0,87342.28) +
      geom_text(aes(label=input$opsibuah), vjust=-0.3, size=3.5)+
      theme_minimal()
  }, height = 400, width = 250)
  
  output$biodef = renderText({
    if (input$bioplants2=="Jahe"){
      paste("Jahe (Zingiber officinale), adalah tumbuhan yang rimpangnya sering digunakan sebagai 
            rempah-rempah dan bahan baku pengobatan tradisional. Rimpangnya berbentuk jemari yang 
            menggembung di ruas-ruas tengah. Rasa dominan pedas yang dirasakan dari jahe disebabkan 
            oleh senyawa keton bernama zingeron.")
    }
    else if (input$bioplants2=="Kencur"){
      paste("Kencur atau cekur (Kaempferia galanga) adalah tanaman yang mempunyai akar batang yang 
            tertanam di dalam tanah, biasa dipakai untuk bahan rempah-rempah dan ramuan obat; Tanaman 
            ini termasuk dalam kingdom Plantae,  sub kingdom: Phanerogamae, divisi: Spermatophyta, sub divisi: 
            Angiospermae, kelas: Monocotyledonae, seri: Epigynae, ordo: Scitaminales, keluarga: Zingiberaceae, 
            genus: Kaempferiam, spesies: galanga. Bagian tanaman kencur yang sering digunakan adalah rimpang, 
            akar dan daun.")
    }
    else if (input$bioplants2=="Mengkudu"){
      paste("Mengkudu (Morinda citrifolia) atau keumeudee(Aceh); pace, kemudu, kudu (Jawa); cangkudu(Sunda); koddhu', 
      pacè (Madura); tibah (Bali) berasal daerah Asia Tenggara, tergolong dalam famili Rubiaceae. Nama lain untuk tanaman 
      ini adalah noni, mengkudu (Betawi), nono (Tahiti), nonu (Tonga), ungcoikan (Myanmar) dan ach (Hindi). Tanaman ini tumbuh 
            di dataran rendah hingga pada ketinggian 1500 m. Tinggi pohon mengkudu mencapai 3–8 m, memiliki bunga bongkol berwarna 
            putih. Buahnya merupakan buah majemuk, yang masih muda berwarna hijau mengkilap dan memiliki totol-totol, dan ketika 
            sudah tua berwarna putih dengan bintik-bintik hitam.")
    }
    else if (input$bioplants2=="Temukunci"){
      paste("Temukunci adalah sejenis rempah-rempah yang rimpangnya dipakai sebagai bumbu dalam masakan Asia Tenggara. Temukunci 
            adalah salah satu tanaman asli dari Indonesia khususnya di pulau Sumatera, Jawa dan juga masih hidup liar di hutan-hutan 
            daerah Jawa Tengah dan Jawa Timur.")
    }
  })
  
  output$fruittext = renderText({
    
    if(input$fruitdef=="Alpukat"){
      paste("Alpukat atau avokad adalah tumbuhan penghasil buah meja dengan nama sama. Tumbuhan ini berasal dari Meksiko dan Amerika 
            Tengah dan kini banyak dibudidayakan di Amerika Selatan dan Amerika Tengah sebagai tanaman perkebunan monokultur dan sebagai 
            tanaman pekarangan di daerah-daerah tropika lainnya di dunia.")
    }
    else if(input$fruitdef=="Belimbing"){
      paste("Belimbing / Belimbing Manis adalah tumbuhan penghasil buah berbentuk khas yang berasal dari Indonesia, India, dan Sri Lanka. 
            Saat ini, belimbing telah tersebar ke penjuru Asia Tenggara, Republik Dominika, Brasil, Peru, Ghana, Guyana, Tonga, dan Polinesia.")
    }
    else if(input$fruitdef=="Jambu Air"){
      paste("Jambu air adalah tumbuhan dalam suku jambu-jambuan atau Myrtaceae yang berasal dari Asia Tenggara. Jambu air sebetulnya 
            berbeda dengan jambu semarang, kerabat dekatnya yang memiliki pohon dan buah hampir serupa.")
    }
    else if(input$fruitdef=="Mangga"){
      paste("Mangga atau mempelam adalah nama sejenis buah, demikian pula nama pohonnya. Mangga termasuk ke dalam marga Mangifera, 
            yang terdiri dari 35-40 anggota dari suku Anacardiaceae. Nama mangga berasal dari bahasa Tamil, mankay, yang berarti man 
            (pohon mangga) + kay (buah).")
    }
    else if(input$fruitdef=="Pepaya"){
      paste("Pepaya, atau battek adalah tumbuhan yang diperkirakan berasal dari Meksiko bagian selatan dan bagian utara dari Amerika 
            Selatan. Pepaya kini telah menyebar luas dan banyak ditanam di seluruh daerah tropis untuk diambil buahnya. C. papaya adalah 
            satu-satunya jenis dalam genus Carica.")
    }
  })
  
  inputforfruit2 <- reactive({
    if(input$opsibuah2 == "Produksi Alpukat"){
      data2 = databuah %>%
        select(Kota.Kabupaten,Tahun,Produksi.Alpukat)%>%
        mutate(produksi=Produksi.Alpukat)}
    else if (input$opsibuah2 == "Produksi Belimbing"){
      data2 = databuah %>%
        select(Kota.Kabupaten,Tahun,Produksi.Belimbing)%>%
        mutate(produksi=Produksi.Belimbing)}
    else if (input$opsibuah2 == "Produksi Jambu Air"){
      data2 = databuah %>%
        select(Kota.Kabupaten,Tahun,Produksi.Jambu.Air)%>%
        mutate(produksi=Produksi.Jambu.Air)}
    else if (input$opsibuah2 == "Produksi Mangga"){
      data2 = databuah %>%
        select(Kota.Kabupaten,Tahun,Produksi.Mangga)%>%
        mutate(produksi=Produksi.Mangga)}
    else if (input$opsibuah2 == "Produksi Pepaya"){
      data2 = databuah %>%
        select(Kota.Kabupaten,Tahun,Produksi.Pepaya)%>%
        mutate(produksi=Produksi.Pepaya)}})
  
  output$fruitgraph2=renderPlot({
    data3=inputforfruit2()%>%filter(Kota.Kabupaten==input$cityfruit2, Tahun==input$yearfruit2)
    
    ggplot(data3, aes(x="", y=produksi)) +
      geom_bar(stat="identity", fill="steelblue", width = 0.2)+
      ylim(0,87342.28) +
      geom_text(aes(label=input$opsibuah2), vjust=-0.3, size=3.5)+
      theme_minimal()
  }, height = 400, width = 250)
  
  inputforfruit3 <- reactive({
    if(input$complexfruit == "Alpukat"){
      datafr = databuah %>%
        select(Tahun,Produksi.Alpukat,Wilayah)%>%
        mutate(density1=Produksi.Alpukat)}
    else if (input$complexfruit == "Belimbing"){
      datafr = databuah %>%
        select(Tahun,Produksi.Belimbing,Wilayah)%>%
        mutate(density1=Produksi.Belimbing)}
    else if (input$complexfruit == "Jambu Air"){
      datafr = databuah %>%
        select(Tahun,Produksi.Jambu.Air,Wilayah)%>%
        mutate(density1=Produksi.Jambu.Air)}
    else if (input$complexfruit == "Mangga"){
      datafr = databuah %>%
        select(Tahun,Produksi.Mangga,Wilayah)%>%
        mutate(density1=Produksi.Mangga)}
    else if (input$complexfruit == "Pepaya"){
      datafr = databuah %>%
        select(Tahun,Produksi.Pepaya,Wilayah)%>%
        mutate(density1=Produksi.Pepaya)}})
  
  output$densitytrial = renderPlot({
    datafr1=inputforfruit3()%>%filter(Tahun==input$complexyear,Wilayah==Wilayah)
    
    ggplot(datafr1, aes(x = "", y = density1, fill = Wilayah)) +
      # violin
      geom_violin(trim=FALSE, alpha = 0.5,
                                     position = position_nudge(0.15)) +
      # jitter
      geom_jitter(aes(colour = Wilayah), size = 3, alpha = .5, show.legend = FALSE, 
                  position = position_jitter(width = 0.1)) +
      # boxplots
      geom_boxplot(width = 0.1, alpha = 0.4, show.legend = FALSE, 
                   position = position_nudge(x = -0.2)) +
      coord_flip() +
      ggtitle("Visualization of Production") +
      scale_fill_brewer(palette = "Set2") + 
      scale_color_brewer(palette = "Set2")
  }, height = 400, width = 400)
  
  output$sesuatu = renderPlot({
    ggplot(datapadi, aes(x = Year, y = Produktivitas, fill = Note)) +
      geom_violin(alpha = 0.5, trim = FALSE)+
      ggtitle("Visualization of Productivity") +
      scale_fill_brewer(palette = "Set2") + 
      scale_color_brewer(palette = "Set2")
  }, height = 400, width = 500 )
  
  output$graphtrial=renderPlot({
    ggplot(choices_trial(), aes(x=Year, y=Produksi)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette="Dark2") +
      ggtitle("Production Trend of Paddy Based on The City")
  }, height = 400, width = 400)
  
  biochoice1 <- reactive({
    databiofix %>% filter(Tanaman==input$bioplants,Tahun==input$bioyear,
                          Wilayah==Wilayah)
  })
  
  output$scatteridk=renderPlot({
    ggplot(biochoice1(), 
           aes(x = Produksi, y = Luas.Panen,  colour = Wilayah))+ 
      geom_point(alpha = 0.8, fill = "black") + 
      scale_size(range = c(.1, 10)) + 
      ggtitle("Biopharmaceutical Plants Scatterplot",
              subtitle = "Production vs Harvested Area") + 
      scale_color_brewer(palette = "Set1")
  }, height = 400, width = 525)
  
  output$plantresult <- renderInfoBox({
    infoBox(title = "Plant Recomendation",
             p(if(input$growcity=="Surabaya"){
               buahsby21[buahsby21$Produksi==max(buahsby21$Produksi),]$Tanaman
             }
             else if(input$growcity=="Pacitan"){
               datapac[datapac$Produksi==max(datapac$Produksi),]$Tanaman
             }
             else if(input$growcity=="Ponorogo"){
               datapnrg[datapnrg$Produksi==max(datapnrg$Produksi),]$Tanaman
             }
             else if(input$growcity=="Trenggalek"){
               datatrng[datatrng$Produksi==max(datatrng$Produksi),]$Tanaman
             }
             else if(input$growcity=="Tulungagung"){
               dataagung[dataagung$Produksi==max(dataagung$Produksi),]$Tanaman
             }
             else if(input$growcity=="Blitar"){
               databl[databl$Produksi==max(databl$Produksi),]$Tanaman
             }
             else if(input$growcity=="Kediri"){
               datakd[datakd$Produksi==max(datakd$Produksi),]$Tanaman
             }
             else if(input$growcity=="Malang"){
               datamlg[datamlg$Produksi==max(datamlg$Produksi),]$Tanaman
             }
             else if(input$growcity=="Lumajang"){
               datalum[datalum$Produksi==max(datalum$Produksi),]$Tanaman
             }
             else if(input$growcity=="Jember"){
               datajem[datajem$Produksi==max(datajem$Produksi),]$Tanaman
             }
             else if(input$growcity=="Banyuwangi"){
               databan[databan$Produksi==max(databan$Produksi),]$Tanaman
             }
             else if(input$growcity=="Bondowoso"){
               databon[databon$Produksi==max(databon$Produksi),]$Tanaman
             }
             else if(input$growcity=="Situbondo"){
               datasitu[datasitu$Produksi==max(datasitu$Produksi),]$Tanaman
             }
             else if(input$growcity=="Probolinggo"){
               datapr[datapr$Produksi==max(datapr$Produksi),]$Tanaman
             }
             else if(input$growcity=="Pasuruan"){
               datapas[datapas$Produksi==max(datapas$Produksi),]$Tanaman
             }
             else if(input$growcity=="Sidoarjo"){
               datasid[datasid$Produksi==max(datasid$Produksi),]$Tanaman
             }
             else if(input$growcity=="Mojokerto"){
               datamj[datamj$Produksi==max(datamj$Produksi),]$Tanaman
             }
             else if(input$growcity=="Jombang"){
               datajom[datajom$Produksi==max(datajom$Produksi),]$Tanaman
             }
             else if(input$growcity=="Nganjuk"){
               datangan[datangan$Produksi==max(datangan$Produksi),]$Tanaman
             }
             else if(input$growcity=="Madiun"){
               datamad[datamad$Produksi==max(datamad$Produksi),]$Tanaman
             }
             else if(input$growcity=="Magetan"){
               datamag[datamag$Produksi==max(datamag$Produksi),]$Tanaman
             }
             else if(input$growcity=="Ngawi"){
               datangawi[datangawi$Produksi==max(datangawi$Produksi),]$Tanaman
             }
             else if(input$growcity=="Bojonegoro"){
               databjn[databjn$Produksi==max(databjn$Produksi),]$Tanaman
             }
             else if(input$growcity=="Tuban"){
               datatbn[datatbn$Produksi==max(datatbn$Produksi),]$Tanaman
             }
             else if(input$growcity=="Lamongan"){
               datalam[datalam$Produksi==max(datalam$Produksi),]$Tanaman
             }
             else if(input$growcity=="Gresik"){
               datagres[datagres$Produksi==max(datagres$Produksi),]$Tanaman
             }
             else if(input$growcity=="Bangkalan"){
               databang[databang$Produksi==max(databang$Produksi),]$Tanaman
             }
             else if(input$growcity=="Sampang"){
               datasam[datasam$Produksi==max(datasam$Produksi),]$Tanaman
             }
             else if(input$growcity=="Sumenep"){
               datasum[datasum$Produksi==max(datasum$Produksi),]$Tanaman
             }
             else if(input$growcity=="Batu"){
               databatu[databatu$Produksi==max(databatu$Produksi),]$Tanaman
             }))})
  
  output$cityresult <- renderInfoBox({
    infoBox(title = "City Recomendation",
            p(if(input$growplant=="Padi"){
              padi2021[padi2021$Produksi==max(padi2021$Produksi),]$Cities
            }
            else if(input$growplant=="Jahe"){
              bio2021[bio2021$Produksi.Jahe==max(bio2021$Produksi.Jahe),]$Kota.Kabupaten
            }
            else if(input$growplant=="Kencur"){
              bio2021[bio2021$Produksi.Kencur==max(bio2021$Produksi.Kencur),]$Kota.Kabupaten
            }
            else if(input$growplant=="Mengkudu"){
              bio2021[bio2021$Produksi.Mengkudu==max(bio2021$Produksi.Mengkudu),]$Kota.Kabupaten
            }
            else if(input$growplant=="Temukunci"){
              bio2021[bio2021$Produksi.Temukunci==max(bio2021$Produksi.Temukunci),]$Kota.Kabupaten
            }
            else if(input$growplant=="Alpukat"){
              buah2021[buah2021$Produksi.Alpukat==max(buah2021$Produksi.Alpukat),]$Kota.Kabupaten
            }
            else if(input$growplant=="Belimbing"){
              buah2021[buah2021$Produksi.Belimbing==max(buah2021$Produksi.Belimbing),]$Kota.Kabupaten
            }
            else if(input$growplant=="Jambu Air"){
              buah2021[buah2021$Produksi.Jambu.Air==max(buah2021$Produksi.Jambu.Air),]$Kota.Kabupaten
            }
            else if(input$growplant=="Mangga"){
              buah2021[buah2021$Produksi.Mangga==max(buah2021$Produksi.Mangga),]$Kota.Kabupaten
            }
            else if(input$growplant=="Pepaya"){
              buah2021[buah2021$Produksi.Pepaya==max(buah2021$Produksi.Pepaya),]$Kota.Kabupaten
            }))
  })
  
  output$minstat <- renderInfoBox({
    infoBox(title = "Minimum Value",
            p(if(input$statchoice=="Padi" & input$yearstat=="2018"){
              min(padi2018$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2019"){
              min(padi2019$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2020"){
              min(padi2020$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2021"){
              min(padi2021$Produksi)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2018"){
              min(bio2018$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2019"){
              min(bio2019$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2020"){
              min(bio2020$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2021"){
              min(bio2021$Produksi.Jahe)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2018"){
              min(bio2018$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2019"){
              min(bio2019$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2020"){
              min(bio2020$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2021"){
              min(bio2021$Produksi.Kencur)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2018"){
              min(bio2018$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2019"){
              min(bio2019$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2020"){
              min(bio2020$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2021"){
              min(bio2021$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2018"){
              min(bio2018$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2019"){
              min(bio2019$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2020"){
              min(bio2020$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2021"){
              min(bio2021$Produksi.Temukunci)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2018"){
              min(buah2018$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2019"){
              min(buah2019$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2020"){
              min(buah2020$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2021"){
              min(buah2021$Produksi.Alpukat)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2018"){
              min(buah2018$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2019"){
              min(buah2019$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2020"){
              min(buah2020$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2021"){
              min(buah2021$Produksi.Belimbing)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2018"){
              min(buah2018$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2019"){
              min(buah2019$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2020"){
              min(buah2020$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2021"){
              min(buah2021$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2018"){
              min(buah2018$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2019"){
              min(buah2019$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2020"){
              min(buah2020$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2021"){
              min(buah2021$Produksi.Mangga)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2018"){
              min(buah2018$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2019"){
              min(buah2019$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2020"){
              min(buah2020$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2021"){
              min(buah2021$Produksi.Pepaya)
            })
      
    )
  })
  
  output$medstat <- renderInfoBox({
    infoBox(title = "Median Value",
            p(if(input$statchoice=="Padi" & input$yearstat=="2018"){
              median(padi2018$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2019"){
              median(padi2019$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2020"){
              median(padi2020$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2021"){
              median(padi2021$Produksi)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2018"){
              median(bio2018$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2019"){
              median(bio2019$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2020"){
              median(bio2020$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2021"){
              median(bio2021$Produksi.Jahe)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2018"){
              median(bio2018$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2019"){
              median(bio2019$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2020"){
              median(bio2020$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2021"){
              median(bio2021$Produksi.Kencur)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2018"){
              median(bio2018$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2019"){
              median(bio2019$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2020"){
              median(bio2020$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2021"){
              median(bio2021$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2018"){
              median(bio2018$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2019"){
              median(bio2019$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2020"){
              median(bio2020$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2021"){
              median(bio2021$Produksi.Temukunci)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2018"){
              median(buah2018$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2019"){
              median(buah2019$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2020"){
              median(buah2020$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2021"){
              median(buah2021$Produksi.Alpukat)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2018"){
              median(buah2018$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2019"){
              median(buah2019$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2020"){
              median(buah2020$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2021"){
              median(buah2021$Produksi.Belimbing)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2018"){
              median(buah2018$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2019"){
              median(buah2019$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2020"){
              median(buah2020$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2021"){
              median(buah2021$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2018"){
              median(buah2018$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2019"){
              median(buah2019$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2020"){
              median(buah2020$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2021"){
              median(buah2021$Produksi.Mangga)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2018"){
              median(buah2018$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2019"){
              median(buah2019$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2020"){
              median(buah2020$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2021"){
              median(buah2021$Produksi.Pepaya)
            })
            
    )
  })

  output$maxstat <- renderInfoBox({
    infoBox(title = "Maximum Value",
            p(if(input$statchoice=="Padi" & input$yearstat=="2018"){
              max(padi2018$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2019"){
              max(padi2019$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2020"){
              max(padi2020$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2021"){
              max(padi2021$Produksi)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2018"){
              max(bio2018$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2019"){
              max(bio2019$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2020"){
              max(bio2020$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2021"){
              max(bio2021$Produksi.Jahe)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2018"){
              max(bio2018$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2019"){
              max(bio2019$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2020"){
              max(bio2020$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2021"){
              max(bio2021$Produksi.Kencur)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2018"){
              max(bio2018$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2019"){
              max(bio2019$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2020"){
              max(bio2020$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2021"){
              max(bio2021$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2018"){
              max(bio2018$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2019"){
              max(bio2019$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2020"){
              max(bio2020$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2021"){
              max(bio2021$Produksi.Temukunci)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2018"){
              max(buah2018$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2019"){
              max(buah2019$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2020"){
              max(buah2020$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2021"){
              max(buah2021$Produksi.Alpukat)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2018"){
              max(buah2018$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2019"){
              max(buah2019$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2020"){
              max(buah2020$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2021"){
              max(buah2021$Produksi.Belimbing)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2018"){
              max(buah2018$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2019"){
              max(buah2019$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2020"){
              max(buah2020$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2021"){
              max(buah2021$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2018"){
              max(buah2018$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2019"){
              max(buah2019$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2020"){
              max(buah2020$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2021"){
              max(buah2021$Produksi.Mangga)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2018"){
              max(buah2018$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2019"){
              max(buah2019$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2020"){
              max(buah2020$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2021"){
              max(buah2021$Produksi.Pepaya)
            })
            
    )
  })
  
  output$meanstat <- renderInfoBox({
    infoBox(title = "Average",
            p(if(input$statchoice=="Padi" & input$yearstat=="2018"){
              mean(padi2018$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2019"){
              mean(padi2019$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2020"){
              mean(padi2020$Produksi)
            }
            else if(input$statchoice=="Padi" & input$yearstat=="2021"){
              mean(padi2021$Produksi)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2018"){
              mean(bio2018$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2019"){
              mean(bio2019$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2020"){
              mean(bio2020$Produksi.Jahe)
            }
            else if(input$statchoice=="Jahe" & input$yearstat=="2021"){
              mean(bio2021$Produksi.Jahe)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2018"){
              mean(bio2018$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2019"){
              mean(bio2019$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2020"){
              mean(bio2020$Produksi.Kencur)
            }
            else if(input$statchoice=="Kencur" & input$yearstat=="2021"){
              mean(bio2021$Produksi.Kencur)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2018"){
              mean(bio2018$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2019"){
              mean(bio2019$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2020"){
              mean(bio2020$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Mengkudu" & input$yearstat=="2021"){
              mean(bio2021$Produksi.Mengkudu)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2018"){
              mean(bio2018$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2019"){
              mean(bio2019$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2020"){
              mean(bio2020$Produksi.Temukunci)
            }
            else if(input$statchoice=="Temukunci" & input$yearstat=="2021"){
              mean(bio2021$Produksi.Temukunci)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2018"){
              mean(buah2018$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2019"){
              mean(buah2019$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2020"){
              mean(buah2020$Produksi.Alpukat)
            }
            else if(input$statchoice=="Alpukat" & input$yearstat=="2021"){
              mean(buah2021$Produksi.Alpukat)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2018"){
              mean(buah2018$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2019"){
              mean(buah2019$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2020"){
              mean(buah2020$Produksi.Belimbing)
            }
            else if(input$statchoice=="Belimbing" & input$yearstat=="2021"){
              mean(buah2021$Produksi.Belimbing)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2018"){
              mean(buah2018$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2019"){
              mean(buah2019$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2020"){
              mean(buah2020$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Jambu Air" & input$yearstat=="2021"){
              mean(buah2021$Produksi.Jambu.Air)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2018"){
              mean(buah2018$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2019"){
              mean(buah2019$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2020"){
              mean(buah2020$Produksi.Mangga)
            }
            else if(input$statchoice=="Mangga" & input$yearstat=="2021"){
              mean(buah2021$Produksi.Mangga)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2018"){
              mean(buah2018$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2019"){
              mean(buah2019$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2020"){
              mean(buah2020$Produksi.Pepaya)
            }
            else if(input$statchoice=="Pepaya" & input$yearstat=="2021"){
              mean(buah2021$Produksi.Pepaya)
            })
            
    )
  })
  
  output$fotoabrhm <- renderImage({
    list(src="www/fotoabrhm.png", height = 200, width = 160)
  }, deleteFile = F)
  output$fotoben <- renderImage({
    list(src="www/fotoben.jpeg", height = 200, width = 160)
  }, deleteFile = F)
  var1<-reactive({
    paste0("Variabel tahun menjelaskan tahun pengambilan data. Dari topik yang diberikan,
           kita menggunakan tahun 2018 - 2021 untuk mengetahui perkembangan dan/atau depresiasi
           selama tahun tersebut.")
  })
  var2<-reactive({
    paste0("Variabel kabupaten/kota menjelaskan wilayah administratif pengambilan data.
           Data diambil dari 38 Kabupaten dan Kota di Jawa Timur.")
  })
  var3<-reactive({
    paste0("Variabel luas panen menunjukkan seberapa luas lahan yang dipakai 
           untuk memanen hasil pertanian di Jawa Timur pada 2018 - 2021. Variabel ini dihitung
           dengan satuan hektar.")
  })
  var4<-reactive({
    paste0("Variabel produktivitas digunakan untuk mengetahui tingkat produktivitas petani dan related worker
           dalam memproduksi hasil pertanian. Variabel ini menggunakan satuan kuintal/hektar")
  })
  var5<-reactive({
    paste0("Variabel produksi digunakan untuk mengetahui tingkat produksi hasil tani di Jawa Timur 
           pada 2018 - 2021. Variabel ini menggunakan satuan ton")
  })
  var6<-reactive({
    paste0("Variabel jenis tanaman merupakan variabel yang menjelaskan tanaman apa saja yang akan dipakai. Dalam data ini,
           kita memakai beberapa jenis tanaman, mulai dari tanaman pangan hingga tanaman biofarmaka")
  })
  var7<-reactive({
    paste0("Variabel jenis buah merupakan variabel yang menjelaskan buah apa saja yang akan dipakai. Dalam data ini, 
           kita memakai beberapa jenis buah, antara lain, alpukat, belimbing, jambu air, mangga, serta pepaya")
  })
  observeEvent(input$var1, {
    showModal(modalDialog(var1(), title = 'Year'))
  })
  observeEvent(input$var2, {
    showModal(modalDialog(var2(), title = 'Cities'))
  })
  observeEvent(input$var3, {
    showModal(modalDialog(var3(), title = 'Harvested Area'))
  })
  observeEvent(input$var4, {
    showModal(modalDialog(var4(), title = 'Productivity'))
  })
  observeEvent(input$var5, {
    showModal(modalDialog(var5(), title = 'Production'))
  })
  observeEvent(input$var6, {
    showModal(modalDialog(var6(), title = 'Type of Plant'))
  })
  observeEvent(input$var7, {
    showModal(modalDialog(var7(), title = 'Type of Fruit'))
  })
}

ui=dashboardPage(header=headerItem,
                 sidebar = sidebarItem,
                 body = bodies,
                 skin="blue-light")

####shinyyyyy####
shinyApp(ui, server)
