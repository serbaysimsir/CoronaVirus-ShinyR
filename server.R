shinyServer(function(input, output) {
    #Dunya Bazinda Ekrani
    output$dunyaGeneli <- renderPlotly({
        t <- list(
            size = 9)
        p <- plot_ly(dunyabazindabulasmis, x=~parsedate::parse_iso_8601(dunyabazindabulasmis$Tarih), y=~dunyabazindabulasmis$Deger, name ="Vaka Sayısı", type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~dunyabazindakurtulmus$Deger, name = 'İyileşen Hasta Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~dunyabazindaolmus$Deger, name = 'Vefat Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~dunyabazindaaktif$Deger, name = 'Aktif Hasta Sayısı', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>Dünya Genelindeki Korona Virüsü Vakaları Zamansal Değişim Grafiği<b>", sep = ""), yaxis= list(overlaying = "y",  title = "<b>Kişi Sayısı</b>"),
                xaxis = list(title="<b>Tarih<b>")
            )
    })
    output$dunyaGeneliBuyume <- renderPlotly({
        t <- list(
            size = 9)
        bulasmisbuyumeOranlari <- dunyabazindabulasmis$Deger
        kurtulmusbuyumeOranlari <- dunyabazindakurtulmus$Deger
        olmusbuyumeOranlari  <-  dunyabazindaolmus$Deger
        for(i in length(dunyabazindabulasmis$Deger):1){
            if(i > 2)
            {
                bulasmisbuyumeOranlari[i] <- round(((dunyabazindabulasmis$Deger[i]-dunyabazindabulasmis$Deger[i-1]) / (dunyabazindabulasmis$Deger[i-1] - dunyabazindabulasmis$Deger[i-2])), digits = 2)
                kurtulmusbuyumeOranlari[i] <- round(((dunyabazindakurtulmus$Deger[i]-dunyabazindakurtulmus$Deger[i-1]) / (dunyabazindakurtulmus$Deger[i-1] - dunyabazindakurtulmus$Deger[i-2])), digits = 2)
                olmusbuyumeOranlari[i] <- round(((dunyabazindaolmus$Deger[i]-dunyabazindaolmus$Deger[i-1]) / (dunyabazindaolmus$Deger[i-1] - dunyabazindaolmus$Deger[i-2])), digits = 2)
            }
            else if(i==2)
            {
                bulasmisbuyumeOranlari[i] <- round(((dunyabazindabulasmis$Deger[i]-dunyabazindabulasmis$Deger[i-1]) / dunyabazindabulasmis$Deger[i-1]), digits = 2)
                kurtulmusbuyumeOranlari[i] <- round(((dunyabazindakurtulmus$Deger[i]-dunyabazindakurtulmus$Deger[i-1]) / dunyabazindakurtulmus$Deger[i-1]), digits = 2)
                olmusbuyumeOranlari[i] <- round(((dunyabazindaolmus$Deger[i]-dunyabazindaolmus$Deger[i-1]) / dunyabazindaolmus$Deger[i-1]), digits = 2)
            }
            else {
                bulasmisbuyumeOranlari[i] <- NaN
                kurtulmusbuyumeOranlari[i] <- NaN
                olmusbuyumeOranlari[i] <- NaN
            }
        }
        buyume <- cbind.data.frame(dunyabazindabulasmis$Tarih, bulasmisbuyumeOranlari,kurtulmusbuyumeOranlari,olmusbuyumeOranlari)
        colnames(buyume) <- c("Tarih", "Bulasmis", "Kurtulmus", "Olmus")
        p <- plot_ly(buyume, x=~parsedate::parse_iso_8601(buyume$Tarih), y=~buyume$Bulasmis, name ="Vaka Sayısının Büyüme Faktörü ", type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~buyume$Kurtulmus, name = 'İyileşen Hasta Sayısının Büyüme Faktörü', mode = 'lines+markers') %>%
            add_trace(y = ~buyume$Olmus, name = 'Vefat Sayısının Büyüme Faktörü', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>Dünya Genelindeki Korona Virüsü Vakalarının Büyüme Faktörünün Zamansal Değişim Grafiği<b>", sep = ""), yaxis= list(overlaying = "y",  title = "<b>Büyüme Faktörü</b>"),
                xaxis = list(title="<b>Tarih<b>")
            )
    })
    output$dunyabulasmisSayi <- renderUI({
        h1(br(), dunyabazindabulasmis$Deger[length(dunyabazindabulasmis$Deger)] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$dunyaiyilesmisSayi <- renderUI({
        h1(br(), dunyabazindakurtulmus$Deger[length(dunyabazindakurtulmus$Deger)] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$dunyaolmusSayi <- renderUI({
        ulke <- input$Ulke
        h1(br(), dunyabazindaolmus$Deger[length(dunyabazindaolmus$Deger)] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$dunyaaktifSayi <- renderUI({
        h1(br(), dunyabazindaaktif$Deger[length(dunyabazindaaktif$Deger)] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$kiyaslama <- renderPlotly({
        t <- list(
            size = 9)
        ulke1 <- input$birinciUlke
        ulke2 <- input$ikinciUlke
        ulke3 <- input$ucuncuUlke
        birinci_data <- NULL
        ikinci_data <- NULL
        ucuncu_data <- NULL
        oznitelik <- ""
        ytitle <- "<b>Kişi Sayısı</b>"
        anatitle <- "" 
        if(input$oznitelik == "Vaka Sayısı" || input$oznitelik=="Vaka Sayısının Büyüme Faktörü")
            oznitelik <- "bulasmis"
        else if(input$oznitelik == "İyileşen Hasta Sayısı")
            oznitelik <- "kurtulmus"
        else if(input$oznitelik == "Vefat Sayısı")
            oznitelik <- "olmus"
        else if(input$oznitelik == "Aktif Hasta Sayısı")
            oznitelik <- "aktif"
        birinci_data <- sqldf(paste("SELECT Tarih, Deger FROM ", oznitelik ," WHERE Ulke = '", ulke1 ,"'" ,sep=""))
        ikinci_data <- sqldf(paste("SELECT Tarih, Deger FROM ", oznitelik ," WHERE Ulke = '", ulke2 ,"'" ,sep=""))
        if(input$ucuncuUlke != "Yok")
        {
            ucuncu_data <- sqldf(paste("SELECT Tarih, Deger FROM ", oznitelik ," WHERE Ulke = '", ulke3 ,"'" ,sep=""))
            anatitle <- paste("<b>", ulke1, " & ", ulke2, " & ", ulke3, " Ülkelerinin ", input$oznitelik, "na Göre Kıyas Grafiği <b>", sep = "")
        }
        else
        {
            ucuncu_data <- ikinci_data
            ucuncu_data$Deger = NaN
            anatitle <- paste("<b>", ulke1, " & ", ulke2," Ülkelerinin ", input$oznitelik, "na Göre Kıyas Grafiği <b>", sep = "")
        }
        if(input$oznitelik=="Vaka Sayısının Büyüme Faktörü")
        {
            birincibuyumeOranlari <- birinci_data$Deger
            ikincibuyumeOranlari <- ikinci_data$Deger
            ucuncubuyumeOranlari  <-  ucuncu_data$Deger
            for(i in length(birinci_data$Deger):1){
                if(i > 2)
                {
                    birincibuyumeOranlari[i] <- round(((birinci_data$Deger[i]-birinci_data$Deger[i-1]) / (birinci_data$Deger[i-1] - birinci_data$Deger[i-2])), digits = 2)
                    ikincibuyumeOranlari[i] <- round(((ikinci_data$Deger[i]-ikinci_data$Deger[i-1]) / (ikinci_data$Deger[i-1] - ikinci_data$Deger[i-2])), digits = 2)
                    ucuncubuyumeOranlari[i] <- round(((ucuncu_data$Deger[i]-ucuncu_data$Deger[i-1]) / (ucuncu_data$Deger[i-1] - ucuncu_data$Deger[i-2])), digits = 2)
                }
                else if(i==2)
                {
                    birincibuyumeOranlari[i] <- round(((birinci_data$Deger[i]-birinci_data$Deger[i-1]) / birinci_data$Deger[i-1]), digits = 2)
                    ikincibuyumeOranlari[i] <- round(((ikinci_data$Deger[i]-ikinci_data$Deger[i-1]) / ikinci_data$Deger[i-1]), digits = 2)
                    ucuncubuyumeOranlari[i] <- round(((ucuncu_data$Deger[i]-ucuncu_data$Deger[i-1]) / ucuncu_data$Deger[i-1]), digits = 2)
                }
                else {
                    birincibuyumeOranlari[i] <- NaN
                    ikincibuyumeOranlari[i] <- NaN
                    ucuncubuyumeOranlari[i] <- NaN
                }
            }
            birinci_data$Deger <- birincibuyumeOranlari
            ikinci_data$Deger <- ikincibuyumeOranlari
            ucuncu_data$Deger <- ucuncubuyumeOranlari
            ytitle <-"<b>Büyüme Faktörü</b>"
        }
        p <- plot_ly(birinci_data, x=~parsedate::parse_iso_8601(birinci_data$Tarih), y=~birinci_data$Deger, name = ulke1, type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~ikinci_data$Deger, name = ulke2 , mode = 'lines+markers') %>%
            add_trace(y = ~ucuncu_data$Deger, name = ulke3, mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
        layout(
            legend = list(x = 0.1, y = 0.9), font=t, title = anatitle, yaxis= list( title = ytitle),
            xaxis = list(title="<b>Tarih<b>")
            )
    })
    output$tarihKiyaslama <- renderPlotly({
        t <- list(
            size = 9)
        ulke1 <- input$birinciUlke2
        ulke2 <- input$ikinciUlke2
        ulke3 <- input$ucuncuUlke2
        oznitelik <- ""
        ytitle <- "<b>Kişi Sayısı</b>"
        gun <- 0
        birinci_data <- NULL
        ikinci_data <- NULL
        ucuncu_data <- NULL
        if(input$oznitelik2 == "Vaka Sayısı" || input$oznitelik2=="Vaka Sayısının Büyüme Faktörü")
            oznitelik <- "bulasmis"
        else if(input$oznitelik2 == "İyileşen Hasta Sayısı")
            oznitelik <- "kurtulmus"
        else if(input$oznitelik2 == "Vefat Sayısı")
            oznitelik <- "olmus"
        else if(input$oznitelik2 == "Aktif Hasta Sayısı")
            oznitelik <- "aktif"
        if(input$gunSayisi == "1")
            gun <- 1 
        else if(input$gunSayisi == "5")
            gun <- 5
        else if(input$gunSayisi == "10")
            gun <- 10
        else if(input$gunSayisi == "20")
            gun <- 20
        else if(input$gunSayisi == "30")
            gun <- 30
        else if(input$gunSayisi == "40")
            gun <- 40
        else if(input$gunSayisi == "50")
            gun <- 50
        birinci_data <- sqldf(paste("SELECT Deger FROM ", oznitelik ," WHERE Ulke = '", ulke1, "'  AND Tarih>= '",as.Date(input$date1),"' AND Tarih < '", as.Date(input$date1) + gun, "'",sep=""))$Deger[1:gun]
        ikinci_data <- sqldf(paste("SELECT Deger FROM ", oznitelik ," WHERE Ulke = '", ulke2, "'  AND Tarih>='",as.Date(input$date2),"' AND Tarih < '", as.Date(input$date2) + gun, "'",sep=""))$Deger[1:gun]
        if(input$ucuncuUlke2 != "Yok")
        {
            ucuncu_data <- sqldf(paste("SELECT Deger FROM ", oznitelik ," WHERE Ulke = '", ulke3, "' AND Tarih>='",as.Date(input$date3),"' AND Tarih < '", as.Date(input$date3) + gun, "'",sep=""))$Deger[1:gun]
            anatitle <- paste("<b>", ulke1, " & ", ulke2, " & ", ulke3, " Ülkelerinin ", input$oznitelik, "na Göre Kıyas Grafiği <b>", sep = "")
        }
        else
        {
            ucuncu_data <- ikinci_data
            ucuncu_data <- NaN
            anatitle <- paste("<b>", ulke1, " & ", ulke2, " Ülkelerinin ", input$oznitelik2, "na Göre Kıyas Grafiği <b>", sep = "")
        }
        if(input$oznitelik2=="Vaka Sayısının Büyüme Faktörü")
        {
            birincibuyumeOranlari <- birinci_data
            ikincibuyumeOranlari <- ikinci_data
            ucuncubuyumeOranlari  <-  ucuncu_data
            for(i in length(birinci_data):1){
                if(i > 2)
                {
                    birincibuyumeOranlari[i] <- round(((birinci_data[i]-birinci_data[i-1]) / (birinci_data[i-1] - birinci_data[i-2])), digits = 2)
                    ikincibuyumeOranlari[i] <- round(((ikinci_data[i]-ikinci_data[i-1]) / (ikinci_data[i-1] - ikinci_data[i-2])), digits = 2)
                    ucuncubuyumeOranlari[i] <- round(((ucuncu_data[i]-ucuncu_data[i-1]) / (ucuncu_data[i-1] - ucuncu_data[i-2])), digits = 2)
                }
                else if(i==2)
                {
                    birincibuyumeOranlari[i] <- round(((birinci_data[i]-birinci_data[i-1]) / birinci_data[i-1]), digits = 2)
                    ikincibuyumeOranlari[i] <- round(((ikinci_data[i]-ikinci_data[i-1]) / ikinci_data[i-1]), digits = 2)
                    ucuncubuyumeOranlari[i] <- round(((ucuncu_data[i]-ucuncu_data[i-1]) / ucuncu_data[i-1]), digits = 2)
                }
                else {
                    birincibuyumeOranlari[i] <- NaN
                    ikincibuyumeOranlari[i] <- NaN
                    ucuncubuyumeOranlari[i] <- NaN
                }
            }
            birinci_data <- birincibuyumeOranlari
            ikinci_data <- ikincibuyumeOranlari
            ucuncu_data <- ucuncubuyumeOranlari
            ytitle <-"<b>Büyüme Faktörü</b>"
        }
        veriler <- cbind.data.frame(seq(1,gun,1), birinci_data, ikinci_data, ucuncu_data)
        colnames(veriler) <- c("Gun", "Ulke1", "Ulke2","Ulke3")
        p <- plot_ly(veriler, x=veriler$Gun, y=~veriler$Ulke1, name = ulke1, type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~veriler$Ulke2, name = ulke2 , mode = 'lines+markers') %>% 
            add_trace(y = ~veriler$Ulke3, name = ulke3, mode = 'lines+markers')  %>% 
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = anatitle, yaxis= list( title = ytitle),
                xaxis = list(title="<b>Gün Sayısı<b>")
            )
    })
    #Ulke Bazinda Ekrani
    output$grafik <- renderPlotly({
        t <- list(
            size = 9)
        ulke <- input$Ulke
        bulasmisUlke <- sqldf(paste("SELECT Tarih, Deger FROM bulasmis WHERE Ulke ='", ulke,"'",sep=""))
        kurtulmusUlke <- sqldf(paste("SELECT Deger FROM kurtulmus WHERE Ulke ='", ulke,"'" ,sep=""))
        olmusUlke <- sqldf(paste("SELECT Deger FROM olmus WHERE Ulke ='", ulke,"'" ,sep=""))
        aktifUlke <- sqldf(paste("SELECT Deger FROM aktif WHERE Ulke ='", ulke,"'" ,sep=""))
        p <- plot_ly(bulasmisUlke, x=~parsedate::parse_iso_8601(bulasmisUlke$Tarih), y=~bulasmisUlke$Deger, name ="Vaka Sayısı", type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~kurtulmusUlke$Deger, name = 'İyileşen Hasta Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~olmusUlke$Deger, name = 'Vefat Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~aktifUlke$Deger, name = 'Aktif Hasta Sayısı', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>",ulke, " Ülkesindeki Korona Virüsü Vakalarının Zamansal Değişim Grafiği <b>", sep = ""), yaxis= list( title = "<b>Kişi Sayısı</b>"),
                xaxis = list(title="<b>Tarih<b>")
            )
    })
    output$ulkeGeneliBuyume <- renderPlotly({
        t <- list(
            size = 9)
        ulke <- input$Ulke
        bulasmisUlke <- sqldf(paste("SELECT Tarih, Deger FROM bulasmis WHERE Ulke ='", ulke,"'",sep=""))
        kurtulmusUlke <- sqldf(paste("SELECT Deger FROM kurtulmus WHERE Ulke ='", ulke,"'" ,sep=""))
        olmusUlke <- sqldf(paste("SELECT Deger FROM olmus WHERE Ulke ='", ulke,"'" ,sep=""))
        bulasmisbuyumeOranlari <- bulasmisUlke$Deger
        kurtulmusbuyumeOranlari <- kurtulmusUlke$Deger
        olmusbuyumeOranlari  <-  olmusUlke$Deger
        ytitle <- "<b>Kişi Sayısı</b>"
        for(i in length(bulasmisUlke$Deger):1){
            if(i > 2)
            {
                bulasmisbuyumeOranlari[i] <- round(((bulasmisUlke$Deger[i]-bulasmisUlke$Deger[i-1]) / (bulasmisUlke$Deger[i-1] - bulasmisUlke$Deger[i-2])), digits = 2)
                kurtulmusbuyumeOranlari[i] <- round(((kurtulmusUlke$Deger[i]-kurtulmusUlke$Deger[i-1]) / (kurtulmusUlke$Deger[i-1] - kurtulmusUlke$Deger[i-2])), digits = 2)
                olmusbuyumeOranlari[i] <- round(((olmusUlke$Deger[i]-olmusUlke$Deger[i-1]) / (olmusUlke$Deger[i-1] - olmusUlke$Deger[i-2])), digits = 2)
            }
            else if(i==2)
            {
                bulasmisbuyumeOranlari[i] <- round(((bulasmisUlke$Deger[i]-bulasmisUlke$Deger[i-1]) / bulasmisUlke$Deger[i-1]), digits = 2)
                kurtulmusbuyumeOranlari[i] <- round(((kurtulmusUlke$Deger[i]-kurtulmusUlke$Deger[i-1]) / kurtulmusUlke$Deger[i-1]), digits = 2)
                olmusbuyumeOranlari[i] <- round(((olmusUlke$Deger[i]-olmusUlke$Deger[i-1]) / olmusUlke$Deger[i-1]), digits = 2)
            }
            else {
                bulasmisbuyumeOranlari[i] <- NaN
                kurtulmusbuyumeOranlari[i] <- NaN
                olmusbuyumeOranlari[i] <- NaN
            }
        }
        buyume <<- cbind.data.frame(dunyabazindabulasmis$Tarih, bulasmisbuyumeOranlari,kurtulmusbuyumeOranlari,olmusbuyumeOranlari)
        colnames(buyume) <<- c("Tarih", "Bulasmis", "Kurtulmus", "Olmus")
        p <- plot_ly(buyume, x=~parsedate::parse_iso_8601(buyume$Tarih), y=~buyume$Bulasmis, name ="Vaka Sayısının Büyüme Faktörü", type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~buyume$Kurtulmus, name = 'İyileşen Hasta Sayısının Büyüme Faktörü', mode = 'lines+markers') %>%
            add_trace(y = ~buyume$Olmus, name = 'Vefat Sayısının Büyüme Faktörü', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>",ulke, " Ülkesindeki Korona Virüsü Vakalarının Büyüme Faktörünün Zamansal Değişim Grafiği<b>", sep = ""), yaxis= list( title = "<b>Büyüme Faktörü</b>"), xaxis = list(title="<b>Tarih<b>")
            )
    })
    output$tablo <- renderDataTable({
        vanilla.table = datatable(sondurumlar, extensions = c('Responsive','Scroller'), rownames = FALSE, selection="single", escape=FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets="_all")), deferRender = TRUE, scrollY = 400, scroller = TRUE, language = list(info = "_TOTAL_ Girdiden _START_ ile _END_ arası gösteriliyor. ", sSearch="Arama")))
    })
    output$bulasmisSayi <- renderUI({
        ulke <- input$Ulke
        a <- sqldf(paste("SELECT `Vaka Sayısı` FROM sondurumlar WHERE Ülke ='", ulke,"'" ,sep=""))
        h1(br(), a[1] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$iyilesmisSayi <- renderUI({
        ulke <- input$Ulke
        a <- sqldf(paste("SELECT `İyileşmiş Hasta Sayısı` FROM sondurumlar WHERE Ülke ='", ulke,"'" ,sep=""))
        h1(br(), a[1] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$olmusSayi <- renderUI({
        ulke <- input$Ulke
        a <- sqldf(paste("SELECT `Vefat Sayısı` FROM sondurumlar WHERE Ülke ='", ulke,"'" ,sep=""))
        h1(br(), a[1] , "Kişi" ,align = "center", style = "color:blue")
    })
    output$aktifSayi <- renderUI({
        ulke <- input$Ulke
        a <- sqldf(paste("SELECT `Aktif Hasta Sayısı` FROM sondurumlar WHERE Ülke ='", ulke,"'" ,sep=""))
        h1(br(), a[1] , "Kişi" ,align = "center", style = "color:blue")
    })
    #Yas-Cinsiyet analizi ekrani
    output$yasAnalizi <- renderPlotly({
        t <- list(
            size = 11)
        fig <- plot_ly( x = yas$Yas, y = yas$Oran, type = "bar") %>%
            config(locale = 'tr') %>%
            layout(
                font= t , yaxis= list(overlaying = "y",  title = "<b>Vefat Oranı</b>", dtick =0.02 ,range = c(0.0,0.20) , tickformat='.1%'),
                xaxis = list(title="<b>Yaş Aralığı<b>")
            )
    })
    output$cinsiyetAnalizi <- renderPlotly({
        t <- list(
            size = 11)
        fig <- plot_ly( x = cinsiyet$Cinsiyet, y = cinsiyet$Oran, type = "bar") %>%
            config(locale = 'tr') %>%
            layout(
                font=t, yaxis= list(overlaying = "y",  title = "<b>Vefat Oranı</b>",dtick =0.005 ,range = c(0.0,0.05), tickformat='.1%'),
                xaxis = list(title="<b>Yaş Aralığı<b>")
            )
    })
    output$agirHastalik <- renderPlotly({
        t <- list(
            size = 11)
        fig <- plot_ly(x = str(agirhastalik$`Hastalık Sayısı`) , y=agirhastalik$Oran, type = "bar") %>%
            config(locale = 'tr') %>%
            layout(
                font=t, yaxis= list(  title = "<b>Hastalık Oranı</b>", dtick =0.05 ,range = c(0.0,0.5), tickformat='.1%'),
                xaxis = list(title="<b>Ağır Hastalık Sayısı<b>")
            )
    })
    output$sikayetler <- renderPlotly({
        t <- list(
            size = 11)
        fig <- plot_ly(x = sikayetler$Sikayet, y=sikayetler$Yuzde, type = "bar") %>%
            config(locale = 'tr') %>%
            layout(
                font=t, yaxis= list(  title = "<b>Hastalık Oranı</b>", dtick =0.00 ,range = c(0.0,1.0), tickformat='.1%'),
                xaxis = list(title="<b>Belirtiler<b>")
            )
    })
    output$hastalikTurleri <- renderDataTable({
        vanilla.table = datatable(hastalikturleri, extensions = c('Responsive','Scroller'), rownames = FALSE, selection="single", escape=FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets="_all")), deferRender = TRUE, scrollY = 400, scroller = TRUE, language = list(info = "_TOTAL_ Girdiden _START_ ile _END_ arası gösteriliyor. ", sSearch="Arama")))
    })
    #Turkiye Ekrani
    output$turkiyeGrafik <- renderPlotly({
        t <- list(
            size = 9)
        p <- plot_ly(turkiyebirlesik, x=~parsedate::parse_iso_8601(turkiyebirlesik$Tarih), y=~turkiyebirlesik$`Toplam Vaka Sayısı`, name ="Toplam Vaka Sayısı", type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~turkiyebirlesik$`Toplam İyileşen Hasta Sayısı`, name = 'Toplam İyileşen Hasta Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~turkiyebirlesik$`Toplam Vefat Sayısı`, name = 'Toplam Vefat Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~turkiyebirlesik$`Aktif Hasta Sayısı` , name = 'Aktif Hasta Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~turkiyebirlesik$`Toplam Test Sayısı`, name = 'Toplam Test Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~turkiyebirlesik$`Toplam Yoğun Bakım Hasta Sayısı`, name = 'Toplam Yoğun Bakım Hasta Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~turkiyebirlesik$`Toplam Entübe Hasta Sayısı`, name = 'Toplam Entübe Hasta Sayısı', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>Türkiyedeki Korona Virüsü Vakaları Zamansal Değişim Grafiği<b>", sep = ""), yaxis= list(overlaying = "y",  title = "<b>Kişi Sayısı</b>"),
                xaxis = list(title="<b>Tarih<b>")
            )
    })
    output$turkiyetablo <- renderDataTable({
        vanilla.table = datatable(turkiyebirlesik, extensions = c('Responsive','Scroller'), rownames = FALSE, selection="single", escape=FALSE, options = list(order = list(0, 'desc'),columnDefs = list(list(className = 'dt-center', targets="_all")), deferRender = TRUE, scrollY = 400, scroller = TRUE, language = list(info = "_TOTAL_ Girdiden _START_ ile _END_ arası gösteriliyor. ", sSearch="Arama")))
    })
    output$harita<-renderPlotly({
        p <- ggplot(final_ist_map, aes(long, lat, group = group))+
            geom_polygon(aes(fill = vaka_sayisi, text=paste("Şehir:",il, "\nVaka Sayısı: ", vaka_sayisi, "\nVefat Sayısı: ", vefat_sayisi)), color="white")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  plot.title = element_text(hjust = 0.5)
                  )+
            scale_fill_gradient(low = "blue", high = "red") +
            labs(title = "1 Nisan 2020 Tarihindeki Korona Vakalarının Türkiye Haritasındaki Dağılımı",y="", fill='Vaka Sayısı')
        p
        ggplotly(tooltip =c("text","size"))
    })
})
