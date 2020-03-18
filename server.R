shinyServer(function(input, output) {
    #Dunya Bazinda Ekrani
    output$dunyaGeneli <- renderPlotly({
        t <- list(
            size = 9)
        p <- plot_ly(dunyabazindabulasmis, x=~parsedate::parse_iso_8601(dunyabazindabulasmis$Tarih), y=~dunyabazindabulasmis$Deger, name ="Vaka Sayısı", type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~dunyabazindakurtulmus$Deger, name = 'İyileşen Hasta Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~dunyabazindaolmus$Deger, name = 'Ölü Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~dunyabazindaaktif$Deger, name = 'Aktif Hasta Sayısı', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>Dünya Genelindeki Korona Virüsü Vakaları Zamansal Değişim Grafiği<b>", sep = ""), yaxis= list(overlaying = "y",  title = "<b>Kişi Sayısı</b>", tickformat=',d'),
                xaxis = list(title="<b> <br>Tarih<b>")
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
        oznitelik <- ""
        if(input$oznitelik == "Vaka Sayısı")
            oznitelik <- "bulasmis"
        else if(input$oznitelik == "İyileşen Hasta Sayısı")
            oznitelik <- "kurtulmus"
        else if(input$oznitelik == "Ölü Sayısı")
            oznitelik <- "olmus"
        else if(input$oznitelik == "Aktif Hasta Sayısı")
            oznitelik <- "aktif"
        birinci_data <- sqldf(paste("SELECT Tarih, Deger FROM ", oznitelik ," WHERE Ulke = '", ulke1 ,"'" ,sep=""))
        ikinci_data <- sqldf(paste("SELECT Tarih, Deger FROM ", oznitelik ," WHERE Ulke = '", ulke2 ,"'" ,sep=""))
        ucuncu_data <- sqldf(paste("SELECT Tarih, Deger FROM ", oznitelik ," WHERE Ulke = '", ulke3 ,"'" ,sep=""))
        p <- plot_ly(birinci_data, x=~parsedate::parse_iso_8601(birinci_data$Tarih), y=~birinci_data$Deger, name = ulke1, type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~ikinci_data$Deger, name = ulke2 , mode = 'lines+markers') %>%
            add_trace(y = ~ucuncu_data$Deger, name = ulke3, mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>", ulke1, " & ", ulke2, " & ", ulke3, " Ülkelerinin ", input$oznitelik, "na Göre Kıyas Grafiği <b>", sep = ""), yaxis= list( title = "<b>Kişi Sayısı</b>"),
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
        gun <- 0
        if(input$oznitelik == "Vaka Sayısı")
            oznitelik <- "bulasmis"
        else if(input$oznitelik2 == "İyileşen Hasta Sayısı")
            oznitelik <- "kurtulmus"
        else if(input$oznitelik2 == "Ölü Sayısı")
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
        birinci_data <<- sqldf(paste("SELECT Deger FROM ", oznitelik ," WHERE Ulke = '", ulke1, "'  AND Tarih>= '",as.Date(input$date1),"' AND Tarih < '", as.Date(input$date1) + gun, "'",sep=""))$Deger[1:gun]
        ikinci_data <<- sqldf(paste("SELECT Deger FROM ", oznitelik ," WHERE Ulke = '", ulke2, "'  AND Tarih>='",as.Date(input$date2),"' AND Tarih < '", as.Date(input$date2) + gun, "'",sep=""))$Deger[1:gun]
        ucuncu_data <<- sqldf(paste("SELECT Deger FROM ", oznitelik ," WHERE Ulke = '", ulke3, "' AND Tarih>='",as.Date(input$date3),"' AND Tarih < '", as.Date(input$date3) + gun, "'",sep=""))$Deger[1:gun]
        veriler <<- cbind.data.frame(seq(1,gun,1), birinci_data, ikinci_data, ucuncu_data)
        colnames(veriler) <<- c("Gun", "Ulke1", "Ulke2","Ulke3")
        p <- plot_ly(veriler, x=veriler$Gun, y=~veriler$Ulke1, name = ulke1, type = "scatter", mode="lines+markers") %>%
            add_trace(y = ~veriler$Ulke2, name = ulke2 , mode = 'lines+markers') %>%
            add_trace(y = ~veriler$Ulke3, name = ulke3, mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>", ulke1, " & ", ulke2, " & ", ulke3, " Ülkelerinin ", input$oznitelik, "na Göre Kıyas Grafiği <b>", sep = ""), yaxis= list( title = "<b>Kişi Sayısı</b>"),
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
            add_trace(y = ~olmusUlke$Deger, name = 'Ölü Sayısı', mode = 'lines+markers') %>%
            add_trace(y = ~aktifUlke$Deger, name = 'Aktif Hasta Sayısı', mode = 'lines+markers') %>%
            config(locale = 'tr') %>%
            layout(
                legend = list(x = 0.1, y = 0.9), font=t, title = paste("<b>",ulke, " Ülkesindeki Korona Virüsü Vakalarının Zamansal Değişim Grafiği <b>", sep = ""), yaxis= list( title = "<b>Kişi Sayısı</b>"),
                xaxis = list(title="<b>Tarih<b>")
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
        a <- sqldf(paste("SELECT `Ölü Sayısı` FROM sondurumlar WHERE Ülke ='", ulke,"'" ,sep=""))
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
                font= t , yaxis= list(overlaying = "y",  title = "<b>Ölüm Oranı</b>", dtick =0.02 ,range = c(0.0,0.20) , tickformat='.1%'),
                xaxis = list(title="<b>Yaş Aralığı<b>")
            )
    })
    output$cinsiyetAnalizi <- renderPlotly({
        t <- list(
            size = 11)
        fig <- plot_ly( x = cinsiyet$Cinsiyet, y = cinsiyet$Oran, type = "bar") %>%
            config(locale = 'tr') %>%
            layout(
                font=t, yaxis= list(overlaying = "y",  title = "<b>Ölüm Oranı</b>",dtick =0.005 ,range = c(0.0,0.05), tickformat='.1%'),
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
})
