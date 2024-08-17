#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$table<-shiny::renderUI({
      shiny::HTML("<b>Table with regression results</b>")
    })


    filteredData <- reactive({
         req(input$sample)
         va <- terra::spatSample(TempModis, method=input$method,
                                 size=input$sampleN, na.rm=T, xy=T)
         names(va)<-c("X", "Y", "Temperature")
         q <- terra::extract(TempDEM, va[,1:2], ID=F )
         va$Altitude<- q$VenetoDEM


         updateSelectInput(inputId = "attribute", choices = names(va) )
         data<- na.omit(va)
         xy <- data[,c("X","Y")]
         data4326 <- sf::st_as_sf(data, coords=c("X","Y") )

         st_crs(data4326) <- st_crs(4326)

         data3035 <- data4326 %>% st_transform(3035)
         bb<-sf::st_bbox(data3035)

         diag <- ((bb[["xmax"]]-bb[["xmin"]])^2 +
                  (bb[["ymax"]]-bb[["ymin"]])^2)^0.5
         updateNumericInput(inputId = "cutoff",
                            value= as.integer(diag/3) )


         # data3035 <- sf::as_Spatial(data3035)
         data3035 <- cbind( data3035, st_coordinates(data3035) )

         data <- list("3035"=data3035,
              "4326"=data4326)

         data


    })

    output$mymap<-renderLeaflet({
         map.map
    })



    output$variogramPlot<-renderPlot({
      req(input$attribute)
      data = filteredData()

       data<- data[["3035"]]
      formula <- as.formula(input$fitm)

      print(as.numeric(input$angles))
      if(shiny::isTruthy(input$angles) ){
        dir.vgm<- variogram(object=formula,
                            data=data,
                            width=input$distance,
                            cutoff = input$cutoff,
                            alpha = as.numeric(input$angles),
                            covariogram=input$covariogram=="Covariogram",
                            cloud=input$variogramCloud=="Cloud")
      } else {
        dir.vgm<- variogram(object=formula,
                            data=data,
                            width=input$distance,
                            cutoff = input$cutoff,

                            covariogram=input$covariogram=="Covariogram",
                            cloud=input$variogramCloud=="Cloud")
      }

      if(input$variogramCloud=="Cloud" || input$covariogram=="Covariogram"){
          plot(dir.vgm)
      } else {
        t.fit<-tryCatch({
          t.fit<-fit.variogram(dir.vgm, vgm(model=input$modelVariogram));

        },
        error=function(e){
          shinyWidgets::show_alert("Error", text=e$message)
        },
        warning=function(w){
          shinyWidgets::show_alert("Warning", text=w$message)
        })


        if(is.element("variogramModel", class(t.fit) ) ) {
          plot(dir.vgm, t.fit)
        } else {
          plot(dir.vgm)
        }
      }

            # use when don't have estimate of range, sill




    })

    output$variogramPlotMap<-renderPlot({
      req(input$attribute)
      data = filteredData()

      data<- data[["3035"]]
      formula <- as.formula(input$fitm)

      t.vgm3<- variogram(object=formula,
                          data=data,
                          width=input$distance,
                          map=TRUE,
                          cutoff = input$cutoff )
      mm<-terra::rast(t.vgm3$map)
      names(mm)<-c("Semivariances", "Number of point pairs")
      #cutoff approx = range,  width = width of bins
      if(shiny::isTruthy(input$angles)){
        plot(mm)
      } else {
        plot(t.vgm3)
      }

      })

   observeEvent(input$fitmChoice, {
     # browser()
     shiny::updateTextInput(inputId = "fitm", value=input$fitmChoice)
   })


    observe({
      data = filteredData()
      leafletProxy("mymap", data = data[["4326"]]) %>%
        clearShapes() %>%
        clearMarkers() %>%
        leaflet::addCircleMarkers(radius = 4, weight = 1, color = "#ff0000",
                                  fillColor = "black", group="Samples",
                                  # lng = ~x, lat = ~y,
                                  popup = ~paste("Temp:", round(Temperature,1),
                                                 "<br>Altitude:", round(Altitude))
        )
    })


    stazionarietà <- function(spdf){

"
Semivariogramma: Calcolare il semivariogramma per valutare l'autocorrelazione spaziale.
      Se il semivariogramma mostra un modello chiaro (ad esempio, non un modello sferico),
      potrebbe indicare una non stazionarietà.
Moran I: Questa statistica misura l'autocorrelazione spaziale globale.
      Pur non essendo un test diretto per la stazionarietà, può fornire
      indicazioni sulla struttura spaziale dei dati."

      coordinates(spdf)<- ~ lon+lat
      spdf.3035 <- sf::st_as_sf(spdf)
      st_crs(spdf.3035) <- st_crs(4326)
      spdf.3035 <- spdf.3035 %>% st_transform(3035)

      nb <- spdep::tri2nb(spdf.3035)
      ### equal weight for all
      lw <- nb2listw(nb, style="W", zero.policy=TRUE)

      inc.lag <- lag.listw(lw, spdf$Temperatureerature)
      inc.lag
      plot(inc.lag ~ spdf$Temperatureerature, pch=16, asp=1)
      M1 <- lm(inc.lag ~ spdf$Temperatureerature)
      abline(M1, col="blue")

      moran.temp<-moran.test(spdf.3035$Temperature, lw)
      moran.temp.mc<-  moran.mc(spdf.3035$Temperature, lw, nsim=999)
      plot(moran.temp.mc)

      sprintf(
        "Il coefficiente I di Moran è pari a %.3f. La pendenza positiva
        (verso l'alto) suggerisce che all'aumentare del valore della temperatura
        di un determinato punto, aumentano anche quelli dei punti vicini.
        Se l'inclinazione fosse negativa (ossia in discesa),
        ciò suggerirebbe una relazione negativa per cui i valori
        crescenti di un determinato punto  sarebbero circondati da punti
        con valori di temperatura decrescenti.",
      coef(M1)[2])

      veneto.proj <- st_transform(veneto, crs = 3035)

      veneto.owin <- as.owin(veneto.proj)

      ppp <- spatstat.geom::ppp(st_coordinates(spdf.3035)[,1],
                         st_coordinates(spdf.3035)[,2],
                         window = veneto.owin)


      attributes(ppp)
      par(pty = "s", mfrow = c(2, 2))
      plot(ppp, boundary = T)
      Lhat(ppp, maxdist = 5)
      Lenv(ppp, 25, process = "binomial", maxdist = 5)
      Lhat(ppp, maxdist = 1.5)
      Lenv(ppp, 100, process = "Strauss", maxdist = 1.5,
           cpar = 0.2, radius = 0.7)


      sprintf("La statistica I di Moran è  %.3f (lo stesso valore
              calcolato con la funzione moran, come previsto).
              Il valore p è %.2e,  molto piccolo. Di solito,
              quando il valore p è molto piccolo,
              è prassi comune riportarlo come < 0,001.",
              moran.temp$estimate[[1]], moran.temp$p.value )

      v <- variogram(temp ~ 1, data = spdf)
      plot(v)
      nb2listw(spdf, style="W")
      moran.test(spdf$Temperature)
    }



    observeEvent({input$calc
      input$scaleAltitude },
                 {


        data = filteredData()
        data <- data[["3035"]]
        formula <- input$fitm
        if(input$scaleAltitude){
          formula <- gsub("Altitude", "I(Altitude/1000)", formula)
        }
        print(formula)
        m.fit<- tryCatch(

          lm(data=data, formula)
          )
        if( is.element("error", class(m.fit)) ){
          shinyWidgets::show_alert("Errore", m.fit$message, type = "danger")
        }

        output$plotSJ1 <- shiny::renderPlot({
          # print(input$plotType)
          input$scaleAltitude
          plot_model(m.fit, type = "est", show.intercept = F,
                     show.values = TRUE,
                     show.p = TRUE, show.legend = TRUE  )
        },res = 100)

        output$plotSJ2 <- shiny::renderPlot({
          ggplot( ) +
            ggtitle( eval(m.fit$call[[2]])) +
            geom_point( aes(y=m.fit$fitted.values,
                            x=m.fit$model$Temperature) ) + ylab("Predicted (°C)") + xlab("Observed (°C)") + theme_bw()
        },res = 100)

        output$plotSJ3 <- shiny::renderPlot({

          ggplot( ) +
            ggtitle(""  ) +
            geom_point( aes(y=data$Temperature,
                            x=data$Altitude/ifelse(input$scaleAltitude,1000,1) ) ) +
            geom_line( aes(y=m.fit$fitted.values,
                            x=data$Altitude/ifelse(input$scaleAltitude,1000,1) ) ) +
            ylab("Temperature (°C)") +
            xlab( ifelse(input$scaleAltitude, "Altitude (km)", "Altitude (m)")) +
            theme_bw()
        },res = 100)

        # output$plotSJ3 <- shiny::renderPlot({
        #   plot_model(m.fit, type = "pred", terms = c("lat") )
        # },res = 100)

        output$table <- shiny::renderUI({
          req(input$fitm)
          dd<-sjPlot::tab_model(m.fit)
          shiny::HTML(dd$knitr)
        })


    },ignoreInit = T )
}
