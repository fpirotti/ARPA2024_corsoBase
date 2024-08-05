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

    filteredData <- reactive({
          req(input$sample)
         va <- terra::spatSample(TempModis, method=input$method,
                                 size=input$sampleN, na.rm=T, xy=T)
         names(va)<-c("lon", "lat", "value")
         va
    })

    output$map<-renderLeaflet({
        map@map
    })

    observe({
        data = filteredData()
        leafletProxy("map", data = data) %>%
            clearShapes() %>%
            clearMarkers() %>%
            leaflet::addCircleMarkers(radius = 4, weight = 1, color = "#ff0000",
                       fillColor = "black", group="Samples",  popup = ~paste(round(value,1))
            )
    } )
}
