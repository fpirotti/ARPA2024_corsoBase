#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


dashboardPage(skin = "black",
    dashboardHeader(title = "Exploratory spatial data analysis (ESDA)", titleWidth = 450),
    dashboardSidebar(
         shiny::actionButton("sample", "Sample Points") ,
         shiny::selectInput("method", "Method", choices = c("regular" , "random")),

        shiny::numericInput("sampleN", "Number of samples", 100) ,
        sliderInput("dir", "Transect Direction:", 1, 360, 1)

    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(
                title = "Mappa",
                leafletOutput("map")
            ),

            box(
                title = "GeoTools",

            )
        )
    )
)
