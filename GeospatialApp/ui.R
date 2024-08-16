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

        sidebarMenu(
          menuItem("Map", tabName = "tmap", icon=icon("map")),
          menuItem("Stats", tabName = "stats", icon=icon("bar-chart")),
          menuItem("Neighbourhood", icon = icon("location"),
                   menuSubItem(tabName = "ripk", "Ripley's K"),
                   menuSubItem(tabName = "mori", "Moran's I"),
                   menuSubItem(tabName = "gfun", "G-function"),
                   menuSubItem(tabName = "ffun", "F-function"),
                   menuSubItem(tabName = "quada", "Quadrat analysis")
                   ),
          menuItem("Spat. Autocorrelation", icon = icon("line-chart"),
                   menuSubItem(tabName = "variogr", "Variogram"),
                   menuSubItem(tabName = "localmori", "Local Moran's I"),
                   menuSubItem(tabName = "geary", "Geary's C"),
                   menuSubItem(tabName = "lisa", "LISA (Local Indicators of Spatial Association)")
          ),
          menuItem("Spat. Regression", icon = icon("line-chart"),
                   menuSubItem(tabName = "lagmodel", "Spatial lag model"),
                   menuSubItem(tabName = "spatmodel", "Spatial error model"),
                   menuSubItem(tabName = "gsam", "Generalized spatial autocorrelation model (GSAM)")
          )
        )
    ),
    dashboardBody(
        tabItems(
          tabItem("stats",

                  box(width = 6,collapsible = T,
                      title = "Statistics: tables",
                      fluidRow(
                        column(width=6, shiny::selectInput("fitmChoice", "Fit function",
                                                           choices = list("No trends"="temp~1" ,
                                                                          "linear"="temp~x+y" ,
                                                                          "linear with height"="temp~lon+lat+altitude" ,
                                                                          "linear only height"="temp~altitude" ,
                                                                          "quadratic"="temp~x+y+I(x^2)+I(y^2)+lon*lat"  ,
                                                                          "power with height"="temp~x+y+altitude+I(x^2)+I(y^2)+x*y",
                                                                          "power only height"="temp~altitude+I(altitude^0.5)") )  )
                        ,column(width=6,  shiny::textInput("fitm", "Custom Function")  )
                      ),
                      fluidRow(
                        column(width=6, title="Altitude is in meters, temperature in °C,
to make the two units comparable,
we can scale meters to km
in altitude... try it!",
                                shiny::checkboxInput("scaleAltitude", "Scale x1000 Altitude",value = F)  )

                        ,column(width=6,  shinyWidgets::actionBttn("calc", "Calculate",
                                                                   style="simple", color = "success") )

                      ),
                      shinycssloaders::withSpinner(  htmlOutput("table") )
                  ),

                  box(width = 6,
                              title = "Statistics: plots"
                              ,shinycssloaders::withSpinner(plotOutput("plotSJ2") )
                              # ,shinycssloaders::withSpinner(plotOutput("plotSJ3") )
                          ),

                  box(width = 6,
                      title = "Statistics: plots"
                      ,shinycssloaders::withSpinner(plotOutput("plotSJ3") )
                      # ,shinycssloaders::withSpinner(plotOutput("plotSJ3") )
                  ),

                  box(width = 6,
                      title = "Statistics: plots"
                      ,shinycssloaders::withSpinner(plotOutput("plotSJ1") )
                      # ,shinycssloaders::withSpinner(plotOutput("plotSJ3") )
                  )

                  ),
            tabItem("tmap",

                    fluidRow(
                      column(width=3, shiny::numericInput("sampleN", "N. of samples", 1000)  )
                      ,column(width=6, shiny::selectInput("method", "Sample method", choices = c("regular" , "random")) )

                      ,column(width=3,
                              shinyWidgets::actionBttn("sample", "Sample", icon = icon("refresh"),
                                                       style="simple", inline=T,
                                                       color = "success")
                              )


                    ),
                    leafletOutput("mymap")
               ),

        tabItem("ripk",
                h3("Ripley's K"),
                h4("Ripley's K test determines if points are clustered, dispersed, or randomly distributed.
                   It considers density of point patterns. Does not consider the values at those points")
        ),

        tabItem("mori",
                h3("Moran's I test"),
                h4("A global spatial autocorrelation statistic used to measure the
                   spatial dependence between features in a dataset.
                   It assesses whether similar values are clustered together or dispersed..")
        ),

        tabItem("gfun",
                h3("G Function"),
                h4("Similar to the K-function of Ripley, but focuses on the number of points outside a given distance.")
        ),

        tabItem("gfun",
                h3("F-function"),
                h4("Compares the distribution of distances between pairs of points
                   in the observed pattern to that expected under CSR (Complete Spatial Randomness).")
        ),

        tabItem("quadana",
                h3("Quadrat analysis"),
                h4(" Divides the study area into quadrats and counts the number of points in each.")
        ),

        tabItem("variogr",
                h3("Variogram"),
                fluidRow(
                  column(width=6,
                         div(title="Choose the distance in meters of the variogram analysis",
                             numericInput("distance", "Distance of kernel", min = 1, value=1000) )
                         ),
                  column(width=6,
                         div(title="Choose angles with respect to north",
                             shinyWidgets::pickerInput("angles", "Angles", choices = list(
                              "0-90-180"   ,
                              "0-45-90"   ,
                              "0-23-45"
                             )
                            )
                           )
                         )
                ),
                plotOutput("variogramPlot")
        ),

        tabItem("localmori",
                h3("Local Moran's I"),
                h4("  Identifies local spatial clusters of high or low values.")
        ),

        tabItem("geary",
                h3("Geary's C"),
                h4(" Similar to Moran's I, but with a different weighting scheme.")
        ),

        tabItem("lisa",
                h3("LISA (Local Indicators of Spatial Association)"),
                h4(" A general framework for assessing local spatial patterns.")
        ),

        tabItem("lagmodel",
                h3("Spatial lag model"),
                h4("Includes a spatially lagged dependent variable to account for spatial autocorrelation.")
        ),

        tabItem("spatmodel",
                h3("Spatial error model"),
                h4("Introduces spatial autocorrelation in the error term")
        ),

        tabItem("gsam",
                h3("Generalized spatial autocorrelation model (GSAM)"),
                h4("Combines spatial lag and error models.")
        )
    )
  )
)
