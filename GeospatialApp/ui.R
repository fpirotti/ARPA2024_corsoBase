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

          div(width=6, shiny::selectInput("fitmChoice", "Fit function",
                                          choices = list(
                                            "linear"="Temperature~X+Y" ,
                                            "No trend"="Temperature~1" ,
                                            "linear with height"="Temperature~X+Y+Altitude" ,
                                            "linear only height"="Temperature~Altitude" ,
                                            "quadratic"="Temperature~X+Y+I(X^2)+I(Y^2)+X*Y"  ) )  ),
          div(title="You can write your own function using R formula syntax",
               shiny::textInput("fitm", "Custom Function")  ),

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
                        column(width=6, title="Altitude is in meters, temperature in °C,
to make the two units comparable,
we can scale meters to km
in Altitude... try it!",
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
                box( width=3, collapsible = T,
                     title = "Params",
                  # column(width=3,
                         div(title="Choose the attribute",
                             selectInput("attribute", "Attribute", choices = c(NULL, "---Please sample the area") ) )
                  # ),
                  # column(width=2,
                         ,div(title="Choose the width of subsequent distance intervals
 into which data point pairs are grouped for semivariance estimates",
                             numericInput("distance", "Kernel distance", min = 1, value=5000) )
                  #        ),
                  ,div(title="Choose the spatial separation distance up to
which point pairs are included in semivariance estimates;
as a default, the length of the diagonal of the box spanning the data is
divided by three.",
                       numericInput("cutoff", "Cutoff distance", min = 1, step = 1,
                                    value=1000) )
                  #        ),
                  # column(width=4,
                         ,div(title="Choose angles with respect to north, don't choose any for generic variogram",
                              selectizeInput("angles","Enter Angles",
                                             choices=c("", 0,22.5,45,90,135) ,
                                             options = list(create=TRUE))

                          )
                  #        ),
                  # column(width=3,
                         ,div(title="Should a variogram cloud be plotted instead of the empirical variogram?
The variogram cloud plot is a raw visualization of the semi-variance values calculated
for all possible pairs of data points within a specified distance.
It's a scatterplot where:
X-axis: Represents the distance between pairs of points.
Y-axis: Represents the calculated semi-variance for each pair.",
                             shinyWidgets::radioGroupButtons("variogramCloud",
                                                              "Cloud or empirical",
                                                             choices =
                                                                c("Empirical", "Cloud" )  )
                         )
                  ,div(title="compute covariogram or  variogram",
                       shinyWidgets::radioGroupButtons("covariogram",
                                                       "Variogram or Covariogram",
                                                       choices =
                                                         c("Variogram", "Covariogram" )  )
                  )

                  ,div(title="model type",
                       shinyWidgets::pickerInput("modelVariogram",
                                                       "Model",
                                                       choices = setNames(vgm()[,1], vgm()[,2])  )
                  )
                ),
               box(width=9,collapsible = T,
                   title = "Semivariogram/Covariogram",
                   plotOutput("variogramPlot") ),
               box(width=12,collapsible = T,
                   title = "Semivariogram/Covariance Surface",
                   plotOutput("variogramPlotMap") )
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
