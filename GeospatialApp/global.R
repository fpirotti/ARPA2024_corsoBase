library(leaflet)
library(shiny)
library(shinydashboard)
library(mapview)

TempModis <- terra::rast("../esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")


map <- mapview(TempModis)
