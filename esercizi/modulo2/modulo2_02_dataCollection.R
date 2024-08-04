###############  review some functions to read csv, many possible variants -----

# built-in, predefined with "base" config, no packages needed
read.table('./dati/Prec_mensili_2020.csv', header=TRUE, sep=";")
read.delim('./dati/Prec_mensili_2020.csv', sep=";")
read.csv('./dati/Prec_mensili_2020.csv', sep = ";")
read.csv2('./dati/Prec_mensili_2020.csv')

# tidyverse approach
# here note the use of :: for accessing the function, read the online manual
# note that "::" it is a function itself
readr::read_csv2('./dati/Prec_mensili_2020.csv')

# almost the same results, to note the differences in the use of arguments
# and the output formats (dataframe or tibble, names of columns,...)

###############  read met data in json format from API ARPAV -----

# define the url
met_url <- "https://api.arpa.veneto.it/REST/v1/meteo_meteogrammi?rete=MGRAMMI&coordcd=18&orario=0&rnd=0.9966091913301682"

# get the data with the use of an appropriate function from library jsonlite
# pay attention: it returns a list (of lists)!
met_lst<- jsonlite::read_json(met_url)

# assign to a variable just the "data" part
# and check that it is still a list!
met_dat <- met_lst$data

library(tidyverse)

# transform it to a dataframe
# read in the manual, this approach is somehow considered "superseded"
met_df <- met_dat %>%
  map_df(.f = function(x){ x })

# a "new" variant but with exactly the same results
met_df <- met_dat %>%
  map(.f = function(x){ x }) %>%
  bind_rows() # concatenate rows

###############  a much shorter alternative for reading json into a dataframe -----
# here it is, in two lines

# read and flatten results
met_lst <- jsonlite::fromJSON(met_url, flatten = TRUE) # the key is the arument flatten
# convert to df
met_df <- as.data.frame(met_lst$data)

# it seems to work pretty well, saving a lot of coding!

###############  some basic data manipulation with tidyverse -----

# by first check some variable classes
map(.x = met_df, .f = function(x){ class(x) }) %>%
  bind_rows() %>%
  select(valore, tipo, provincia)

# to note "valore" is a charachter, as many others vars
# mutate the class of the vars to be later properly manipulated

met_df <- met_df %>%
  mutate(valore = as.numeric(valore),
         tipo = factor(tipo),
         provincia = factor(provincia))


# use of the tidyverse verbs

met_df %>%
  filter(provincia =="TREVISO") %>% # filter data for tv
  summarise(recs = sum(!is.na(valore)), # note this
            mean = mean(valore, na.rm = TRUE),
            median = median(valore, na.rm = TRUE),
            min = min(valore, na.rm = TRUE),
            max = max(valore, na.rm = TRUE),
            pct98 = quantile(valore, 0.98, na.rm = TRUE),
            sd = sd(valore, na.rm = TRUE))

# stratification (aggregation) of results by "provincia"
met_df %>%
  group_by(provincia) %>%
  summarise(recs = sum(!is.na(valore)), # note this
            mean = mean(valore, na.rm = TRUE),
            median = median(valore, na.rm = TRUE),
            min = min(valore, na.rm = TRUE),
            max = max(valore, na.rm = TRUE),
            pct98 = quantile(valore, 0.98, na.rm = TRUE),
            sd = sd(valore, na.rm = TRUE))

# stratification (aggregation) of results by "provincia" and "tipo"
met_df %>%
  group_by(provincia, tipo) %>%
  summarise(recs = sum(!is.na(valore)),
            mean = mean(valore, na.rm = TRUE),
            median = median(valore, na.rm = TRUE),
            min = min(valore, na.rm = TRUE),
            max = max(valore, na.rm = TRUE),
            pct98 = quantile(valore, 0.98, na.rm = TRUE),
            sd = sd(valore, na.rm = TRUE))

met_df %>%
  group_by(provincia, tipo) %>%
  summarise(recs = sum(!is.na(valore)),
            mean = mean(valore, na.rm = TRUE),
            min = min(valore, na.rm = TRUE),
            max = max(valore, na.rm = TRUE),
            pct98 = quantile(valore, 0.98, na.rm = TRUE))

# Q:
# prova a filtrare, stratificare  e riassumere i risultati sulla base di altri criteri, "a piacere"

# Q:
# come posso a calcolare una nuova variabile "t_norm" con temperatura "normalizzata" sulla media globale?

met_df %>%
  mutate(t_norm = valore / mean(valore, na.rm=TRUE)) %>%
  select(valore, t_norm)

# Q:
# calcola il valore medio di tutte le osservazioni della variabile "valore"
# ed assegna il risultato ad una variabile vettore di lunghezza 1.
# Proponi una soluzione nel modo tradizionale ed una nel modo "tidyverse"

m1 <- mean(met_df$valore, na.rm = TRUE)

# this is not exactly the same as above
met_df %>% summarise(m = mean(valore, na.rm=TRUE)) -> m2

# you need to pull out the value from the data structure
met_df %>% summarise(m = mean(valore, na.rm=TRUE)) %>% pull() -> m2

# check it
m1 == m2

met_df %>%
  mutate(t_norm = valore / m1) %>%
  select(valore, t_norm)

############ anticipiamo alcune funzioni  ------
# che introducono la trattazione e la visualizzazione
# degli attributi geografici di un oggetto (dataframe)
# utile per il successivo modulo su kriging & Co.

# create a new object (a shallow copy)
# it's just not to mix up things
# but not stricltly necessary
met_sp <- met_df

library(sp)

# set coordinates
# yes, it's a weird syntax...
coordinates(met_sp) <- ~longitudine+latitudine

# set coordinate reference system for mapping
proj4string(met_sp) <- CRS("epsg:4326")
# for reference about epsg
# https://spatialreference.org/ref/epsg/
# https://epsg.io/

# check if not and eventually install mapview package
if(!require(mapview)){
  install.packages("mapview")
  library(mapview)
}

# see the interactive map in viewer
# explore it: click, zoom, drag and change the background
mapview(met_sp)

mapview(met_sp,
        zcol="valore",
        layer.name = "temp")

# new modern approach of simple feature
# compliant with tidyverse syntax
# by the same author as "sp" package

library(sf)

# transform to sf object
met_sf <- st_as_sf(met_df,
                   coords = c("longitudine", "latitudine"),
                   crs = 4326)

# a static map
met_sf %>%
  ggplot()+
  geom_sf(aes(colour=valore))+
  geom_sf_text(aes(label=valore, colour = valore), # map colour and valore
               size=2.5,
               nudge_y = 0.02)+
  theme_void()

# a dynamic map
mapview::mapview(met_sf)

mapview::mapview(met_sf %>% sf::st_transform(32632),
                 zcol="valore",
                 layer.name = "temp")

