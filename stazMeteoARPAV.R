library(tidyverse)

api.url <- "https://api.arpa.veneto.it/REST/v1/meteo_meteogrammi?rete=MGRAMMI&coordcd=18&orario=0&rnd=0.9966091913301682"
stazioni.meteo <- jsonlite::read_json(api.url)
dati <- stazioni.meteo$data
length(dati)

for(i in 1:length(dati)){
  print(dati[[i]])
 # break
  # next
  print("ciao")
}


funz <- function(x){
  # browser()
  x$nome_stazione
  # length(x)
}

lapply(dati, funz)

dtf <- map_df(.x=dati, .f= function(x){ x } )

map(.x = dtf, .f = function(x){ class(x) } )

dtf$valore <- as.numeric(dtf$valore)

# as.numeric("D")

dtf$tipo <- as.factor(dtf$tipo)





dtf.belluno <- dtf %>% filter( provincia =="BELLUNO")

dtf %>% mutate(valore2=valore^2)  %>% select(valore, valore2)

dtf %>% mutate(valore2=valore^2, valore3=valore^3)  %>% select(valore, valore2, valore3)

dtf %>% group_by(tipo)  %>% summarise(valoreMedio=mean(valore, na.rm = TRUE),
                                      valoreSD=sd(valore, na.rm = TRUE) )



