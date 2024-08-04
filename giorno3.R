library(tidyverse)
library(ggplot2)

api.url <- "https://api.arpa.veneto.it/REST/v1/meteo_meteogrammi?rete=MGRAMMI&coordcd=18&orario=0&rnd=0.9966091913301682"

stazioni.meteo <- jsonlite::fromJSON(api.url, flatten = TRUE)

dtf <- stazioni.meteo$data

dt.past <- read.csv("dati/ARPA_temperatura_20240528.csv")
dt.past <- dt.past %>% select(statnm, statcd, dataora, media)






dt.joined <- dt.past %>%  inner_join(dtf, by=c("statcd"="codice_stazione") ) %>%
                          inner_join(dtf2, by=c("statcd"="codice_stazione") )

dt.joined2 <- inner_join(
                          inner_join(dt.past, dtf, by=c("statcd"="codice_stazione") ),
                         dtf2,
                         by=c("statcd"="codice_stazione") )




dtf <- dtf %>% mutate(valore=as.numeric(valore))


classi <- cut(dtf$valore, breaks = c(-10,0,10,20,30))
levels(classi)

tb.wide <- table(dtf$provincia, classi)
tb.wide <- as.data.frame.matrix(tb.wide)

tb.wide <- tb.wide %>%
              mutate(Provincie = rownames(tb.wide)) %>%
              relocate(Provincie)



tb.long <- tb.wide  %>%
  pivot_longer(!Provincie,
               names_to = "Classe Temp. °C",
               values_to = "Numero"
                )
tb.long


tb.long2wide <- tb.long  %>%
  pivot_wider( names_from = "Classe Temp. °C",
               values_from = "Numero"
            )
tb.long2wide
