## usiamo un iteratore per richiedere le temperature di
## tutte le stazioni ARPAV per l'anno 2014!
##

# TEMPERATURA giornaliera anno 2020 ----
## prepariamo la stringa per l'API ----
# stringa di base: %s verrà sostituito con una stringa di testo (il codice della stazione),
# %d con un intero (l'anno) - vedi https://stat.ethz.ch/R-manual/R-devel/library/base/html/sprintf.html
baseurl <- "https://api.arpa.veneto.it/REST/v1/meteo_storici_tabella?codseq=%s&anno=%d"


url.stz <- "https://api.arpa.veneto.it/REST/v1/meteo_meteogrammi?rete=MGRAMMI&coordcd=18&orario=0&rnd=0.9966091913301682"
anno <- 2023

## ...da recap Bressan usiamo flatten
stazioni.meteo <- jsonlite::fromJSON(url.stz, flatten = TRUE)$data

## iterazione (vedi slide 40 parte 1)
## può essere usata anche per fare chiamate alla rete
meteo <- stazioni.meteo$codseqst %>% map(function(IDstazione){
        url <- sprintf(baseurl, IDstazione, anno)
        meteo <- jsonlite::fromJSON(url, flatten = TRUE)$data
        meteo
    })

## pulizia tabella per mantenere solo temperatura,
## diversi passaggi...


## raccogliamo i risultati in una tabella ----
# ............
# ............
# ............
meteo.df <- meteo %>%
  bind_rows()

## modifichiamo il tipo di colonne dando classi adeguate ----
# ............
# ............
# ............
meteo.df <- meteo %>%
  bind_rows()  %>%
  ## qui converto nome stazione a factor e colonna dataora a classe adatta
  mutate(nome_stazione=as.factor(nome_stazione),
         dataora=as.POSIXct(dataora))



## filtriamo solo temperatura! ----
# ............
# ............
# ............
meteo.df <- meteo %>%
  bind_rows()  %>%
  ## qui converto nome stazione a factor e colonna dataora a classe adatta
  mutate(nome_stazione=as.factor(nome_stazione),
         nome_sensore=as.factor(nome_sensore),
         dataora=as.POSIXct(dataora))  %>%
  # filter(nome_sensore == "Temperatura aria a 2m") %>%
  # grepl("Temperatura", nome_sensore ) ... grepl restituisce T/F se la stringa
  # è contenuta in quella del secondo argomento
  filter( grepl("Temperatura", nome_sensore ) )


## isoliamo il valore di temperatura media ----
# ............
# ............
# ............
meteo.df <- meteo %>%
  bind_rows()  %>%
  ## qui converto nome stazione a factor e colonna dataora a classe adatta
  mutate(nome_stazione=as.factor(nome_stazione),
         nome_sensore=as.factor(nome_sensore),
         dataora=as.POSIXct(dataora))  %>%
  # filtriamo solo temperatura!
  # filter(nome_sensore == "Temperatura aria a 2m") %>%
  # grepl("Temperatura", nome_sensore ) ... grepl restituisce T/F se la stringa
  # è contenuta in quella
  filter( grepl("Temperatura", nome_sensore ) ) %>%
  # i dati sono
  mutate( temp= map(valore, function(x){ jsonlite::fromJSON(x, flatten = TRUE)$MEDIO  }) %>% unlist()   )
  ## il seguente non funziona come mai?
  # mutate( temperaturaMedia= jsonlite::fromJSON(valore) )


## tutto insieme isolando le colonne che ci interessano ----
# ............
# ............
# ............
meteo.df <- meteo %>%
  bind_rows()  %>%
  ## qui prima filtro, provate a vedere la differenza (factor stazione)
  filter( grepl("Temperatura", nome_sensore ) ) %>%
  ## qui converto nome stazione a factor e colonna dataora a classe adatta
  mutate(nome_stazione=as.factor(nome_stazione),
         nome_sensore=as.factor(nome_sensore),
         dataora=as.POSIXct(dataora))  %>%
  # i dati sono in json... estrai minima media e massima
  mutate( tempMedia= map(valore, function(x){ jsonlite::fromJSON(x)$MEDIO })  %>% unlist() ) %>%
  mutate( tempMin= map(valore, function(x){ jsonlite::fromJSON(x)$MINIMO }) %>% unlist() ) %>%
  mutate( tempMax= map(valore, function(x){ jsonlite::fromJSON(x)$MASSIMO }) %>% unlist() ) %>%
  ## il seguente non funziona come mai?
  #mutate( temperaturaMedia= jsonlite::fromJSON(valore) )
  select( codice_stazione, nome_stazione, tempMedia,tempMin, tempMax, dataora, tipo )
# mutate( temperaturaMedia= map(valore, jsonlite::fromJSON) )

# ultimo passaggio - associamo latitudine e longitudine ----
# ............
# ............
# ............
meteo.df.sp <- meteo.df %>% left_join(stazioni.meteo %>% select(codice_stazione, latitudine, longitudine) )

# CHALLENGE   - sareste capaci di estrarre per più anni
# la serie temporale per le stazioni e unirla in una singola tabella?
#

#
#
#
# CONTINUAZIONE - grafici - modulo1_esercizio_finale2.R
save(meteo,meteo.df, file = "esercizi/modulo1/modulo1esercizio1.rda")
