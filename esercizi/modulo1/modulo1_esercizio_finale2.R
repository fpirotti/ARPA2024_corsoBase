## facciamo qualche grafico

load(file = "esercizi/modulo1/modulo1esercizio1.rda")

## creare un grafico con andamento valore temp. medio u
ggplot(meteo.df) + geom_line(aes(x=dataora, y=tempMedia))


## divido colore per stazione
ggplot(meteo.df) + geom_line(aes(x=dataora, y=tempMedia, color=nome_stazione))


## divido   per facet
ggplot(meteo.df) + geom_line(aes(x=dataora, y=tempMedia)) +
  facet_wrap(vars(nome_stazione))


## aggiungi min max usn
ggplot(meteo.df) + geom_line(aes(x=dataora, y=tempMedia)) +
  geom_ribbon(aes(x=dataora, y=tempMedia,
                  ymin=tempMin, ymax=tempMax), fill="#ff000044", color="#ff000044") +
  facet_wrap(vars(nome_stazione))


## stampate un PNG ed un PDF come nelle slides
##  e salvate nei file seguenti
# es3_cognome_nome.png
# es3_cognome_nome300dpi.png
# es3_cognome_nome.pdf
