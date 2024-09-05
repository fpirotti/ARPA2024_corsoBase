## facciamo qualche grafico

load(file = "esercizi/modulo1/modulo1esercizio1.rda")

## creare un grafico con andamento valore temp. medio u
ggplot(meteo.df, aes(x=dataora, y=tempMedia)) +
  geom_line() +
  geom_point(  )


## divido colore per stazione
ggplot(meteo.df) +
  geom_line(
    aes(x=dataora, y=tempMedia,
        color=nome_stazione))


## divido   per facet
ggplot(meteo.df) + geom_line(aes(x=dataora, y=tempMedia)) +
  facet_wrap(vars(nome_stazione))


## aggiungi min max usn
p<-ggplot(meteo.df) +
  geom_line(aes(x=dataora, y=tempMedia)) +
  geom_ribbon(aes(x=dataora, y=tempMedia,
                  ymin=tempMin, ymax=tempMax),
              fill="#ff000044", color="#ff000044") +
  facet_wrap(vars(nome_stazione))

png("fileGrafico100dpi.png", width=2000, height=2000, res=100)
  print(p)
dev.off()

pdf("fileGrafico.pdf")
print(p)
dev.off()
## stampate un PNG ed un PDF come da indicazioni nelle slides del corso
##  e salvate nei file seguenti
# es3_cognome_nome.png
# es3_cognome_nome300dpi.png
# es3_cognome_nome.pdf
