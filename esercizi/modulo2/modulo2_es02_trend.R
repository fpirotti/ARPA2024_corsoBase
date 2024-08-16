############### Check stationarity -----
library(zoo)
varianze.fin30.t2 <- rollapplyr(t2, 30, var, by.column = FALSE, fill = NA)
medie.fin30.t2 <- rollapplyr(t2, 30, mean, by.column = FALSE, fill = NA)



varianze.fin30.t1 <- rollapplyr(t1, 30, var, by.column = FALSE, fill = NA)
medie.fin30.t1 <- rollapplyr(t1, 30, mean, by.column = FALSE, fill = NA)


plot(medie.fin30.t2, ylim=c(5,16), col="red")
points(medie.fin30.t1 )


plot(varianze.fin30.t2, ylim=c(0,2), col="red")
points(varianze.fin30.t1 )

