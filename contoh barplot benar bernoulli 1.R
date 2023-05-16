#Nanik Suryani
#Bernouli 1 benar barplot

Bernouli_1<-function(p,i) {
  U<-runif(i)
  Y<-NULL
  for (z in 1:i) {
    ifelse (U[z]<=p, Y[z]<-1, Y[z]<-0)
    tabel<-table(Y)/length(Y)
  }
  barplot(tabel, main="Bernoulli")
}
Bernouli_1(p=0.75,i=1000)
