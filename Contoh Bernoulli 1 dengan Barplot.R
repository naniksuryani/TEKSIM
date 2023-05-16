#Nanik Suryani
#Bernouli 1 benar barplot

Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) {
    ifelse (X[z]<=p, Y[z]<-1, Y[z]<-0)
    tabel<-table(Y)/length(Y)
  }
  barplot(tabel, main="Bernoulli")
}
#Bernouli_1(n=1000,p=0.75)