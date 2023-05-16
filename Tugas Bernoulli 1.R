#Nanik Suryani B2A020014
#Bernoulli 1

#n=dari xj,p=0.65

Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
barplot(table,main="Bernoulli")
#Bernouli_1(100,0.65)