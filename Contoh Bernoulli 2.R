#Nanik Suryani B2A020014
#Bernoulli 2

#n=1000,p=0.75

Bernouli_2<-function(n,p) {{
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
barplot(table,main="Bernoulli")}