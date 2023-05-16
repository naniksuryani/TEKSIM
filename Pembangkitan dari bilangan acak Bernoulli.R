#Pembangkitan dari Bilangan Acak Bernoulli

Binomial_sim<-function(n,p) {
  i<-1000
  n<-n
  p<-p
  Binom<-NULL
  for (z in 1:i){
    m<-0
    for (k in 1:n){
      y<-(runif(1)<=p)+0
      m<-m+y
    }
    Binomial[z]<-m
  }
  (tabel<-table(Binom)/length(Binom))
  print(Binom)
}
Binom