#Nanik Suryani B2A020014

#Gabungan Perulangan Multiplicative RNG dan Bernoulli 1


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
#barplot(table,main="Bernoulli")
#Bernouli_1(100,0.65)

#Gabungan

Gabungan_RNG_ber<-function(a,z0,m,n,p) {
  xi<-matrix(NA,n,3)
  di<-matrix(NA,n,3)
  colnames(xi)<-c("aZ","Xi","Ui")
  colnames(xi)<-c("Xi","0","1")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
    di[i,1]<-xi[i,2]
    di[i,2:3]<-Bernouli_1(xi[i,2],p)
  }
  #hist (xi[,3])
  View(xi)
  View(di)
}