#Nanik Suryani B2A020014
#Multiplicative RNG

#a=35,z0=11123,m=138,n=100

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3]) 
  View(xj)
}
multiplicative_RNG(35,11123,138,100)