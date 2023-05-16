#Pembangkitan dari bilangan acak Uniform

i<-1000
X<-runif(i)
Binom<-as.numeric(cut(X,breaks=c(0,1/8,4/8,7/8,1),include.lowest = T))-1
(tabel<-table(Binom)/length(Binom))
View(Binom)
Binom