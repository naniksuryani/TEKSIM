#input nilai p (peluang sukses)
#input peluang sukses

i<-100
sebaran_geom<-function(p){
  R<-runif(i)
  X<-log(1-R)/log(1-p)
  print(X)
}
sebaran_geom(0.5)