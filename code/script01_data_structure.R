## scalar
a<-1.2
b<-"a"

## vector
va<-c(1.2,3,4.6)
mean(va)

vb<-c("1","2","3")
#mean(vb)seq ##Error

## Matrix
vc<-c(1.3, 4.2, 5.6)
vd<-c(10,3,2)

ma<-cbind(vc, vd)
mb<-rbind(vc, vd)

x<-c("1", 3)
# Matrix will combine the data types, so make sure you separate numerical with characters"

## tricky data type: Factor
vz<-c("a","b","c")
vf<-factor(vz)

## data frame
df0<-data.frame(letter=vz,
           numeric=va)
df0
