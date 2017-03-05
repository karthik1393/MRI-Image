library(flexclust)
f<-read.csv("flower.csv",header = FALSE)
str(f)
fmat<-as.matrix(f)
str(fmat)
flower<-as.vector(fmat)
str(flower)
#compute distance and hclust
distance<-dist(flower,method = "euclidean")
flowercluster<-hclust(distance,method="ward.D")
plot(flowercluster)
rect.hclust(flowercluster,k=3)
###allocate the number of clusters
flowercluster1<-cutree(flowercluster,k=3)
flowercluster1
##mean for each cluster
tapply(flower,flowercluster1,mean)
dim(flowercluster1)=c(50,50)
##image function always takes matrix as input.refer the previous step
image(flowercluster1,axes=FALSE)
image(fmat,axes=FALSE,col=grey(seq(0,1,length=256)))
###MRI
m<-read.csv("healthy.csv",header=FALSE)
mmat<-as.matrix(m)
mri_vec<-as.vector(mmat)
str(mri_vec)
image(mmat,axes=FALSE,col=grey(seq(0,1,length=256)))
##k means
set.seed(1)
mri_k<-kmeans(mri_vec,centers = 5,iter.max = 1000)
str(mri_k)
healthyvector<-mri_k$cluster
dim(healthyvector)<-c(nrow(mmat),ncol(mmat))
image(healthyvector,axes=FALSE,col=rainbow(5))
##tumour 
t<-read.csv("tumor.csv",header=FALSE)
tu_mat<-as.matrix(t)
tu_vec<-as.vector(tu_mat)
tu_kcca<-as.kcca(mri_k,mri_vec)
tu_re<-predict(tu_kcca,newdata=tu_vec)
dim(tu_re)<-c(nrow(tu_mat),ncol(tu_mat))
image(tu_re,axes=TRUE,col=rainbow(5))
##one of the best recitations til date