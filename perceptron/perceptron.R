data("iris")
set.seed(12345)
rand<-sample(1:100,80)
iris<-iris[iris$Species!="virginica",]
ones<-rep(1,80)
X<-cbind(ones,iris[rand,1:4])
Y<-as.data.frame(iris[rand,5])
t<-1
w<-list(c(0,0,0,0,-1))
rownames(Y)<-rand
for(x in 1:80){
  if(t==1){
  mul<-c(as.matrix(X[x,])%*%as.matrix(w[[t]]))
  }
  else
  {
    mul<-c(as.matrix(X[x,])%*%t(as.matrix(w[[t]]))) 
  }
 
  if(Y[x,1]=="setosa"){
    if(mul<=0){
      w[[t+1]]<-w[[t]]+X[x,]
      t<-t+1
    }
  }
  else if(Y[x,1]=="versicolor"){
    if(mul>=0){
      w[[t+1]]<-w[[t]]-X[x,]
      t<-t+1
    }
  }
  
}

wfinal<-w[[length(w)]]
test<-cbind(1,iris[11,1:4])
finalmul<-c(as.matrix(test)%*%t(as.matrix(wfinal)))
if(finalmul>=0)
{
  classfinal<-"setosa"
}else
  classfinal<-"versicolor"

print(classfinal)
