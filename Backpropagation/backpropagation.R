library(e1071)
data("iris")
m<-80
set.seed(1234)
rand<-sample(1:150,80)
X<-iris[rand,1:4]
X<-scale(X)
ones<-rep(1,80)
Y<-matrix(nrow=80,ncol=3)
t<-1
for(i in rand){
  if(iris[i,5]=="setosa"){
    Y[t,]<-c(1,0,0)
  }
  else if(iris[i,5]=="versicolor")
  {
    Y[t,]<-c(0,1,0)
  }
  else
    Y[t,]<-c(0,0,1)
  t<-t+1
}
set.seed(100)
theta1<-matrix(runif(150,-1.09,1.09),nrow=30,ncol=5)
set.seed(2000)
theta2<-matrix(runif(93,-1.09,1.09),nrow=3,ncol=31)
cost<-function(theta,X,Y,lambda)
{
  theta1<-matrix(theta[1:150],nrow=30,ncol=5)
  theta2<-matrix(theta[151:243],nrow=3,ncol=31)  
  a1<-cbind(1,X)
  z2<-as.matrix(a1)%*%t(theta1)
  a2<-cbind(1,sigmoid(z2))
  z3<-as.matrix(a2)%*%t(theta2)
  a3<-sigmoid(z3)
  h<-a3
  
  
  
  regu = lambda*(sum(theta1[,-1]^2)+sum(theta2[,-1]^2))/(2*m)
  J = -sum(Y*log(h)+(1-Y)*log(1-h))/m+regu
  
  delta3 = h-Y
  delta2 = (delta3%*%theta2[,-1])*dsigmoid(z2)
  
  thres1 = matrix(1,nrow(theta1),ncol(theta1))
  thres1[,1] = 0
  thres2 = matrix(1,nrow(theta2),ncol(theta2))
  thres2[,1] = 0
  
  theta1grad = (t(delta2)%*%a1)/m + thres1*theta1*lambda/m
  theta2grad = (t(delta3)%*%a2)/m + thres2*theta2*lambda/m
  res<-J
  res<-list(cost=J,grad=list(grad1=theta1grad,grad2=theta2grad))
  
}
costFunction = function(p) {
  result <-cost(p,X,Y,lambda=1)
  J = result$cost
  grad = c(as.vector(result$grad$grad1),as.vector(result$grad$grad2))
  grad = as.matrix(grad,length(grad),1)
  attr(J,"gradient")<-grad
  return(J)
}

theta<-c(as.vector(theta1),as.vector(theta2))
theta<-as.matrix(theta,length(theta),1)
g<-nlm(costFunction,theta)
theta1<-matrix(g$estimate[1:150],nrow=30,ncol=5)
theta2<-matrix(g$estimate[151:243],nrow=3,ncol=31)
test<-iris[-rand,1:4]
test<-scale(test)
h1<-sigmoid(cbind(1,as.matrix(test))%*%t(theta1))
h2<-sigmoid(cbind(1,h1)%*%t(theta2))
h3<-sigmoid(h2)
ans<-c()
index<-c()
for(i in rownames(test))
{
  index[i]<-which.max(h3[i,])
  if(index[i]==1)
    ans[i]<-"setiosa"
  if(index[i]==2)
    ans[i]<-"versicolor"
  if(index[i]==3)
    ans[i]<-"virginica"
}
ans<-as.data.frame(ans)
View(ans)