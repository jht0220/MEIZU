
#定义核函数
sigmoid <- function(x){
  1/(1+exp(-1*x))
}

lr_train <- function(train_x, train_y, opts){
  
  dim_x = dim(train_x)[1]
  dim_y = dim(train_x)[2]
  
  max_iter = opts["max_iter"]
  alpha = as.numeric(opts["alpha"])
  weights = matrix(rep(1, dim_y), ncol=1)
  
  for(i in 1:max_iter){
    if(opts["method"]=="GrandDescend"){
      temp1 = sigmoid(train_x%*%weights)
      temp2 = train_y - temp1
      weights = weights + alpha*t(train_x)%*%temp2
    }
    else if(opts["method"]=="StocGradDescend"){
      for(j in 1:dim_x){
        temp1 = sigmoid(train_x[j, ]%*%weights)
        temp2 = train_y[j, ] - temp1
        weights = weights + alpha*train_x[j, ]%*%temp2
      }
    }else if(opts["method"] =="SmoothStocGradDescend"){
      for(j in 1:dim_x){
        alpha = 4/(i+j+1) +0.01
        temp1 = sigmoid(train_x[j, ]%*%weights)
        temp2 = train_y[j, ] - temp1
        weights = weights + alpha*train_x[j, ]%*%temp2
      }
    }else{
      print("Error!")
    }
  }
  
 weights
}

temp <- read.csv("E:\\MEIZU\\lr\\lr.csv", header=T, stringsAsFactors = F)
train_x = as.matrix(temp[, 1:2])
train_y = as.matrix(temp[, 3])
opts=c("max_iter"=1500, "alpha"=0.01, "method"="SmoothStocGradDescend")

t <- lr_train(train_x, train_y, opts)


lab <- ifelse(sigmoid(train_x %*%t)>=.5, 1, 0)

sum(ifelse(lab == train_y,1,0))

write.csv(data.frame(lab,train_y), file="d:/1.csv")
s1 ="Species"
s2 ="Petal.Width"
ind <- which(colnames(iris) %in% c(s1,s2))
result = iris[, ind]


sqrt(444/pi)/5

a1 <- 