library(class)
#每组数据用剩余的数据进行测试
knn_cross <- function(data, k,tag, k_cross){
  
  ind <- cut(1:nrow(data), breaks = k_cross, labels = F)[sample(1:nrow(data))]
  
  pred <- lapply(1:k_cross, function(x){
    ind_temp <- which(ind == x)
    temp <- knn(data[-ind_temp, ], data[ind_temp, ], cl=tag[-ind_temp], k=k)
    temp
  })
    pred <- unlist(pred)
  tag1 <- tag[unlist(lapply(1:k_cross, function(x){which(ind == x)}))]
  #结果
  table(pred,  tag1)
  
}

data <- iris[, 1:4]
tag <- iris[, 5]

knn_cross(data, 3, tag, 10)