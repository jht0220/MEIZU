library(reshape2)
library(plyr)
#   朴素贝叶斯分类 Naive Bayesian classification)


 cal_prob <- function(data){
   data <- as.vector(data)
   num <- length(data)
   prob_all <- table(data)/num
   result <- as.data.frame(prob_all)
   names(result) <- c("result", "prob_all")
   result
 }
 

 cal_prioi_prob <- function(data){
   temp <- melt(data, id = length(data))
   names(temp)[1] <- "result"
   num_result <- length(unique(temp$result))
   
   temp1 <- ddply(temp, .(result, variable, value), summarize, num=length(value))
   temp2 <- ddply(temp1, .(result, variable), transform, total=sum(num))
   temp2$prob_each <- temp2$num/temp2$total
   result <- subset(temp2, select=c("result", "variable", "value", "prob_each", "num", "total"))
   result
 }
 
 #如果检测集特征值在训练集中没有出现，进行拉普拉斯校准 即每个类型加1  在总数很大的情况下，分类加1不影响总体情况
 cal_result <- function(prob_each,prob_all, test){
   temp <- data.frame()
   sapply(1:length(test), function(x){ temp <<- rbind(temp,
                                                      subset(prob_each, (variable==names(test)[x])&(value==test[[x]][1])))})
   
   prob_each1 <- ddply(temp, .(result), summarize, prob_each=prod(prob_each))
   result <- merge(prob_each1, prob_all, by=c("result"))
   result$prob <- result$prob_each*result$prob_all
   as.character(result[which.max(result$prob), "result"])
   
 }
 

 cal_result2 <- function(prob_each,prob_all,test){
   temp <- data.frame()
   cnt_tag <-length(prob_all$result)
   #如果检测集特征值在训练集中没有出现，进行拉普拉斯校准 即每个类型加1  在总数很大的情况下，分类加1不影响总体情况
   for(x in 1:length(test)){
    # cat(x)
     temp1 <- subset(prob_each, (variable==names(test)[x])&(value==test[[x]][1]))
    if(length(temp1[[1]])<cnt_tag){
       tag_loss <- setdiff(prob_all$result, temp1$result)
       cnt <- sapply(tag_loss, function(y){ length(unique(subset(prob_each, result==y & variable==names(test)[x],"value", drop=T)))})
       total_cnt <- sapply(tag_loss, function(y){ unique(subset(prob_each, result==y & variable==names(test)[x],"total", drop=T))})
       temp1 <- data.frame(result = tag_loss,
                           variable = rep(names(test)[x], length(tag_loss)),
                           value = rep(test[[x]][1], length(tag_loss)),
                           prob_each = 1/(total_cnt+cnt),
                           num = rep(length(tag_loss), 1),
                           total = total_cnt + cnt)
     } 
     temp <- rbind(temp, temp1)
   }
   
   prob_each1 <- ddply(temp, .(result), summarize, prob_each=prod(prob_each))
   result <- merge(prob_each1, prob_all, by=c("result"))
   result$prob <- result$prob_each*result$prob_all
   result$prob_unifrom <- result$prob/sum(result$prob)
   #直接取概率最大的分类
    paste(as.character(result[which.max(result$prob), "result"]), 
          as.character(result[which.max(result$prob), "prob_unifrom"]),
          sep=",")
#    #当且分类1的概率最大，且标准化的值大于指定值时 才分类为1
#    ind <- which.max(result$prob_unifrom)
#    tag_result <- ifelse(result[ind, "result"]=="1", ifelse(result[ind, "prob_unifrom"]>=0.7, 
#                                                            result[ind, "result"],
#                                                            result[-ind, "result"]), result[ind, "result"])
#    as.character(tag_result)
   
   
 }
 
 
 cal_result22 <- function(prob_each,prob_all,test){
   temp <- data.frame()
   cnt_tag <-length(prob_all$result)
   
   temp <- sapply( 1:length(test), function(x){
     
     temp1 <- subset(prob_each, (variable==names(test)[x])&(value==test[[x]][1]), select=c("result", "prob_each"))
     if(length(temp1[[1]])<cnt_tag){
       tag_loss <- setdiff(prob_all$result, temp1$result)
       cnt <- sapply(tag_loss, function(y){ length(unique(subset(prob_each, result==y & variable==names(test)[x],"value", drop=T)))})
       total_cnt <- sapply(tag_loss, function(y){ unique(subset(prob_each, result==y & variable==names(test)[x],"total", drop=T))})
       temp1 = data.frame(result =c(temp1$result, tag_loss),
                          prob_each = c(temp1$prob_each, 1/(total_cnt + cnt)))
       
     } 
     temp1 
   })
   
   temp1 <- data.frame(result =as.vector(sapply(seq(from=1, to = length(temp), by=2), function(x){ temp[[x]]})),
                       prob_all= as.vector(sapply(seq(from=2, to = length(temp), by=2), function(x){ temp[[x]]})))
   temp2 <- rbind(temp1, prob_all)
   result <- ddply(temp2, .(result), summarize, prob=prod(prob_all))
   result$prob_unifrom <- result$prob/sum(result$prob)
   #直接取概率最大的分类
   paste(as.character(result[which.max(result$prob), "result"]), 
         as.character(result[which.max(result$prob), "prob_unifrom"]),
         sep=",")
   
 }
 #要求最后一列是分类结果列
 bayesian  <- function(data_train, data_test){
   prob_all <- cal_prob(data_train[, length(data_train)])
   prob_each <- cal_prioi_prob(data_train)
   result <- sapply(1:length(data_test[, 1]), function(x){
                                      cal_result22(prob_each,prob_all, data_test[x, ])})
   result
   
   
 }
#  data_train <- read.table("e:\\Meizu\\weather.txt", sep="\t", header=T,stringsAsFactors = F)
#  data_test <- data_train[1:14,1:4]
#  sapply(1:length(data_test[, 1]), function(x){ str(data_test[x, ])})
 