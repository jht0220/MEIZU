library("bitops")
library("RCurl")
library("rjson")

#提取flyme官网发布的固件以及适配的机型

link_source <- "http://www.flyme.cn/static/js/zh/firmware.js"


x <- "http://www.flyme.cn/static/js/zh/model_base_18.js"


#获取型号id
get_model <-  function(x){
  temp1 <- readLines(x)[3]
  
  temp2 <- unlist(strsplit(unlist(strsplit(temp1, split="var firmware = "))[2], ";"))[1]
  
  #获取支持的机型
  phone_list <- fromJSON(temp2)
  
  #获取对应机型的页面链接
  phone <- unlist(sapply(phone_list, function(x){sapply(x, function(y){ y$phone_name})}))
  link1 <- paste("http://www.flyme.cn/firmwarelist-", 
                 unlist(sapply(phone_list, function(x){sapply(x, function(y){ y$phone_id})})),
                 ".html", sep="")
  link2 <- paste("http://www.flyme.cn/static/js/zh/model_base_", 
                 unlist(sapply(phone_list, function(x){sapply(x, function(y){ y$phone_id})})),
                 ".js", sep="")
  
  phone_id <- unlist(sapply(phone_list, function(x){sapply(x, function(y){ y$phone_id})}))
  type <- gsub("[0-9]+", "", names(phone))
  names(phone) <- NULL
  names(phone_id) <- NULL
  temp <- data.frame(phone = phone, id = phone_id, link = link2,
                     type = type, stringsAsFactors = F)
}


#获取型号的链接
get_link <- function(x){
  temp_phone1 <- readLines(x)[3]
  temp_phone2 <- unlist(strsplit(unlist(strsplit(temp_phone1, split="var model_base = "))[2], ";"))[1]
  
  brand <-  fromJSON(temp_phone2)$brand
  model <-  fromJSON(temp_phone2)$model
  versions <- fromJSON(temp_phone2)$versions
  version <- sapply(versions, function(x){x[2]})
  link1 <- paste("http://www.flyme.cn/", sapply(versions, function(x){x[1]}), sep="")
  temp <- data.frame(link = rep(x, length(link1)), link1 = link1,
                     version = version, stringsAsFactors = F)
  
}


#获取型号对应版本的flyme
get_value <- function(y){
  
  temp_phone3 <- fromJSON(unlist(strsplit(unlist(strsplit(readLines(y)[3], split=" = "))[2], ";"))[1])
  hardware <- NULL
  shortname <- NULL
  publish_time  <- NULL
  update_time <- NULL
  tag <- NULL
  if(length(temp_phone3$type1)>0){
    hardware <- sapply(temp_phone3$type1, function(x){x$hardware})
    shortname <- sapply(temp_phone3$type1, function(x){x$shortname})
    publish_time <- sapply(temp_phone3$type1, function(x){x$publish_time})
    update_time <- sapply(temp_phone3$type1, function(x){x$update_time})
    tag <- rep("稳定版", length(update_time))
  }
  
  if(length(temp_phone3$type2)>0){
    hardware <- c(hardware, sapply(temp_phone3$type2, function(x){x$hardware}))
    shortname <- c(shortname, sapply(temp_phone3$type2, function(x){x$shortname}))
    publish_time <- c(publish_time, sapply(temp_phone3$type2, function(x){x$publish_time}))
    update_time <- c(update_time, sapply(temp_phone3$type2, function(x){x$update_time}))
    tag <- c(tag, rep("体验版", length(temp_phone3$type2)))
  }
  publish_date <- substr(as.POSIXct(as.numeric(publish_time)/1000, origin="1970-01-01"), 1, 10)
  update_date <- substr(as.POSIXct(as.numeric(update_time)/1000, origin="1970-01-01"), 1, 10) 
  
  temp <- data.frame(hardware = hardware,
                     shortname = shortname,
                     publish_date = publish_date,
                     update_date = update_date, 
                     link1 = rep(y, length(hardware)), 
                     tag = tag, stringsAsFactors = F)
  temp
}


##最终执行的sql
get_info <- function(link_source){
  
  phone <- get_model(link_source)
  
  temp <- sapply(phone$link, get_link)
  link <- NULL
  link <- unlist(sapply(seq(from=1, to=length(temp), by= 3),  function(x){link <- c(link, temp[[x]])}))
  link1 <- NULL
  link1 <- unlist(sapply(seq(from=2, to=length(temp), by= 3),  function(x){link1 <- c(link1, temp[[x]])}))
  version <- NULL
  version <- unlist(sapply(seq(from=3, to=length(temp), by= 3),  function(x){version <- c(version, temp[[x]])}))
  
  phone_temp1 <- data.frame(link = link, link1 = link1, version = version,
                            stringsAsFactors = F)
  
  phone_temp2 <- merge(phone, phone_temp1 ,all =T, by=c("link"))
  
  temp <- NULL
  for ( zz in phone_temp2$link1){
    tryCatch({
      t1 <- get_value(zz)
      temp <- rbind(temp, t1)
    },error=function(e){cat("解析出现错误")}
    )
  }
  
  #temp <- sapply(phone_temp2$link1, get_value)
  
  hardware <- NULL
  hardware <- unlist(sapply(seq(from=1, to=length(temp), by= 6),  function(x){hardware <- c(hardware, temp[[x]])}))
  shortname <- NULL
  shortname <- unlist(sapply(seq(from=2, to=length(temp), by= 6),  function(x){shortname <- c(shortname, temp[[x]])}))
  publish_date <- NULL
  publish_date <- unlist(sapply(seq(from=3, to=length(temp), by= 6),  function(x){publish_date <- c(publish_date, temp[[x]])}))
  update_date <- NULL
  update_date <- unlist(sapply(seq(from=4, to=length(temp), by= 6),  function(x){update_date <- c(update_date, temp[[x]])}))
  link1 <- NULL
  link1 <- unlist(sapply(seq(from=5, to=length(temp), by= 6),  function(x){link1 <- c(link1, temp[[x]])}))
  tag <- NULL
  tag <- unlist(sapply(seq(from=6, to=length(temp), by= 6),  function(x){tag <- c(tag, temp[[x]])}))
  
  temp <- data.frame(hardware = hardware,
                     shortname = shortname,
                     publish_date = publish_date,
                     update_date = update_date,
                     link1 = link1, tag=tag, stringsAsFactors = F)
  
  phone_temp3 <- merge(temp, phone_temp2 ,all =T, by=c("link1"))
  
  result <- subset(phone_temp3, select=c("hardware", "shortname", "publish_date",
                                         "update_date", "phone", "id", "type", "version", "tag"))
  
  result$publish_day <- as.integer(gsub("-", "", result$publish_date))
  result$insert_day <- format(Sys.Date(), "%Y%m%d")
  result$phone1 <- iconv(result$phone, "CP936", "UTF8")
  result$version1 <- iconv(result$version, "CP936", "UTF8")
  result$tag1 <- iconv(result$tag, "CP936", "UTF8")
  
  
  #写入到数据库
  library(DBI)
  library(RMySQL)
  
  # 
  con <- dbConnect(RMySQL::MySQL(), host = "****",dbname="****",
                   user = "****", password = "****")
  
  
  
  dbSendQuery(con, 'SET NAMES UTF8')
  if(!dbExistsTable(con, "phone_flyme")){
   dbSendQuery(con, "create table phone_flyme (hardware char(100),
               shortname char(100), publish_date char(20), 
              update_date char(20), phone char(50), id int,
               type char(20), version char(20), tag char(20), publish_day int,
               insert_day int)")
  dbCommit(con)
  }
  dbSendQuery(con, paste("delete from meizu.phone_flyme where insert_day=", 
                         format(Sys.Date(), "%Y%m%d"), sep="")
                          )
  dbCommit(con)
  result1 <- result[, -c(5, 8,9)]
  names(result1)[9:11] <- c("phone", "version", "tag")
  dbWriteTable(con, "phone_flyme", result1,append=T, row.names=F)
  dbCommit(con)
  # dbGetQuery(con, "select * from phone_flyme limit 10")
  write.csv(result[, -c(12:14)], file=paste("d:\\", format(Sys.Date(), "%Y%m%d"), "_Flyme最新固件.csv",sep=""),
             row.names=F)
}




get_info(link_source)




