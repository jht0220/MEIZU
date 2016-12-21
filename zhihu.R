library("RCurl")
library("rvest")
library("rjson")

#模拟登录知乎 获取对应的session

login <- function(user, pass){
  
 postinfo <- c(NULL) 
 postinfo["xsrf"] <- "7be65f6d343abde0697b31f8b5162088"
 postinfo["password"] <- pass 
 postinfo["remember_me"] <- "true" 
 postinfo["email"] <- user 

 myHttpheader <- c(
  
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  
  "Accept-Language"="en-us",
  
  "Connection"="keep-alive",
  
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
  
 )

 cHandle <- getCurlHandle(httpheader = myHttpheader,.opts = list(verbose = TRUE))
 d = debugGatherer()

 temp <- postForm("https://www.zhihu.com/login/email",.params=postinfo,
                 
                 .opts=list(cookiefile=""),curl=cHandle,style="post")

 getCurlInfo(cHandle)[["cookielist"]]

 cHandle
}

#通过关键词获取查询的数据
get_search_value <- function(keyword, handle){
  
#   postinfo["type"] <- "question"
#   postinfo["q"] <- keyword

#   search_uri <- paste("https://www.zhihu.com/search?type=question&q=",
#                       keyword, sep="")
#   search_data <- getURL(search_uri, curl=handle)
  
  temp <- getForm("https://www.zhihu.com/search?",
                  .params=c(type = "content", q = URLencode(iconv(keyword, to="UTF-8", toRaw = F))),
                  
                  curl=handle)
  title <- html(temp) %>% html_nodes(".list") %>% html_nodes(".item") %>% html_nodes(".title") %>% html_text()
  link <- paste("https://www.zhihu.com",
                html(temp) %>% html_nodes(".list") %>% html_nodes(".item") %>% html_nodes(".title a") %>% html_attr("href"),
                sep="")
  
  
  # https://www.zhihu.com/r/search?q=%E6%89%8B%E6%9C%BA&range=&type=question&offset=10
  #获更的数据 
  ind <- 10
  next_page <- paste("https://www.zhihu.com/r/search?q=",
                     URLencode(iconv(keyword, to="UTF-8", toRaw = F)),
                     "&range=&type=content&offset=",
                     ind,
                     sep="")
  tag <- TRUE
  while(tag){
    temp <- getURL( next_page, curl = handle)
    
    # 判断是否有数据
    tag <- grepl("paging", temp)
    ind <- ind + 10
    if(tag){

#此处可能存在转换问题 使用trycath捕捉
      
      temp1 <- tryCatch({fromJSON(temp)}, 
                        warning = function(e){""},
                        error = function(e){""})
     if(c(temp1!="")[1]){
       title1 <- unlist(sapply(temp1$htmls, function(x){html(x, encoding = "UTF-8") %>% html_nodes(".item") %>% html_nodes(".title") %>% html_text()}))
       names(title1) <- NULL
       link1 <- unlist(sapply(temp1$htmls, function(x){html(x, encoding = "UTF-8") %>% html_nodes(".item") %>% html_nodes(".title a") %>% html_attr("href")}))
       names(link1) <- NULL
       link1 <- paste("https://www.zhihu.com", link1, sep="")
      # html(temp1$htmls[[5]], encoding = "UTF-8") %>% html_nodes(".item") %>% html_nodes(".title") %>% html_text()
      
       next_page <- paste("https://www.zhihu.com", temp1$paging[[1]], sep="")
        if(next_page == "https://www.zhihu.com"){
          tag <- FALSE 
          break
        }else{
          title <- c(title, title1)
          link <- c(link, link1)
        }
      }else{
        next_page <- paste("https://www.zhihu.com/r/search?q=",
                           URLencode(iconv(keyword, to="UTF-8", toRaw = F)),
                           "&range=&type=content&offset=",
                           ind,
                           sep="")
      }
    }
    
  }
#   id <- sapply(link, function(x){unlist(strsplit(x, split="/"))[5]})
#   names(id) <- NULL
  result <- data.frame(title = title, link = link, id=id, stringsAsFactors = F)
  
  
}

#"%E6%89%8B%E6%9C%BA"
#URLencode(iconv(keyword, to="UTF-8", toRaw = F)))
#paste("%", unlist(iconv(keyword, to="UTF-8",toRaw = T)), sep="", collapse="")

# temp <- getForm("https://www.zhihu.com/search?",
#                 .params=c(type = "question", q = URLencode(iconv(keyword, to="UTF-8", toRaw = F))),
#                  
#                  curl=handle)

# temp <- getURL("https://www.zhihu.com/question/31548509",
#                
#                curl=handle)




#模拟登录知乎 并获取代码
zhihu <- function(keyword = "魅族", user="kyl20@126.com", pass="kyl20@sina"){


  handle <- login(user, pass)
  result <- get_search_value(keyword, handle)

  write.csv(file = paste("d:\\zhihu_", keyword, ".csv", sep=""), result, row.names = F)
  
}


library(DBI)
library(RMySQL)

# 
con <- dbConnect(RMySQL::MySQL(), host = "127.0.0.1",
                 user = "root", password = "kyl0220",dbname="zhihu")

# sql <- "select Host, user from mysql.user"
# dbGetQuery(con, sql)
dbSendQuery(con, "SET NAMES UTF8")
if(dbExistsTable(con, "table_question")){
  dbWriteTable(con, "table_question",result,row.names=F)
}else{
  dbWriteTable(con, "table_question",dat,row.names=F, append=F,overwrite=T)
}
