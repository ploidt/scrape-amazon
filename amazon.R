# install.packages("rvest")
library(rvest)
library(magrittr)
require(RSelenium)

search.keyword <- "mirror"
page.number <- 2

remDr <- remoteDriver(browserName = "phantomjs")

remDr$open()
remDr$navigate(paste("https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=", search.keyword, sep = ""))
remDr$deleteAllCookies()

web.html <- read_html(remDr$getPageSource()[[1]])

d = NULL

get.all.info <- function(web.html,id){
  
  products <- web.html %>%
    html_nodes(".s-result-item")
  
  title <- products %>%
    html_node("h2") %>%
    html_text()
  
  asin <- products %>%
    html_attr("data-asin")
  
  price <- products %>%
    html_node(".a-color-base span") %>%
    html_text(trim= TRUE) %>% gsub("\n",".",.) %>% gsub(" ","",.)
  
  star <- products %>%
    html_node(".a-icon-star")%>%
    html_text() %>%
    gsub(" out of 5 stars", "", .) %>%
    as.double()
  
  prime <- products %>%
    html_node(".a-icon-prime") %>% html_text()
  prime <- ifelse(is.na(prime),"N","Y")
  
  link <- products %>%
    html_node("a[href]") %>% html_attr("href")
  
  sponser <- products %>%
    html_node("h5") %>% html_text(trim=TRUE)
  sponser <- ifelse(is.na(sponser),0,1)
  
  data <- data.frame(title, asin,price, star, prime,link,sponser, stringsAsFactors = FALSE)
}

for(i in seq(1, page.number, 1)){
  
  print(i)
  temp <- get.all.info(web.html, id)
  temp2 <- subset(temp, temp$sponser == 0)
  temp2 <- temp2[complete.cases(temp2),]
  
  d = rbind(d,temp2)
  
  if(i != page.number){
    script <- "document.getElementById('pagnNextLink').click();"
    remDr$executeScript(script, args = list())
    
    Sys.sleep(5)
  }
  
}

get.seller <- function(link) {
  seller <- link %>%
    read_html() %>%
    html_node("#merchant-info") %>%
    html_text(trim=TRUE)
  seller <- ifelse(grepl("Amazon.com", seller),"AMZ","FBA")
}

get.customer.review <- function(link) {
  review <- link %>%
    read_html() %>%
    html_node("#acrCustomerReviewText") %>%
    html_text(trim=TRUE) %>% 
    gsub(" customer reviews","",.) %>%
    gsub(" customer review","",.)
}
seller <- rep(NA,nrow(d))
review <- rep(NA,nrow(d))

for(i in 1:nrow(d)){

  seller[i] <- get.seller(d$link[i])
  review[i] <- get.customer.review(d$link[i])
  if(i %% 20 == 0) print(i)
}

d$seller <- seller
d$review <- review

remDr$close()

write.csv(d, file = "amazaon_1.csv")

d %>% View()