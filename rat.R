library(tidyverse)

rat <-function(data=dat,st,ed){ 
share9193 <- data %>% filter(between(year,st,ed)&!is.na(sales)) %>% select(-year) %>%
  group_by(industry,purpose) %>% mutate(weight=mean(sales)) %>% slice_head() %>% ungroup()
sh9193 <- share9193 %>% filter(weight!=0) %>% select(-c("quantity","sales")) %>% 
  group_by(industry) %>% mutate(weight=weight/sum(weight)) %>% ungroup()
dat9193 <- right_join(data,sh9193,by=c("industry","purpose"))
rat9193 <- dat9193 %>% group_by(year,purpose) %>% 
  mutate(rat=sum(sales,na.rm = T)/sum(quantity,na.rm = T)) %>% 
  ungroup() %>% group_by(year,industry) %>% mutate(price=prod(rat^weight)) %>% 
  distinct(industry,year,price) %>% arrange(industry,year) %>% rename(pin=price)
return(rat9193)
}

pim1 <- function(data=dat,t=12,d=0,st=1982,ed=1982,real=FALSE){
  share <- data %>% filter(dplyr::between(year,st,ed)&!is.na(sales)) %>% 
    group_by(industry,purpose) %>% mutate(weight=mean(sales)) %>% slice_head() %>% ungroup(purpose) %>% 
    mutate(weight=weight/sum(weight)) %>% ungroup() %>% select(-c("year","quantity","sales"))
  datsh <- right_join(data,share,by=c("industry","purpose"))
  akss <- datsh %>% arrange(industry,purpose,year)
  if (real==TRUE){
    pr <- data %>% filter(dplyr::between(year,st,ed)&!is.na(quantity)) %>% group_by(purpose) %>% 
      mutate(price=sum(sales,na.rm = T)/sum(quantity)) %>% ungroup() %>% distinct(purpose,price)
    akss <- left_join(akss,pr,by="purpose") %>% group_by(year,industry) %>% 
      mutate(quantity=if_else(sales==0,1,sales/price),capital=prod(quantity^weight)) %>%
      ungroup() %>% distinct(year,industry,capital)
  } else {
    akss <- akss %>% group_by(year,industry) %>% 
      mutate(quantity=if_else(quantity==0,1,quantity),
             capital=prod(quantity^weight)) %>% ungroup() %>% distinct(year,industry,capital)
  }
  akss$stock <- 0
  akss$stock[1] <- akss$capital[1]
  
  for (i in 2:nrow(akss)) {
    for (j in 0:(t-1)) {
      if (i-j<1) next
      if (akss$year[i]==1978) akss$stock[i] <- akss$capital[i]
      if (akss$industry[i]==akss$industry[i-j]){
        akss$stock[i] <- akss$stock[i]+akss$capital[i-j]*((1-d)^j)
      }
    }
  }
  akss <- akss %>% distinct(industry,year,stock)
  return(akss)
}

pim2 <- function(data=dat,t=12,d=0,st=1991,ed=1993,real=F){
  share <- data %>% filter(dplyr::between(year,st,ed)&!is.na(sales)) %>% 
    group_by(industry,purpose) %>% mutate(weight=mean(sales)) %>% slice_head() %>% ungroup(purpose) %>% 
    mutate(weight=weight/sum(weight)) %>% ungroup() %>% select(-c("year","quantity","sales"))
  datsh <- right_join(data,share,by=c("industry","purpose"))
  akss <- datsh %>% arrange(industry,purpose,year)
  
  if (real==T){
    pr <- data %>% filter(dplyr::between(year,st,ed)&!is.na(quantity)) %>% group_by(purpose) %>% 
      mutate(price=sum(sales,na.rm = T)/sum(quantity)) %>% ungroup() %>% distinct(purpose,price)
    akss <- left_join(akss,pr,by="purpose") %>% group_by(year,industry) %>% 
      mutate(quantity=if_else(sales==0,1,sales/price),capital=prod(quantity^weight)) %>% ungroup()
  } else {
    akss <- akss %>% mutate(quantity=if_else(quantity==0,1,quantity))
  }
  akss$stock <- 0
  akss$stock[1] <- akss$quantity[1]
  for (i in 2:nrow(akss)) {
    for (j in 0:(t-1)) {
      if (i-j<1) next
      if (akss$year[i]==1978) akss$stock[i] <- akss$quantity[i]
      if (akss$industry[i]==akss$industry[i-j]&akss$purpose[i]==akss$purpose[i-j]){
        akss$stock[i] <- akss$stock[i]+akss$quantity[i-j]*((1-d)^j)
      }
    }
  }
  rbs <- akss
  rbs <- rbs %>% group_by(year,industry) %>% mutate(stock=prod(stock^weight)) %>% ungroup() %>% 
    distinct(industry,year,stock)
  return(rbs)
}