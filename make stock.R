dat <- read.csv("maindata.csv")
adat <- read.csv("averagedata.csv")
boj <- read.csv("boj_robot.csv") %>% distinct(year,.keep_all = T)
boj <- boj[,3:4]
colnames(boj)[2] <- "boj_price"
imp <- data.frame(industry=rep(0,27*97),import=0,year=0)
st=1
for (s in 2:26) {
  jip <- readxl::read_xlsx("jip2021.xlsx",sheet = s)[4:100,c(2,104)]
  jip <- jip %>% mutate(year=1992+s)
  imp[st:(st+96),1] <- jip[,1]
  imp[st:(st+96),2] <- jip[,2]
  imp[st:(st+96),3] <- jip[,3]
  st <- st+97
}
jjara <- read.csv("jipjara.csv")
for (j in 1:nrow(jjara)) {
  imp$industry[imp$industry==jjara$JIP2018_name[j]] <- jjara$JRA[j]
}
new <- read.csv("newmergedata.csv")
tr <- read.csv("/Users/KosukeA/Documents/R/Reas/inpmaster.csv")
for (i in 1:nrow(tr)) {
  new$industry[new$industry==tr$jpi[i]] <- tr$engi[i] 
}
new$purpose[new$purpose=="組立"] <- "assembly"
new$purpose[new$purpose=="切削・研削"] <- "cutting_grinding"
new$purpose[new$purpose=="ダイキャスティング"] <- "casting"
new$purpose[new$purpose=="その他鋳造"] <- "casting"
new$purpose[new$purpose=="入出荷"] <- "picking_packaging"
new$purpose[new$purpose=="鍛造"] <- "forging"
new$purpose[new$purpose=="塗装"] <- "painting"
new$purpose[new$purpose=="プレス"] <- "press"
new$purpose[new$purpose=="溶接"] <- "welding"
new$purpose[new$purpose=="樹脂成型"] <- "resin_molding"
new$purpose[new$purpose=="プレス"] <- "press"

np <- new$purpose %>% unique() %>% sort()
np <- np[1:9]
hinan <- new
new <- left_join(hinan,boj) %>% filter(purpose%in%np&!is.na(boj_price)) %>% group_by(year,industry,purpose) %>% 
  mutate(toq=sum(quantity,na.rm = T),toi=sum(investment,na.rm = T),
         quality=price/boj_price,realq=toq*quality) %>% slice_head() %>% 
  ungroup()
new <- left_join(new,imp) %>% mutate(import=as.numeric(import))

stock <- new %>% 
  group_by(industry) %>% mutate(obs=n_distinct(year)) %>% ungroup() %>% 
  filter(obs==max(obs,na.rm = T)) %>% select(-obs) %>% arrange(industry,purpose,year)

stand <- stock %>% filter(year==2001) %>% group_by(industry) %>% 
  mutate(ratio=toi/sum(toi)) %>% ungroup() %>% select(industry,purpose,ratio)

stock12 <- left_join(stock,stand) %>%distinct_all() %>%  mutate(stockrq=realq,stockq=toq) %>% 
  arrange(industry,purpose,year)

for (i in 1:(nrow(stock12)-12)) {
  for (j in 1:12) {
    if (stock12$industry[i+j]==stock12$industry[i]&
        stock12$purpose[i+j]==stock12$purpose[i]){
      stock12$stockrq[i+j] <- stock12$stockrq[i+j]+stock12$realq[i]
      stock12$stockq[i+j] <- stock12$stockq[i+j]+stock12$toq[i]
    }
  }
}

stock12 <- stock12 %>% mutate(stockrq=stockrq^ratio,stockq=stockq^ratio,pin=price^ratio) %>% 
  group_by(year,industry) %>% 
  mutate(rqstock=prod(stockrq,na.rm = T),qstock=prod(stockq,na.rm = T),pin=prod(pin,na.rm = T)) %>% 
  slice_head() %>% ungroup()

fdat12 <- left_join(dat,stock12,by=c("year","industry")) %>% 
  filter(!is.na(rqstock)) %>% filter(year>=2001) %>% filter(!is.na(import))

fadat12 <- left_join(adat,stock12,by=c("year","industry")) %>% 
  filter(rqstock!=0) %>% mutate(rqstock=rqstock/firm,
                                qstock=qstock/firm) %>% filter(year>=2001)%>% filter(!is.na(import)) %>% 
  arrange(industry,year)

fadat12 %>% with(.,cor(pin,qstock))

ause <- fadat12[,c(1:23,47:54)]

write_excel_csv(ause,"avstock.csv")
