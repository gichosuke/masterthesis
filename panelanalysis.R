library(data.table)
library(tidyverse)
manudat <- read.csv("manudata.csv",fileEncoding = "CP932")
dat <- read.csv("mergeddata.csv",fileEncoding = "CP932") %>% 
  rename(industry=産業,firm=企業数,office=事業所数,asset=資産,liquid=流動資産,fixed=固定資産,
         worker=従業者数,full_worker=常時従業者数,
         sales=売上高,sales_benefit=売上総利益,deferred_asset=繰延資産,debt=負債,net_asset=純資産,
         operating_cost=営業費用,operating_benefit=営業利益,ordinary_benefit=経常利益,
         net_income=当期純利益,added_value=付加価値額,group_company=子会社_関連会社数,
         subsidiary=国内子会社,abroad_subsidiary=海外子会社) %>% 
  mutate(ave_labor=full_worker/firm,
         ave_asset=asset/firm)

tr <- read.csv("/Users/KosukeA/Documents/R/Reas/inpmaster.csv")
for (i in 1:nrow(tr)) {
  dat$industry[dat$industry==tr$jpi[i]] <- tr$engi[i] 
}
dat$industry[dat$industry=="自動車・同附属品製造業"] <- "Motor Vehicles"
dat$industry[dat$industry=="その他の輸送用機械器具製造業"] <- "Miscellaneous transportation equipment"

library(plm)
boj <- read.csv("boj_robot.csv") %>% distinct(year,.keep_all = T)
boj <- boj[,3:4]
colnames(boj)[2] <- "boj_price"

tmp <- dat %>% filter(!is.na(quantity)) %>% group_by(industry,year) %>% 
  mutate(investment=sum(investment,na.rm = T),
         quantity=sum(quantity,na.rm = T),
         price=investment/quantity) %>% ungroup() %>% distinct(industry,year,.keep_all = T)
tmp <- left_join(tmp,boj) %>% 
  mutate(quality=price-boj_price)
pdat <- pdata.frame(tmp,index = c("industry","year"))


# price / quality and other variables -------------------------------------

price <- data.frame(var=rep(0,19),VIF=rep(0,19))
quality <- data.frame(var=rep(0,19),VIF=rep(0,19))

for (i in 1:19) {
  price$var[i] <- colnames(tmp)[i+3]
  quality$var[i] <- colnames(tmp)[i+3]
  price$VIF[i] <- 1/(1-cor(tmp$price,tmp[,i+3]/tmp$firm)^2)
  quality$VIF[i] <- 1/(1-cor(tmp$quality,tmp[,i+3]/tmp$firm)^2)
}

# na list

nalist <- data.frame(var=colnames(tmp)[4:22],include_na=rep(0,19))
for (i in 1:19) {
 if (which(tmp[nalist$var[i]]==0) %>% length()!=0)  nalist$include_na[i] <- 1
}

# where 0
# until 2001 !!!!

paste(colnames(tmp)[4:22],collapse = "+")

pmodel <- plm(firm~price+worker+full_worker+asset+liquid+fixed+deferred_asset+負債及び純資産+debt+net_asset+sales+sales_benefit+operating_cost+operating_benefit+ordinary_benefit+net_income+added_value+group_company+subsidiary+abroad_subsidiary,
              data = pdat,model = "within")
summary(pmodel)
apmodel <- plm(firm^2~price*firm+worker+full_worker+asset+liquid+fixed+deferred_asset+負債及び純資産+debt+net_asset+sales+sales_benefit+operating_cost+operating_benefit+ordinary_benefit+net_income+added_value+group_company+subsidiary+abroad_subsidiary,
              data = pdat,model = "within")
summary(apmodel)

#select some
#make average data
adat <- tmp
for (i in 4:22) {
  adat[,i] <- tmp[,i]/tmp$firm
}
padat <- pdata.frame(adat,index = c("industry","year"))
alllm <- 
  lm(firm~price+worker+full_worker+asset+liquid+fixed+debt+sales+sales_benefit+operating_cost+operating_benefit+ordinary_benefit+net_income+added_value+group_company+subsidiary,
    data = adat)
library(car)
vif(alllm)
master <- 1/(1-cor(adat[,4:22])^2) %>% as.data.table()

# omit full_worker liquid fixed 負債及び純資産 debt deffered_asset net_asset sales_benefit operating_cost
# net_income added_value ordinary_benefit asset
alllm2 <- 
  lm(firm~price+worker+sales+operating_benefit+asset+group_company+subsidiary,
     data = adat)
summary(alllm2)
vif(alllm2)

# log ---------------------------------------------------------------------
# benefit can be negative
lalllm2 <- 
  lm(log(firm)~log(price)+log(worker)+log(sales)+log(asset)+
       log(group_company)+log(subsidiary),
     data = adat)
summary(lalllm2)


# panel reg ---------------------------------------------------------------
pres1 <- plm(firm~price+worker+sales+operating_benefit+asset+group_company+subsidiary,
            data = padat,model = "within")
summary(pres1)

pres2 <- plm(log(firm)~log(price)+log(worker)+log(sales)+log(asset)+log(group_company)+log(subsidiary),
            data = padat,model = "within")
summary(pres2)

pres3 <- plm(firm~price+worker+sales+asset+group_company+subsidiary,
            data = padat,model = "within")
summary(pres3)

qres1 <- plm(firm~quantity+worker+sales+operating_benefit+asset+group_company+subsidiary,
            data = padat,model = "within")
summary(qres1)

qres2 <- plm(log(firm)~log(quantity)+log(worker)+log(sales)+log(asset)+log(group_company)+log(subsidiary),
            data = padat,model = "within")
summary(qres2)

qres3 <- plm(firm~quantity+worker+sales+asset+group_company+subsidiary,
             data = padat,model = "within")
summary(qres3)


# add gdp -----------------------------------------------------------------

gdp <- read.csv("nominalgdp.csv")
gdp <- gdp[1:29,1:2] %>% 
  mutate(gdp=str_remove_all(gdp,"[^\\d]") %>% as.numeric())

adat <- left_join(adat,gdp)
padat <- pdata.frame(adat,index = c("industry","year"))

cor(adat$quality,adat$gdp)

# write.csv(tmp,"maindata.csv",fileEncoding = "UTF-8",row.names = F)
# write.csv(adat,"averagedata.csv",fileEncoding = "UTF-8",row.names = F)

