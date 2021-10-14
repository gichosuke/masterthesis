library(tidyverse)
dat <- read.csv("maindata.csv")
adat <- read.csv("averagedata.csv")


# VAR Granger -------------------------------------------------------------

library(vars)
needs::prioritize(dplyr)

gdat <- dat %>% filter(year>1991) %>% group_by(year) %>% 
  mutate(total_quantity=sum(quantity),total_sales=sum(investment),total_price=total_sales/total_quantity,
         total_firm=sum(firm,na.rm = T)) %>% 
  slice_head() %>% ungroup() %>% arrange(year) %>% 
  dplyr::select(total_quantity,total_sales,total_price,total_firm)

gqf <- gdat %>% dplyr::select(total_quantity,total_firm)
gsf <- gdat %>% dplyr::select(total_sales,total_firm)
gpf <- gdat %>% dplyr::select(total_price,total_firm)

VARselect(gqf)
VARselect(gsf)
VARselect(gpf)

gqf_var <- VAR(gqf,p=7)
gsf_var <- VAR(gsf,p=7)
gpf_var <- VAR(gpf,p=7)

causality(gpf_var,cause = "total_firm")
causality(gpf_var,cause = "total_price")

causality(gqf_var,cause = "total_firm")
causality(gqf_var,cause = "total_quantity")

causality(gsf_var,cause = "total_firm")
causality(gsf_var,cause = "total_sales")

agdat <- dat %>% filter(year>1991) %>% group_by(year) %>% 
  mutate(total_firm=sum(firm,na.rm = T),average_quantity=sum(quantity)/total_firm,
         average_sales=sum(investment)/total_firm,average_price=average_sales/average_quantity,
         ) %>% 
  slice_head() %>% ungroup() %>% arrange(year)

agqf <- agdat %>% dplyr::select(average_quantity,total_firm)
agsf <- agdat %>% dplyr::select(average_sales,total_firm)
agpf <- agdat %>% dplyr::select(average_price,total_firm)

VARselect(agqf)
VARselect(agsf)
VARselect(agpf)

agqf_var <- VAR(agqf,p=7)
agsf_var <- VAR(agsf,p=7)
agpf_var <- VAR(agpf,p=7)

causality(agpf_var,cause = "total_firm")
causality(agpf_var,cause = "average_price")

causality(agqf_var,cause = "total_firm")
causality(agqf_var,cause = "average_quantity")

causality(agsf_var,cause = "total_firm")
causality(agsf_var,cause = "average_sales")

# price moves faster!

library(panelvar)
library(plm)

vadat <- adat %>% filter(year>1991) %>%
  group_by(industry) %>% mutate(obs=n_distinct(year)) %>% ungroup() %>% filter(obs==max(obs)) %>% 
  mutate(indusindex=as.factor(industry) %>% as.numeric(),
         year=as.numeric(year)) %>% 
  select(indusindex,year,firm,price)

padat <- pdata.frame(adat,index = c("industry","year"))

polm1 <- lm(log(firm)~log(lag(price))+log(sales),data = adat)
palm1 <- plm(log(firm)~log(lag(quantity))+log(sales),effect = "twoways",model = "within",data = padat)
summary(palm1)
palm2 <- plm(log(firm)~log(lag(price))+log(sales),effect = "twoways",model = "within",data = padat)
summary(palm2)
palm3 <- plm(log(firm)~log(lag(price))+log(sales),effect = "twoways",model = "random",data = padat)
summary(palm3)

pFtest(palm2,polm1)
phtest(palm2,palm3)

# F test and Hausman test support random effect model (GLS)

polm2 <- lm(log(firm)~log(price)+log(sales),data = adat)
palm4 <- plm(log(firm)~log(price)+log(sales),effect = "twoways",model = "within",data = padat)
summary(palm2)
palm5 <- plm(log(firm)~log(price)+log(sales),effect = "twoways",model = "random",data = padat)
summary(palm5)
palm6 <- plm(log(firm)~log(quantity)+log(sales),effect = "twoways",model = "within",data = padat)
summary(palm6)

pFtest(palm4,polm2)
phtest(palm4,palm5)

res5 <- resid(palm5)
cor(res5,pdat$price)

res6 <- resid(palm6)
cor(res6,pdat$quantity)


# olddata ----------------------------------------------------------------

old <- read.csv("oldmergedata.csv")
tr <- read.csv("inpmaster.csv")
for (i in 1:nrow(tr)) {
  old$industry[old$industry==tr$jpi[i]] <- tr$engi[i] 
}
old$industry[old$industry=="Motor vehicles"] <- "Motor Vehicles"
old$industry[old$industry=="その他の輸送用機械器具製造業"] <- "Miscellaneous transportation equipment"

stock <- old %>% group_by(year,industry) %>% mutate(toq=sum(quantity,na.rm=T),tos=sum(investment,na.rm = T),top=tos/toq) %>% 
  slice_head() %>% ungroup() %>% arrange(industry,year)

stock <- stock %>% group_by(industry) %>% mutate(obs=n_distinct(year)) %>% ungroup() %>% 
  filter(obs==max(obs)) %>% select(-obs) %>% arrange(industry,year)

stock12 <- stock %>% mutate(stocki=toi,stockq=toq)

for (i in 1:(nrow(stock12)-12)) {
  for (j in 1:12) {
    if (stock12$industry[i+j]==stock12$industry[i]){
    stock12$stocki[i+j] <- stock12$stocki[i+j]+stock12$toi[i]
    stock12$stockq[i+j] <- stock12$stockq[i+j]+stock12$toq[i]
    }
  }
}

fdat <- dat %>% group_by(year,industry) %>% slice_head() %>% ungroup()

fdat12 <- left_join(dat,stock12,by=c("year","industry")) %>% 
  filter(!is.na(stocki))

fadat12 <- left_join(adat,stock12,by=c("year","industry")) %>% 
  filter(!is.na(stocki)) %>% mutate(stocki=stocki/firm,
                             stockq=stockq/firm,
                             top=top/firm,tos=tos/firm) %>% filter(year>1991)

pfdat <- pdata.frame(fdat12,index = c("industry","year"))
pfadat <- pdata.frame(fadat12,index = c("industry","year"))


splm1 <- plm(log(firm)~log(stockq)+log(sales),data = pfadat,model = "within",effect = "twoways")
summary(splm1)
splm2 <- plm(log(firm)~log(stockq)+log(sales),data = pfadat,model = "pooling",effect = "twoways")
summary(splm2)

pFtest(splm1,splm2)
splm3 <- plm(log(firm)~log(stockq)+log(sales),data = pfadat,model = "random",effect = "twoways")
phtest(splm1,splm3)
cor(resid(splm1),log(pfadat$stockq))
summary(splm3)
cor(resid(splm3),log(pfadat$stockq))

splm4 <- plm(log(firm)~log(stocki)+log(sales),data = pfadat,model = "within",effect = "twoways")
summary(splm4)
cor(resid(splm4),log(pfadat$stocki))

plmtest(splm3,effect = "twoways","bp")

cor(resid(splm2),log(pfadat$stockq))

qfadat <- fadat12 %>% mutate(quality=top/boj_price,realq=quality*toq,stockq=realq) %>% 
  arrange(industry,year)
for (i in 1:(nrow(qfadat)-12)) {
  for (j in 1:12) {
    if (qfadat$industry[i+j]==qfadat$industry[i]){
      qfadat$stockq[i+j] <- qfadat$stockq[i+j]+qfadat$realq[i]
    }
  }
}
pqfa <- pdata.frame(qfadat,index = c("industry","year"))

qplm1 <- plm(log(firm)~log(stockq)+log(sales),data = pqfa,model = "within",effect = "twoways")
summary(qplm1)
qplm2 <- plm(log(firm)~log(stockq)+log(sales),data = pqfa,model = "pooling",effect = "twoways")
summary(qplm2)

pFtest(qplm1,qplm2)
qplm3 <- plm(log(firm)~log(stockq)+log(sales),data = pqfa,model = "random",effect = "twoways")
phtest(qplm1,qplm3)
cor(resid(qplm1),log(pqfa$stockq))

plmtest(qplm3,effect = "twoways","bp")


# robustness check --------------------------------------------------------
RT <- data.frame(life_length=5:20,Model=0,Coef=0,P_val=0,Cor=0)
QT <- data.frame(life_length=5:20,Model=0,Coef=0,P_val=0,Cor=0)
for (y in 5:20) {
  stocktmp <- stock %>% mutate(stocki=toi,stockq=toq)
  for (i in 1:(nrow(stocktmp)-y)) {
    for (j in 1:y) {
      if (stocktmp$industry[i+j]==stocktmp$industry[i]){
        stocktmp$stocki[i+j] <- stocktmp$stocki[i+j]+stocktmp$toi[i]
        stocktmp$stockq[i+j] <- stocktmp$stockq[i+j]+stocktmp$toq[i]
      }
    }
  }
  fadattmp <- left_join(adat,stocktmp,by=c("year","industry")) %>% 
    filter(!is.na(stocki)) %>% mutate(stocki=stocki/firm,
                                      stockq=stockq/firm,
                                      top=top/firm,tos=tos/firm) %>% filter(year>1991)
  
  pfadattmp <- pdata.frame(fadattmp,index = c("industry","year"))
  
  tplm1 <- plm(log(firm)~log(stockq)+log(sales),data = pfadattmp,model = "within",effect = "twoways")
  tplm2 <- plm(log(firm)~log(stockq)+log(sales),data = pfadattmp,model = "pooling",effect = "twoways")
  
  if (pFtest(tplm1,tplm2)$p.value<0.1) {
    RT$Model[y-4] <- "Fixed Effect"
    RT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    RT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
    RT$Cor[y-4] <- cor(resid(tplm1),log(pfadattmp$stockq))
    tplm3 <- plm(log(firm)~log(stockq)+log(sales),data = pfadattmp,model = "random",effect = "twoways")
  if (phtest(tplm1,tplm3)$p.value>0.1){
    RT$Model[y-4] <- "Random Effect"
    RT$Coef[y-4] <- summary(tplm3)$coefficients[2,1]
    RT$P_val[y-4] <- summary(tplm3)$coefficients[2,4]
    RT$Cor[y-4] <- cor(resid(tplm3),log(pfadattmp$stockq))
  }
  } else{
    RT$Model[y-4] <- "Pooling"
    RT$Coef[y-4] <- summary(tplm2)$coefficients[2,1]
    RT$P_val[y-4] <- summary(tplm2)$coefficients[2,4]
    RT$Cor[y-4] <- cor(resid(tplm2),log(pfadattmp$stockq))
  }
  
  qfadattmp <- fadattmp %>% mutate(quality=top/boj_price,realq=quality*toq,stockq=realq) %>% 
    arrange(industry,year)
  for (i in 1:(nrow(qfadattmp)-y)) {
    for (j in 1:y) {
      if (qfadattmp$industry[i+j]==qfadattmp$industry[i]){
        qfadattmp$stockq[i+j] <- qfadattmp$stockq[i+j]+qfadattmp$realq[i]
      }
    }
  }
  pqfatmp <- pdata.frame(qfadattmp,index = c("industry","year"))
  tplm1 <- plm(log(firm)~log(stockq)+log(sales),data = pqfatmp,model = "within",effect = "twoways")
  tplm2 <- plm(log(firm)~log(stockq)+log(sales),data = pqfatmp,model = "pooling",effect = "twoways")
  tplm3 <- plm(log(firm)~log(stockq)+log(sales),data = pqfatmp,model = "random",effect = "twoways")
  
  if (pFtest(tplm1,tplm2)$p.value<0.1) {
    QT$Model[y-4] <- "Fixed Effect"
    QT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    QT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
    QT$Cor[y-4] <- cor(resid(tplm1),log(pqfatmp$stockq))
    if (phtest(tplm1,tplm3)$p.value>0.1){
      QT$Model[y-4] <- "Random Effect"
      QT$Coef[y-4] <- summary(tplm3)$coefficients[2,1]
      QT$P_val[y-4] <- summary(tplm3)$coefficients[2,4]
      QT$Cor[y-4] <- cor(resid(tplm3),log(pqfatmp$stockq))
    }
  } else{
    QT$Model[y-4] <- "Pooling"
    QT$Coef[y-4] <- summary(tplm2)$coefficients[2,1]
    QT$P_val[y-4] <- summary(tplm2)$coefficients[2,4]
    QT$Cor[y-4] <- cor(resid(tplm2),log(pqfatmp$stockq))
  }
  print(y)
}

xtable::xtable(RT) %>% print(include.rownames=F)
xtable::xtable(QT) %>% print(include.rownames=F)


# more accurate -----------------------------------------------------------

# newdata ----------------------------------------------------------------

new <- read.csv("newmergedata.csv")
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
np <- np[1:8]
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


pfdat <- pdata.frame(fdat12,index = c("industry","year"))
pfadat <- pdata.frame(fadat12,index = c("industry","year"))


splm1 <- plm(log(firm)~log(qstock)+log(sales)+log(import),data = pfadat,model = "within",effect = "twoways")
summary(splm1)
boxs1 <- data.frame(indsutry=rep(0,length(unique(pfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(pfadat)) {
  if (fadat12$year[r]==2001 ){
    boxs1$Cor[st] <- cor(resid(splm1)[r:(r+17)],log(pfadat$qstock[r:(r+17)]))
    boxs1$indsutry[st] <- fadat12$industry[r]
    st <- st+1
  }
}

test <- plm(log(qstock)~log(pin)+log(sales)+log(import),data = pfadat,model = "within",effect = "twoways")
summary(test)

spplm <- plm(log(firm)~log(pin)+log(sales)+log(import),data = pfadat,model = "within",effect = "twoways")
summary(spplm)
boxp1 <- data.frame(indsutry=rep(0,length(unique(pfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(pfadat)) {
  if (fadat12$year[r]==2001 ){
    boxp1$Cor[st] <- cor(resid(spplm)[r:(r+17)],log(pfadat$pin[r:(r+17)]))
    boxp1$indsutry[st] <- fadat12$industry[r]
    st <- st+1
  }
}

splm2 <- plm(log(firm)~log(qstock)+log(sales)+log(import),data = pfadat,model = "pooling",effect = "twoways")
summary(splm2)

pFtest(splm1,splm2)
splm3 <- plm(log(firm)~log(qstock)+log(sales)+log(import),data = pfadat,model = "random",effect = "twoways")
phtest(splm1,splm3)
cor(resid(splm1),log(pfadat$stockq))
summary(splm3)
cor(resid(splm3),log(pfadat$stockq))

plmtest(splm3,effect = "twoways","bp")

cor(resid(splm2),log(pfadat$stockq))

qplm1 <- plm(log(firm)~log(rqstock)+log(sales)+log(import),data = pfadat,model = "within",effect = "twoways")
summary(qplm1)
boxq1 <- data.frame(indsutry=rep(0,length(unique(pfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(pfadat)) {
  if (fadat12$year[r]==2001 ){
    boxq1$Cor[st] <- cor(resid(qplm1)[r:(r+17)],log(pfadat$rqstock[r:(r+17)]))
    boxq1$indsutry[st] <- fadat12$industry[r]
    st <- st+1
  }
}
qplm2 <- plm(log(firm)~log(rqstock)+log(sales)+log(import),data = pfadat,model = "pooling",effect = "twoways")
summary(qplm2)

pFtest(qplm1,qplm2)
qplm3 <- plm(log(firm)~log(rqstock)+log(sales),data = pfadat,model = "random",effect = "twoways")
phtest(qplm1,qplm3)
cor(resid(qplm1),log(pfadat$stockq))
summary(qplm3)
plmtest(qplm3,effect = "twoways","bp")


# robustness check --------------------------------------------------------
RT <- data.frame(life_length=5:20,Model=0,Coef=0,P_val=0,Cor=0)
QT <- data.frame(life_length=5:20,Model=0,Coef=0,P_val=0,Cor=0)
for (y in 5:20) {
  stocktmp <- left_join(stock,stand) %>% mutate(stockrq=realq,stockq=toq) %>% 
    arrange(industry,purpose,year)
  for (i in 1:(nrow(stocktmp)-y)) {
    for (j in 1:y) {
      if (stocktmp$industry[i+j]==stocktmp$industry[i]&
          stocktmp$purpose[i+j]==stocktmp$purpose[i]){
        stocktmp$stockrq[i+j] <- stocktmp$stockrq[i+j]+stocktmp$realq[i]
        stocktmp$stockq[i+j] <- stocktmp$stockq[i+j]+stocktmp$toq[i]
      }
    }
  }
  stocktmp <- stocktmp %>% mutate(stockrq=stockrq^ratio,stockq=stockq^ratio) %>% 
    group_by(year,industry) %>% mutate(rqstock=sum(stockrq,na.rm = T),qstock=sum(stockq,na.rm = T)) %>% 
    slice_head() %>% ungroup()
    fadattmp <- left_join(adat,stocktmp,by=c("year","industry")) %>% 
    filter(rqstock!=0) %>% mutate(rqstock=rqstock/firm,
                                  qstock=qstock/firm) %>% filter(year>=1989+y)
  
  pfadattmp <- pdata.frame(fadattmp,index = c("industry","year"))
  
  tplm1 <- plm(log(firm)~log(qstock)+log(sales),data = pfadattmp,model = "within",effect = "twoways")
  tplm2 <- plm(log(firm)~log(qstock)+log(sales),data = pfadattmp,model = "pooling",effect = "twoways")
  tplm3 <- plm(log(firm)~log(qstock)+log(sales),data = pfadattmp,model = "random",effect = "twoways")
  
  if (pFtest(tplm1,tplm2)$p.value<0.1) {
    RT$Model[y-4] <- "Fixed Effect"
    RT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    RT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
    RT$Cor[y-4] <- cor(resid(tplm1),log(pfadattmp$qstock))
    if (phtest(tplm1,tplm3)$p.value>0.1){
      RT$Model[y-4] <- "Random Effect"
      RT$Coef[y-4] <- summary(tplm3)$coefficients[2,1]
      RT$P_val[y-4] <- summary(tplm3)$coefficients[2,4]
      RT$Cor[y-4] <- cor(resid(tplm3),log(pfadattmp$qstock))
    }
  } else{
    RT$Model[y-4] <- "Pooling"
    RT$Coef[y-4] <- summary(tplm2)$coefficients[2,1]
    RT$P_val[y-4] <- summary(tplm2)$coefficients[2,4]
    RT$Cor[y-4] <- cor(resid(tplm2),log(pfadattmp$qstock))
  }
  tplm1 <- plm(log(firm)~log(rqstock)+log(sales),data = pfadattmp,model = "within",effect = "twoways")
  tplm2 <- plm(log(firm)~log(rqstock)+log(sales),data = pfadattmp,model = "pooling",effect = "twoways")
  tplm3 <- plm(log(firm)~log(rqstock)+log(sales),data = pfadattmp,model = "random",effect = "twoways")
  
  if (pFtest(tplm1,tplm2)$p.value<0.1) {
    QT$Model[y-4] <- "Fixed Effect"
    QT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    QT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
    QT$Cor[y-4] <- cor(resid(tplm1),log(pfadattmp$rqstock))
    if (phtest(tplm1,tplm3)$p.value>0.1){
      QT$Model[y-4] <- "Random Effect"
      QT$Coef[y-4] <- summary(tplm3)$coefficients[2,1]
      QT$P_val[y-4] <- summary(tplm3)$coefficients[2,4]
      QT$Cor[y-4] <- cor(resid(tplm3),log(pfadattmp$rqstock))
    }
  } else{
    QT$Model[y-4] <- "Pooling"
    QT$Coef[y-4] <- summary(tplm2)$coefficients[2,1]
    QT$P_val[y-4] <- summary(tplm2)$coefficients[2,4]
    QT$Cor[y-4] <- cor(resid(tplm2),log(pfadattmp$rqstock))
  }
  print(y)
}

xtable::xtable(RT[,1:4]) %>% print(include.rownames=F)
xtable::xtable(QT[,1:4]) %>% print(include.rownames=F)


# take lag -------------------------------------------------------------------

# newdata ----------------------------------------------------------------

lfadat <- fadat12 %>% arrange(year) %>% group_by(industry) %>% 
  mutate(lqstock=lag(log(qstock)),lrqstock=lag(log(rqstock)),lim=lag(log(import)),lpin=log(lag(pin)),
                             lsales=lag(log(sales)),lgc=lag(log(group_company)),
                             lsub=lag(log(subsidiary))) %>% 
  filter(year>2001) %>% ungroup() %>% filter(!is.na(lim))
lfadat <- lfadat %>% arrange(industry,year)
lpfadat <- pdata.frame(lfadat,index = c("industry","year"))
lsplm1 <- plm(log(firm)~lqstock+lsales+lim,data = lpfadat,model = "within",effect = "twoways")
summary(lsplm1)
lsplm2 <- plm(log(firm)~lqstock+lsales+lgc+lsub,data = lpfadat,model = "pooling",effect = "twoways")
summary(lsplm2)
lsplm1 <- plm(log(firm)~lpin+lsales+lim,data = lpfadat,model = "within",effect = "twoways")
summary(lsplm1)

pFtest(lsplm1,lsplm2)
lsplm3 <- plm(log(firm)~lqstock+lsales+lgc+lsub,data = lpfadat,model = "random",effect = "twoways")
summary(lsplm3)
phtest(lsplm1,lsplm3)
plmtest(splm3,effect = "twoways","bp")

boxl1 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxl1$Cor[st] <- cor(resid(lsplm1)[r:(r+16)],lpfadat$lpin[r:(r+16)])
    boxl1$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}


cor(resid(splm2),log(pfadat$stockq))

lqplm1 <- plm(log(firm)~lrqstock+lsales+lim,data = lpfadat,model = "within",effect = "twoways")
summary(lqplm1)
lqplm2 <- plm(log(firm)~lrqstock+lsales+lgc+lsub,data = lpfadat,model = "pooling",effect = "twoways")
summary(lqplm2)

pFtest(lqplm1,lqplm2)
lqplm3 <- plm(log(firm)~lrqstock+lsales+lgc+lsub,data = lpfadat,model = "random",effect = "twoways")
phtest(lqplm1,lqplm3)
summary(lqplm3)
plmtest(lqplm3,effect = "twoways","bp")

boxl2 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxl2$Cor[st] <- cor(resid(lqplm1)[r:(r+16)],lpfadat$lrqstock[r:(r+16)])
    boxl2$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}


# robustness check --------------------------------------------------------
lRT <- data.frame(life_length=5:20,Model=0,Coef=0,P_val=0)
lQT <- data.frame(life_length=5:20,Model=0,Coef=0,P_val=0)
for (y in 5:20) {
  stocktmp <- left_join(stock,stand) %>% mutate(stockrq=realq,stockq=toq) %>% 
    arrange(industry,purpose,year)
  
  for (i in 1:(nrow(stocktmp)-y)) {
    for (j in 1:y) {
      if (stocktmp$industry[i+j]==stocktmp$industry[i]&
          stocktmp$purpose[i+j]==stocktmp$purpose[i]){
        stocktmp$stockrq[i+j] <- stocktmp$stockrq[i+j]+stocktmp$realq[i]
        stocktmp$stockq[i+j] <- stocktmp$stockq[i+j]+stocktmp$toq[i]
      }
    }
  }
  stocktmp <- stocktmp %>% mutate(stockrq=stockrq^ratio,stockq=stockq^ratio) %>% 
    group_by(year,industry) %>% mutate(rqstock=sum(stockrq,na.rm = T),qstock=sum(stockq,na.rm = T)) %>% 
    slice_head() %>% ungroup()
  fadattmp <- left_join(adat,stocktmp,by=c("year","industry")) %>% 
    filter(rqstock!=0) %>% mutate(rqstock=rqstock/firm,
                                  qstock=qstock/firm) %>% filter(year>=1989+y)
  fadattmp <- fadattmp %>% arrange(year) %>% group_by(industry) %>% 
    mutate(lqstock=lag(log(qstock)),lrqstock=lag(log(rqstock)),
           lsales=lag(log(sales)),lgc=lag(log(group_company)),
           lsub=lag(log(subsidiary))) %>% 
    filter(year>1989+y) %>% ungroup()
  fadattmp <- fadattmp %>% arrange(industry,year)
  pfadattmp <- pdata.frame(fadattmp,index = c("industry","year"))
  
  tplm1 <- plm(log(firm)~lqstock+lsales+lgc+lsub,data = pfadattmp,model = "within",effect = "twoways")
  tplm2 <- plm(log(firm)~lqstock+lsales+lgc+lsub,data = pfadattmp,model = "pooling",effect = "twoways")
  tplm3 <- plm(log(firm)~lqstock+lsales+lgc+lsub,data = pfadattmp,model = "random",effect = "twoways")
  
  if (pFtest(tplm1,tplm2)$p.value<0.1) {
    lRT$Model[y-4] <- "Fixed Effect"
    lRT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    lRT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
    
    if (phtest(tplm1,tplm3)$p.value>0.1){
      lRT$Model[y-4] <- "Random Effect"
      lRT$Coef[y-4] <- summary(tplm3)$coefficients[2,1]
      lRT$P_val[y-4] <- summary(tplm3)$coefficients[2,4]
      
    }
  } else{
    lRT$Model[y-4] <- "Pooling"
    lRT$Coef[y-4] <- summary(tplm2)$coefficients[2,1]
    lRT$P_val[y-4] <- summary(tplm2)$coefficients[2,4]
    
  }
  tplm1 <- plm(log(firm)~lrqstock+lsales+lgc+lsub,data = pfadattmp,model = "within",effect = "twoways")
  tplm2 <- plm(log(firm)~lrqstock+lsales+lgc+lsub,data = pfadattmp,model = "pooling",effect = "twoways")
  tplm3 <- plm(log(firm)~lrqstock+lsales+lgc+lsub,data = pfadattmp,model = "random",effect = "twoways")
  
  if (pFtest(tplm1,tplm2)$p.value<0.1) {
    lQT$Model[y-4] <- "Fixed Effect"
    lQT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    lQT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
    
    if (phtest(tplm1,tplm3)$p.value>0.1){
      lQT$Model[y-4] <- "Random Effect"
      lQT$Coef[y-4] <- summary(tplm3)$coefficients[2,1]
      lQT$P_val[y-4] <- summary(tplm3)$coefficients[2,4]
      
    }
  } else{
    lQT$Model[y-4] <- "Pooling"
    lQT$Coef[y-4] <- summary(tplm2)$coefficients[2,1]
    lQT$P_val[y-4] <- summary(tplm2)$coefficients[2,4]
    
  }
  print(y)
}

xtable::xtable(lRT) %>% print(include.rownames=F)
xtable::xtable(lQT) %>% print(include.rownames=F)


# fixed only --------------------------------------------------------------

FRT <- data.frame(life_length=5:20,Coef=0,P_val=0)
FQT <- data.frame(life_length=5:20,Coef=0,P_val=0)
for (y in 5:20) {
  stocktmp <- left_join(stock,stand) %>% mutate(stockrq=realq,stockq=toq) %>% distinct_all() %>% 
    arrange(industry,purpose,year)
  for (i in 1:(nrow(stocktmp)-y)) {
    for (j in 1:y) {
      if (stocktmp$industry[i+j]==stocktmp$industry[i]&
          stocktmp$purpose[i+j]==stocktmp$purpose[i]){
        stocktmp$stockrq[i+j] <- stocktmp$stockrq[i+j]+stocktmp$realq[i]
        stocktmp$stockq[i+j] <- stocktmp$stockq[i+j]+stocktmp$toq[i]
      }
    }
  }
  stocktmp <- stocktmp %>% mutate(stockrq=stockrq^ratio,stockq=stockq^ratio) %>% 
    group_by(year,industry) %>% mutate(rqstock=prod(stockrq,na.rm = T),qstock=prod(stockq,na.rm = T)) %>% 
    slice_head() %>% ungroup()
  fadattmp <- left_join(adat,stocktmp,by=c("year","industry")) %>% 
    filter(rqstock!=0) %>% mutate(rqstock=rqstock/firm,
                                  qstock=qstock/firm) %>% filter(year>=1989+y)
  
  pfadattmp <- pdata.frame(fadattmp,index = c("industry","year"))
  
  tplm1 <- plm(log(firm)~log(qstock)+log(sales)+log(import),data = pfadattmp,model = "within",effect = "twoways")

    FRT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    FRT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
  tplm1 <- plm(log(firm)~log(rqstock)+log(sales)+log(import),data = pfadattmp,model = "within",effect = "twoways")

    FQT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    FQT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
  print(y)
}

xtable::xtable(FRT) %>% print(include.rownames=F)
xtable::xtable(FQT) %>% print(include.rownames=F)


# fl ----------------------------------------------------------------------

flRT <- data.frame(life_length=5:20,Coef=0,P_val=0)
flQT <- data.frame(life_length=5:20,Coef=0,P_val=0)
for (y in 5:20) {
  stocktmp <- left_join(stock,stand) %>% mutate(stockrq=realq,stockq=toq) %>% distinct_all() %>% 
    arrange(industry,purpose,year)
  
  for (i in 1:(nrow(stocktmp)-y)) {
    for (j in 1:y) {
      if (stocktmp$industry[i+j]==stocktmp$industry[i]&
          stocktmp$purpose[i+j]==stocktmp$purpose[i]){
        stocktmp$stockrq[i+j] <- stocktmp$stockrq[i+j]+stocktmp$realq[i]
        stocktmp$stockq[i+j] <- stocktmp$stockq[i+j]+stocktmp$toq[i]
      }
    }
  }
  stocktmp <- stocktmp %>% mutate(stockrq=stockrq^ratio,stockq=stockq^ratio) %>% 
    group_by(year,industry) %>% mutate(rqstock=prod(stockrq,na.rm = T),qstock=prod(stockq,na.rm = T)) %>% 
    slice_head() %>% ungroup()
  fadattmp <- left_join(adat,stocktmp,by=c("year","industry")) %>% 
    filter(rqstock!=0) %>% mutate(rqstock=rqstock/firm,
                                  qstock=qstock/firm) %>% filter(year>=1989+y)
  fadattmp <- fadattmp %>% arrange(year) %>% group_by(industry) %>% 
    mutate(lqstock=lag(log(qstock)),lrqstock=lag(log(rqstock)),
           lsales=lag(log(sales)),lgc=lag(log(group_company)),lim=lag(log(import)),
           lsub=lag(log(subsidiary))) %>% 
    filter(year>1989+y) %>% ungroup()
  fadattmp <- fadattmp %>% arrange(industry,year)
  pfadattmp <- pdata.frame(fadattmp,index = c("industry","year"))
  
  tplm1 <- plm(log(firm)~lqstock+lsales+lim,data = pfadattmp,model = "within",effect = "twoways")

    flRT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    flRT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
  tplm1 <- plm(log(firm)~lrqstock+lsales+lim,data = pfadattmp,model = "within",effect = "twoways")

    flQT$Coef[y-4] <- summary(tplm1)$coefficients[1,1]
    flQT$P_val[y-4] <- summary(tplm1)$coefficients[1,4]
  print(y)
}

xtable::xtable(flRT) %>% print(include.rownames=F)
xtable::xtable(flQT) %>% print(include.rownames=F)


# chg rate ----------------------------------------------------------------

cfadat <- fadat12 %>% arrange(year) %>% group_by(industry) %>% 
  mutate(cfirm=log(firm)-lag(log(firm)),
    cqstock=log(qstock)-lag(log(qstock)),crqstock=log(rqstock)-lag(log(rqstock)),cpin=log(pin)-log(lag(pin)),lpin=lag(pin),
         csales=log(sales)-lag(log(sales)),cgc=log(group_company)-lag(log(group_company)),
         csub=log(subsidiary)-lag(log(subsidiary)),lim=log(lag(import)),cim=import-lim,
    lqs=log(lag(qstock)),lrqs=log(lag(rqstock)),ls=log(lag(sales)),
    lgc=lag(log(group_company)),lsub=lag(log(subsidiary))) %>% 
  filter(year>2001) %>% ungroup() %>% arrange(industry,year)
cpfadat <- pdata.frame(cfadat,index = c("industry","year"))
csplm1 <- plm(cfirm~cqstock+csales+cim,data = cpfadat,model = "within",effect = "twoways")
summary(csplm1)

boxc1 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxc1$Cor[st] <- cor(resid(csplm1)[r:(r+16)],cpfadat$cqstock[r:(r+16)])
    boxc1$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}

csplm2 <- plm(cfirm~crqstock+csales+cim,data = cpfadat,model = "within",effect = "twoways")
summary(csplm2)
boxc2 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxc2$Cor[st] <- cor(resid(csplm2)[r:(r+16)],cpfadat$crqstock[r:(r+16)])
    boxc2$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}

csplm2 <- plm(cfirm~cpin+csales+cim,data = cpfadat,model = "within",effect = "twoways")
summary(csplm2)
boxc2 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxc2$Cor[st] <- cor(resid(csplm2)[r:(r+16)],cpfadat$cpin[r:(r+16)])
    boxc2$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}

csplm3 <- plm(cfirm~lqs+ls+lim,data = cpfadat,model = "within",effect = "twoways")
summary(csplm3)
boxc3 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxc3$Cor[st] <- cor(resid(csplm3)[r:(r+16)],cpfadat$lqs[r:(r+16)])
    boxc3$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}
csplm4 <- plm(cfirm~lrqs+ls+lim,data = cpfadat,model = "within",effect = "twoways")
summary(csplm4)
boxc4 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxc4$Cor[st] <- cor(resid(csplm2)[r:(r+16)],cpfadat$lrqs[r:(r+16)])
    boxc4$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}

csplm5 <- plm(cfirm~lpin+ls+lim,data = cpfadat,model = "within",effect = "twoways")
summary(csplm5)
boxc5 <- data.frame(indsutry=rep(0,length(unique(lpfadat$industry))),Cor=0)
st <- 1
for (r in 1:nrow(lpfadat)) {
  if (lfadat$year[r]==2002 ){
    boxc5$Cor[st] <- cor(resid(csplm2)[r:(r+16)],cpfadat$lpin[r:(r+16)])
    boxc5$indsutry[st] <- lfadat$industry[r]
    st <- st+1
  }
}

Ind <- data.frame(Industry=unique(new$industry)[1:9],tmp=c(unique(new$industry)[c(10:16,18,19)],""))
APP <- data.frame(Application=unique(new$purpose))

xtable::xtable(Ind) %>% print(include.rownames=F)
xtable::xtable(APP) %>% print(include.rownames=F)


# import ------------------------------------------------------------------

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

#write_excel_csv(imp$industry %>% unique() %>% as.data.frame(),"impkey.csv")
jjara <- read.csv("impkey.csv")
for (j in 1:nrow(jjara)) {
  imp$industry[imp$industry==jjara$industry[j]] <- jjara$aks[j]
}
imp <- imp %>% group_by(industry,year) %>% mutate(import=sum(import %>% as.numeric(),na.rm = T)) %>% 
  slice_head() %>% ungroup()

write_excel_csv(imp,"aksimport.csv")
xtable::xtable(boxp1) %>% print(include.rownames=F)
