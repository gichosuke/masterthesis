# make data ---------------------------------------------------------------
source("rat.R")
dat <- read_csv("masterdata.csv") %>% rename(sales=investment)

st7880 <- pim1(t=45,d=0.18,st=1978,ed=1980,real = T) %>% rename(stock7880=stock)
rat7880 <- rat(dat,st=1978,1980) %>% rename(pin7880=pin)
st9193 <- pim1(t=45,d=0.18,st=1991,ed=1993,real = T) %>% rename(stock9193=stock)
rat9193 <- rat(dat,st=1991,1993) %>% rename(pin9193=pin)

dat2 <- left_join(dat,st7880,by=c("industry","year")) %>% 
  left_join(.,rat7880,by=c("industry","year")) %>% 
  left_join(.,st9193,by=c("industry","year")) %>% 
  left_join(.,rat9193,by=c("industry","year")) %>% distinct(industry,year,.keep_all = T)

# to do IV test -----------------------------------------------------------

tes <- dat2 
j <- 1
for (i in tes$industry %>% unique()){
  tes[paste0("flgi",j)] <- 0
  tes[tes$industry==i,paste0("flgi",j)] <- 1
  j <- j+1
}
j <- 1
for (i in tes$year %>% unique()){
  tes[paste0("flgt",j)] <- 0
  tes[tes$year==i,paste0("flgt",j)] <- 1
  j <- j+1
}

colnames(tes)[34:86] %>% paste0(collapse = "+") %>% print()

restmp7880 <- 
  AER::ivreg(log(stock7880+glob)~log(stock7880)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10+flgt11+flgt12+flgt13+flgt14+flgt15+flgt16+flgt17+flgt18+flgt19+
               flgt20+flgt21+flgt22+flgt23+flgt24+flgt25+flgt26+flgt27+flgt28+flgt29+flgt30+flgt31+
               flgt32+flgt33+flgt34+flgt35+flgt36+flgt37+flgt38+flgt39+flgt40+flgt41
           |log(pin7880)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
             flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
             flgt8+flgt9+flgt10+flgt11+flgt12+flgt13+flgt14+flgt15+flgt16+flgt17+flgt18+flgt19+
             flgt20+flgt21+flgt22+flgt23+flgt24+flgt25+flgt26+flgt27+flgt28+flgt29+flgt30+flgt31+
             flgt32+flgt33+flgt34+flgt35+flgt36+flgt37+flgt38+flgt39+flgt40+flgt41,
           data=tes)

summary(restmp7880,diagnostics = T)$diagnostics    # WEAK IV 29.59 8.91e-08

tes2 <- dat2 %>% filter(year>1993)
j <- 1
for (i in tes2$industry %>% unique()){
  tes2[paste0("flgi",j)] <- 0
  tes2[tes2$industry==i,paste0("flgi",j)] <- 1
  j <- j+1
}
j <- 1
for (i in tes2$year %>% unique()){
  tes2[paste0("flgt",j)] <- 0
  tes2[tes2$year==i,paste0("flgt",j)] <- 1
  j <- j+1
}

restmp9193 <- 
  AER::ivreg(log(stock9193+glob)~log(stock9193)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10+flgt11+flgt12+flgt13+flgt14+flgt15+flgt16+flgt17+flgt18+flgt19+
               flgt20+flgt21+flgt22+flgt23+flgt24+flgt25
             |log(pin9193)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10+flgt11+flgt12+flgt13+flgt14+flgt15+flgt16+flgt17+flgt18+flgt19+
               flgt20+flgt21+flgt22+flgt23+flgt24+flgt25,
             data=tes2)

summary(restmp9193,diagnostics = T)$diagnostics    # WEAK IV 1.915 0.16763 

restmp9193d <- 
  AER::ivreg(log(stock9193+glob)~log(stock7880)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10+flgt11+flgt12+flgt13+flgt14+flgt15+flgt16+flgt17+flgt18+flgt19+
               flgt20+flgt21+flgt22+flgt23+flgt24+flgt25
             |log(pin7880)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10+flgt11+flgt12+flgt13+flgt14+flgt15+flgt16+flgt17+flgt18+flgt19+
               flgt20+flgt21+flgt22+flgt23+flgt24+flgt25,
             data=tes2)

summary(restmp9193d,diagnostics = T)$diagnostics    # WEAK IV 3.816 0.05184 .
# 1980's IV test ----------------------------------------------------------

tes3 <- dat2 %>% filter(between(year,1980,1989))
j <- 1
for (i in tes3$industry %>% unique()){
  tes3[paste0("flgi",j)] <- 0
  tes3[tes3$industry==i,paste0("flgi",j)] <- 1
  j <- j+1
}
j <- 1
for (i in tes3$year %>% unique()){
  tes3[paste0("flgt",j)] <- 0
  tes3[tes3$year==i,paste0("flgt",j)] <- 1
  j <- j+1
}

restmp80 <- 
  AER::ivreg(log(stock9193+glob)~log(stock7880)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10
             |log(pin7880)+log(glob)+log(ict)+flgi1+flgi2+flgi3+flgi4+flgi5+flgi6+
               flgi7+flgi8+flgi9+flgi10+flgi11+flgi12+flgt1+flgt2+flgt3+flgt4+flgt5+flgt6+flgt7+
               flgt8+flgt9+flgt10,
             data=tes3)

summary(restmp80,diagnostics = T)$diagnostics    # WEAK IV 30.58 1.95e-07 ***

# export price ------------------------------------------------------------

exdat <- read_csv("ExportByAPP.csv")
expin7880 <- exrat(st=1978,ed=1980)
expin9193 <- exrat(st=1991,ed=1993)
datex <- left_join(dat,st7880,by=c("industry","year")) %>% 
  left_join(.,expin7880,by=c("industry","year")) %>% 
  left_join(.,st9193,by=c("industry","year")) %>% 
  left_join(.,expin9193,by=c("industry","year")) %>% distinct(industry,year,.keep_all = T)

# #  whole data for stock and price ------------------------------------------
# 
# tes <- left_join(dat2 %>% select(-price),rat7880) %>% left_join(.,pim1(t=45,d=0.18,st=1978,ed=1980)) %>%
#   distinct(industry,year,.keep_all = T)
# tes2 <- tes %>% group_by(industry) %>% arrange(year) %>% filter(year<2018) %>%
#    mutate(year5=rep(1:(n()/5),5) %>% sort()) %>% group_by(industry,year5) %>%
#    mutate(across(.cols = where(is.numeric),.fns = ~mean(.,na.rm=T))) %>% slice_head() %>%
#    ungroup()
# 
# ptes <- pdata.frame(dat2,index = c("industry","year"))
# 
# summary(plm(log(stock7880)~log(pin7880)+
#               log(glob)+log(ict),data = ptes,effect = "twoways",model = "within"))
# 

# draw graphs of price and stock ---------------------------------------------------
dat2 %>% group_by(industry) %>% arrange(year) %>% 
  mutate(pricechg=log(pin7880)-log(dplyr::lag(pin7880)),
         stockchg=log(stock7880)-log(dplyr::lag(stock7880)),
         changesign=if_else(pricechg*stockchg>=0,1,0)) %>% ungroup() %>% group_by(year) %>% 
  mutate(samesignrate=mean(changesign)*100) %>% slice_head() %>% ungroup() %>% 
ggplot()+
  geom_line(aes(year,samesignrate))+
  theme_bw()

ggplot(dat2)+
  aes(year,log(stock7880),color=industry)+
  geom_line()+
  theme_bw()+
  geom_vline(xintercept = 1994,linetype="dashed")+
  ylab(label = "robot stock")

ggplot(dat2)+
  aes(year,log(pin7880),color=industry)+
  geom_line()+
  theme_bw()+
  geom_vline(xintercept = 1994,linetype="dashed")+
  ylab(label = "robot price")

pim1(d=0.10,st=1978,ed=1980,real = T) %>% 
ggplot()+
  aes(year,log(stock),color=industry)+
  geom_line()+
  theme_bw()

pim1(t = 45,d=0.10,st=1978,ed=1980,real = T) %>% 
  ggplot()+
  aes(year,log(stock),color=industry)+
  geom_line()+
  theme_bw()

pim1(t = 45,d=0.10,st=1978,ed=1980,real = T) %>% group_by(year) %>% 
  mutate(stock=sum(stock)) %>% slice_head() %>% ungroup() %>% 
  ggplot()+
  aes(year,stock)+
  geom_line()+
  theme_bw()

dat %>% group_by(year,industry) %>% mutate(sales=sum(sales,na.rm = T)) %>% slice_head() %>% 
  ungroup() %>% 
  ggplot(.,aes(year,log(sales),color=industry))+
  geom_line()+
  theme_bw()
