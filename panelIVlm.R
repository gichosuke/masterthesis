source("rat.R")

rat9193 <- rat(st=1991,ed = 1993)
st12 <- pim1(st=1991,ed=1993)
rst12 <- pim1(st=1991,ed=1993,real = T) %>% rename(rstock=stock)
imp <- read_csv("aksimport.csv")
ag91 <- left_join(dat,rat9193) %>% left_join(.,st12) %>% left_join(.,rst12) %>% left_join(.,imp) %>% 
  filter(year>=1994)


# 91-93年を基準にして94-2018年のデータを --------------------------------------------------

library(plm)
p91 <- pdata.frame(ag91 %>% distinct(year,industry,.keep_all = T),index = c("industry","year"))

res1 <- plm(log(stock/firms)~log(pin)+log(net_sales/firms)+log(import/firms),
            effect = "twoways",model = "within",data = p91)
res2 <- plm(log(stock)~log(pin)+log(net_sales)+log(import),
            effect = "twoways",model = "within",data = p91)
res3 <- plm(log(stock)~log(pin)+log(net_sales/firms)+log(import),
            effect = "twoways",model = "within",data = p91)
summary(plm(log(pin)~log(import/firms),effect = "twoways",model = "within",data = p91))
summary(plm(log(stock)~log(pin),effect = "twoways",model = "within",data = p91))

summary(plm(log(firms)~log(pin)+log(net_sales),effect = "twoways",
            model = "within",data = p91))

summary(plm(log(firms)~log(pin)+operating_profit,effect = "twoways",
            model = "within",data = p91))
tmp <- p91 %>% mutate(op=operating_profit/firms)
summary(plm(log(firms)~log(pin)+op,effect = "twoways",
            model = "within",data = tmp))

summary(plm(log(firms)~log(lag(pin))+log(lag(net_sales)),effect = "twoways",
        model = "within",data = p91))
summary(plm(log(firms)~log(lag(pin))+log(lag(net_sales/firms)),effect = "twoways",
            model = "within",data = p91))

summary(plm(log(firms)~log(lag(pin))+lag(operating_profit),effect = "twoways",
            model = "within",data = p91))
tmp <- p91 %>% mutate(op=operating_profit/firms)
summary(plm(log(firms)~log(lag(pin))+lag(op),effect = "twoways",
            model = "within",data = tmp))

tes <- plm(log(firms)~log(stock/firms)+log(net_sales/firms)+log(import/firms)|
             log(net_sales/firms)+log(import/firms)+log(pin),
    effect = "twoways",model = "within",data = p91)
summary(tes)


# 一区切り --------------------------------------------------------------------

tmpdat <- left_join(ag91 %>% distinct(year,industry,.keep_all = T),ict)

ptmp <- pdata.frame(tmpdat,index = c("year","industry"))

plm(log(stock)~log(pin),data = ptmp,model = "within",effect = "twoways") %>% summary()
plm(log(rstock)~log(pin),data = ptmp,model = "within",effect = "twoways") %>% summary()
plm(log(stock/firms)~log(pin),data = ptmp,model = "within",effect = "twoways") %>% summary()
plm(log(stock/firms)~log(pin)+log(ict/firms),data = ptmp,model = "within",effect = "twoways") %>% summary()
plm(log(stock)~log(pin)+log(ict),data = ptmp,model = "within",effect = "twoways") %>% summary()

# main result -------------------------------------------------------------

plm(log(firms)~log(stock)+log(ict)|log(pin)+log(ict),
    data = ptmp,model = "within",effect = "twoways") %>% summary()
plm(log(firms)~log(stock/firms)+log(ict/firms)+log(import)|log(pin)+log(ict/firms)+log(import),
    data = ptmp,model = "within",effect = "twoways") %>% summary()

plm(log(firms)~log(stock/firms)+log(ict/firms)+log(net_sales/firms)+log(import)|
      log(pin)+log(ict/firms)+log(net_sales/firms)+log(import),
    data = ptmp,model = "within",effect = "twoways") %>% summary()

plm(log(offices)~log(stock/firms)+log(ict/firms)+log(net_sales/firms)+log(import)|
      log(pin)+log(ict/firms)+log(net_sales/firms)+log(import),
    data = ptmp,model = "within",effect = "twoways") %>% summary()

plm(log(employees/firms)~log(stock/firms)+log(ict/firms)+log(net_sales/firms)+log(import)|
      log(pin)+log(ict/firms)+log(net_sales/firms)+log(import),
    data = ptmp,model = "within",effect = "twoways") %>% summary()
