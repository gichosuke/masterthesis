# dat <- read_csv("maindata2.csv")
# ict <- read_csv("ICTstock.csv")
# ict <- ict %>% pivot_longer(cols = -year,names_to = "industry",values_to = "ict")
# imex <- read_csv("import_export.csv") %>%
#   transmute(industry=name,glob=import+export,year=year)
# dat <- left_join(dat,ict,by=c("year","industry")) %>% left_join(.,imex,by=c("year","industry"))

# write_csv(dat,"masterdata.csv")
source("rat.R")
library(openxlsx)
dat <- read_csv("masterdata.csv") %>% rename(sales=investment)

st7880 <- pim1(t=45,d=0.18,st=1978,ed=1980,real = T) %>% rename(stock7880=stock)
rat7880 <- rat(dat,st=1978,1980) %>% rename(pin7880=pin)
st9193 <- pim1(t=45,d=0.18,st=1991,ed=1993,real = T) %>% rename(stock9193=stock)
rat9193 <- rat(dat,st=1991,1993) %>% rename(pin9193=pin)

dat2 <- left_join(dat,st7880,by=c("industry","year")) %>% 
  left_join(.,rat7880,by=c("industry","year")) %>% 
  left_join(.,st9193,by=c("industry","year")) %>% 
  left_join(.,rat9193,by=c("industry","year")) %>% distinct(industry,year,.keep_all = T)

#  weak IV test done clear!

datp <- pdata.frame(dat,index = c("industry","year"))
