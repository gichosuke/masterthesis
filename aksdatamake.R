library(data.table)
library(tidyverse)
master <- read_csv("bsb.csv")
colnames(master) <- c("industry","firms","offices","employees","full_time_employees",
  "assets","current_assets","fixed_asset","differed_assets","liabilities_net_assets",
  "liabilities","net_assets","net_sales","gross_profit","operating_costs",
  "operating_profit","ordinary_profit","net_profit","value_added","subsidiaries_affiliates",
  "domestic_subsidiaries","overseas_subsidiaries","year")
# tmp <- data.frame(indus=master$industry%>% unique())
# write_excel_csv(tmp,"bsbindus2.csv")
namemaster <- read_csv("bsbindus2.csv")
# namas <- read_csv("bsbindus2.csv")
# namas <- left_join(namas,namemaster)
# write_excel_csv(namas,"bsbindus2.csv")
bsb <- left_join(namemaster,master) %>% mutate(industry=aks) %>% select(-aks)
bsb <- bsb %>% group_by(year,industry) %>% 
  mutate(across(.cols = everything(),.fns = ~sum(.,na.rm = T))) %>% distinct_all()
#write_excel_csv(bsb,"aksbsb.csv")

jra <- read_csv("JRAdata78.csv")
namemaster <- read_csv("IND.csv")
appmas <- read_csv("APP.csv")
jra <- jra %>% filter(industry%in%namemaster$ind&purpose%in%appmas$app)
for (i in 1:nrow(namemaster)) {
  jra$industry[jra$industry==namemaster$ind[i]] <- namemaster$aks[i]
}
for (i in 1:nrow(namemaster)) {
  jra$purpose[jra$purpose==appmas$app[i]] <- appmas$purpose[i]
}

jra <- jra %>% group_by(year,industry,purpose) %>% 
  mutate(across(.cols = everything(),.fns = ~sum(.,na.rm = T))) %>% distinct_all()

dat <- left_join(jra,bsb,by=c("year","industry")) %>% mutate(price=investment/quantity)

write_excel_csv(dat,"maindata2.csv")
