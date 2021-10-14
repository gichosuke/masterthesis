
# read the data set ---------------------------------------------------------
library(openxlsx)
library(tidyverse)
library(plm)
library(prediction)
st12 <- read_csv("avstock.csv")


# regress stock num with price ---------------------------------------------

pst <- pdata.frame(st12,index = c("year","industry"))

SP <- plm(log(qstock)~log(pin)+log(sales)+log(import),data = pst,model = "within",effect = "twoways")
# Sig but low R^2...0.16
RSP <- plm(log(rqstock)~log(pin)+log(sales)+log(import),data = pst,model = "within",effect = "twoways")
# Sig but low R^2...0.31

# take three year mean
st3 <- st12 %>% arrange(industry,year)
for (i in seq(2001,2018,3)) {
  st3[st3$year==i,-c(1,23)] <- (st3[st3$year==i,-c(1,23)]+st3[st3$year==i+1,-c(1,23)]+st3[st3$year==i+2,-c(1,23)])/3
}
st3 <- filter(st3,year%in%seq(2001,2018,3))
pst3 <- pdata.frame(st3,index = c("year","industry"))
SP3 <- plm(log(qstock)~log(pin)+log(sales)+log(import),data = pst3,model = "within",effect = "twoways") #0.26
RSP3 <- plm(log(rqstock)~log(pin)+log(sales)+log(import),data = pst3,model = "within",effect = "twoways") 
# Coefficients:
#             Estimate Std. Error t-value  Pr(>|t|)    
# log(pin)     0.85478    0.23452  3.6448 0.0006673 ***
# log(sales)  -1.13956    0.64235 -1.7741 0.0825310 .  
# log(import) -0.75039    0.29715 -2.5253 0.0149934 *  

# Sig and R^2=0.47 use this so far

# regress firm number with hat(log(rqstock)) 3year -----------------------------------

hlrs3 <- data.frame(hatrq=predict(RSP3))
rst3 <- bind_cols(st3,hlrs3)
prst3 <- pdata.frame(rst3,index = c("year","industry"))
FHR3 <- plm(log(firm)~hatrq+log(sales)+log(import),data = prst3,model = "within",effect = "twoways")

# Coefficients:
#              Estimate Std. Error t-value Pr(>|t|)  
# hatrq        0.060419   0.037327  1.6187  0.11221  
# log(sales)  -0.299737   0.133746 -2.2411  0.02978 *
# log(import)  0.060808   0.063056  0.9643  0.33981  

# regress firm number with hat(log(qstock)) 3year -----------------------------------

hls3 <- data.frame(hatq=predict(SP3))
st3 <- bind_cols(st3,hls3)
pst3 <- pdata.frame(st,index = c("year","industry"))
FH3 <- plm(log(firm)~hatq+log(sales)+log(import),data = pst3,model = "within",effect = "twoways")
# Coefficients:
#              Estimate Std. Error t-value Pr(>|t|)  
# hatq         0.118422   0.066211  1.7886  0.08013 .
# log(sales)  -0.320481   0.132989 -2.4098  0.01993 *
# log(import)  0.056084   0.062925  0.8913  0.37732  


# result so far -----------------------------------------------------------

# automation increases number of firms ! however, data says price fall decreases the amount of robots...why?