ivtest <- function(data=dat2,target=stock7880,iv=pin7880,mysample=F){
  target <- substitute(target)
  iv <- substitute(iv)
  if (mysample==T)  data <- data %>% filter(year>1993)
  j <- 1
  for (i in data$industry %>% unique()) {
    data[paste0("flgi", j)] <- 0
    data[data$industry == i, paste0("flgi", j)] <- 1
    j <- j + 1
  }
  flgi <- paste0("flgi", 1:(j - 1), collapse = "+")
  j <- 1
  for (i in data$year %>% unique()) {
    data[paste0("flgt", j)] <- 0
    data[data$year == i, paste0("flgt", j)] <- 1
    j <- j + 1
  }
  flgt <- paste0("flgt", 1:(j - 1), collapse = "+")
  name <- paste(flgi, flgt, sep = "+")
  res <- eval(parse(text = paste0("AER::ivreg(log(glob+year) ~ log(",target,") + log(glob) + log(ict) +",name,
        "|log(",iv,") + log(glob) + log(ict) +",name,",data = data) %>% summary(.,diagnostics = T)")))
  return(res$diagnostics)
}

