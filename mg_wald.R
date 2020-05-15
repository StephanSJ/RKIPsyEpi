library(dplyr)
library(lavaan)

# multiple group Wald-Test of Coefficiants
## Function to test equality of parameters step by step and putting all the results in one table. Has no excel export option yet.
## Used after fitting a multiple groupe model in lavaan

mg_wald <- function(fit_model, const, par_name, html=TRUE) {
  model.partest <- data.frame(paramter=character(0), Chi=integer(0) , df=numeric(0), pValue=integer(0), SE=character(0))
  
  for (par in 1:length(const)) {
    a<-lavTestWald(fit_model, constraints=const[par])
    model.partest[,1] <- as.character(c(model.partest[,1]))
    model.partest[,5] <- as.character(c(model.partest[,5]))
    model.partest[nrow(model.partest) + 1,]  <- list(par_name[par], a$stat[[1]], a$df[[1]], a$p.value[[1]], a$se[[1]])
  }
  
  if(html==TRUE) {
    model.partes.html <- model.partest %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover"))
    return(model.partes.html)
  } else {
    return(model.partest)
  }
}