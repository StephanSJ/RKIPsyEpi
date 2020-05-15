library(dplyr)
library(survey)
library(srvyr)

# Prediction for Regressionsmodel with declaration of subpopulations by one categorical variable
subpop_pred <- function(formula, data, subpop, new_data) {
  
  subpop_quo <- enquo(subpop)
  subpop_txt <- data$variables %>% select(!!subpop_quo) %>% colnames
  
  for(i in min(data$variables[subpop_txt]):max(data$variables[subpop_txt])){
    subpop_data <- filter(data, !!subpop_quo == i)
    reg <- svyglm(formula, subpop_data)
    pred <- predict(reg, newdata=new_data)
    
    if(exists("reg_end")==TRUE){
      pred <- cbind(new_data, pred, confint(pred))
      pred[subpop_txt] <- i
      reg_end <- rbind(reg_end, pred)
    } else {
      reg_end <- cbind(new_data, pred, confint(pred))
      reg_end[subpop_txt] <- i
    }
  }
  names(reg_end)[names(reg_end) == "link"] <- stri_sub(quo_name(formula[2]),1,-3)
  row.names(reg_end) <- NULL
  return(reg_end)
}