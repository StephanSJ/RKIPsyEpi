#Makes Regressions in subpops of one category
library(dplyr)
library(survey)
library(srvyr)

##Regression
subpop_reg <- function(formula, data, subpop) {
  
  subpop_quo <- enquo(subpop)
  subpop_txt <- data$variables %>% select(!!subpop_quo) %>% colnames
  
  for(i in min(data$variables[subpop_txt]):max(data$variables[subpop_txt])) {
    subpop_data <- filter(data, !!subpop_quo == i)
    reg <- svyglm(formula, subpop_data)
    
    if(exists("reg_out")==TRUE) {
      reg_out1 <- tidy(reg)
      reg_out1[subpop_txt] <- i
      reg_out <- rbind(reg_out, reg_out1)
      
      
    } else {
      
      reg_out <- tidy(reg)
      reg_out[subpop_txt] <- i
    }
  }
  return(reg_out)
}