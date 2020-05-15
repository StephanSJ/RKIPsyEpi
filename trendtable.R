library(dplyr)
library(survey)
library(srvyr)
library(kableExtra)
library(xlsx)

## survey trendtable: gives table of prevelances of trend.var for the time points specified by time.var in different subgroups specified by ... without test of difference
## without survey design it gives the absolute number, this needs to change
trendtable <- function(data, trend.var, ..., time.var, excel=FALSE, row.names=FALSE, file, sheet="sheet1", append=FALSE){
  
  trend.var <- enquo(trend.var)
  time.var <- enquo(time.var)
  grouping <- enquos(...)
  
  if(class(data)[1]=="data.frame"){  
    a <- data %>% group_by(!!time.var) %>%
      group_by(!!!grouping, add=TRUE) %>%
      group_by(!!trend.var, add=TRUE) %>%
      summarize(total = n())
  }
  else if(class(data)[1]=="tbl_svy"){
    a <- data %>% group_by(!!time.var) %>%
      group_by(!!!grouping, add=TRUE) %>%
      group_by(!!trend.var, add=TRUE) %>%
      summarize(proportion=survey_mean(na.rm=TRUE, vartype=c("se", "var", "ci")))
    
  }
  else stop('class of data must be data.frame or tbl_svy')
  
  b <- a %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>% 
    collapse_rows(columns = 1:(2+length(grouping)), valign = "top")
  
  if(excel==TRUE){
    write.xlsx(as.data.frame(a), file, row.names = row.names, sheet=sheet, append = append)
  }
  return(b)
}