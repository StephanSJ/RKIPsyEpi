library(dplyr)
library(survey)
library(srvyr)
library(kableExtra)
library(xlsx)

## grouped means
nice_svy_gmean <- function(data, mean, ..., excel=FALSE, row.names=FALSE, file, sheet="sheet1", append=FALSE, html=TRUE){
  mean <- enquo(mean)
  group <- enquos(...)
  groups <- as.character(group) 
  groups <- sub('.','',groups)
  
  tab <- data %>% group_by(!!! group) %>% summarize(Durchschnitt=survey_mean(as.numeric(!!mean), na.rm = TRUE, vartype=c("se", "var", "ci")))
  
  if(length(groups)==1) {
    tab_html <- tab %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover"))
    
  } else {
    tab_html <- tab %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>%
      collapse_rows(columns = 1:length(groups), valign = "top")
  }
  if(excel==TRUE){
    write.xlsx(as.data.frame(tab), file, row.names = row.names, sheet=sheet, append = append)
  }
  if(html==TRUE){
    return(tab_html)
  } else {
    return(tab)
  }
}
