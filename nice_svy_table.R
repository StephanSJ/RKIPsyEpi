library(survey)
library(srvyr)
library(kableExtra)
library(dplyr)
library(xlsx)


# Standard function for frequency tables with survey Design
nice_svy_table <- function(data, ... , type="total", name=FALSE, test="Rao", excel=FALSE, row.names=FALSE, file, sheet="sheet1", append=FALSE, html=TRUE) {
  variable <- enquos(...)
  varnames <- as.character(variable)
  varnames <-  sub('.','',variable)
  
  if(type=="total") {
    tab <- data %>% group_by(!!! variable) %>% summarize(total = survey_total(na.rm = TRUE, vartype=c("se", "var", "ci")))
    if(any(name==FALSE)) {
      names <- c(varnames, "Bevölkerung", "SF", "Varianz", "KI 2,5%", "KI 97,5%")
    } else {
      names <- c(name, "Bevölkerung", "SF", "Varianz", "KI 2,5%", "KI 97,5%")
    }
    colnames(tab)<-names
  }
  
  else if(type=="prop") {
    tab <- data %>% group_by(!!! variable) %>% summarize(proportion = survey_mean(na.rm = TRUE, vartype=c("se", "var", "ci")))
    
    if(any(name==FALSE)) {
      names <- c(varnames, "Anteil", "SF", "Varianz", "KI 2,5%", "KI 97,5%")
    } else {
      names <- c(name, "Anteil", "SF", "Varianz", "KI 2,5%", "KI 97,5%")
    }
    colnames(tab)<-names
    
  }
  else stop('type must be total or prop (=proportion)')
  
  if(length(varnames)==1) {
    tab_html <- tab %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover"))
    
  } else if(length(varnames)==2 & test=="Rao") {
    test <- svychisq(reformulate(varnames), data, na.rm=TRUE)
    test_text<-paste0("Rao-Scott Chi-Square Test: p = ", round(test$p.value[[1]], digits=3),
                      ", F = ", round(test$statistic[[1]], digits=3), 
                      ", ndf = ", round(test$parameter[[1]], digits=0),
                      ", ddf = ", round(test$parameter[[2]], digits=0)
    ) 
    
    tab_html <- tab %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>% 
      collapse_rows(columns = 1:length(varnames), valign = "top") %>%
      footnote(general = test_text, general_title = " ") 
    
  } else {
    tab_html <- tab %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>%
      collapse_rows(columns = 1:length(varnames), valign = "top")
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