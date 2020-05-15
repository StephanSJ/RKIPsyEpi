### mean compare ###
library(dplyr)
library(survey)
library(srvyr)
library(kableExtra)
library(xlsx)


## I need to check again how this function works. It also cannot handle data without survey design yet

mean_compare <- function(design, ..., formula, test.option="ttest", round=3, excel=FALSE, row.names=FALSE, file, sheet="sheet1", append=FALSE, html=TRUE) {
  
  grouping <- enquos(...)
  
  grouping_text <- as.character(grouping)
  grouping_text <- sub('.','',grouping_text)
  
  
  target_var <- sym(quo_name(formula[[2]]))
  comp_text <- quo_name(formula[[3]]) #Spaltenname
  comp <- sym(comp_text)
  # Dieser Teil der Formel muss für Berechnung group_by als quosure vorliegen
  
  
  
  if (length(grouping)>=2) {
    p <- c()
    sd <- data.frame(sd = numeric(),
                     stringsAsFactors = FALSE)
    ## for first and second grouping variable
    for(i in 1:length(levels(design$variables[[grouping_text[1]]]))){
      for(h in 1:length(levels(design$variables[[grouping_text[2]]]))){
        design1 <- design %>% filter(!!(grouping[[1]])==levels(design$variables[[grouping_text[1]]])[i],
                                     !!(grouping[[2]])==levels(design$variables[[grouping_text[2]]])[h])
        
        #filtert nach der ersten und zweiten grouping variable erst erstes und danach folgende level
        
        sd_append <- design1 %>% group_by(!!comp) %>% summarise(sd=survey_sd(!!target_var, na.rm=TRUE))
        sd <- rbind(sd, sd_append["sd"])
        
        if(test.option=="ttest") {
          test <-svyttest(formula, design1) 
          p <- c(p, round(test$p.value[[1]], digits=3))
          
        } else if(test.option=="wilcox") {
          test <-svyranktest(formula, design1)
          
          p <- c(p, round(test$p.value[[1]], digits=3))
        } else stop('test must be ttest or wilcox')
      }
    }
    
    mean_table <- nice_svy_gmean(design, !!target_var, !!!grouping, !! comp, html = FALSE)
    mean_table <- cbind(mean_table, sd)
    mean_table["varname"] <- quo_name(formula[[2]])
    
    mean_table1 <- mean_table %>%
      filter(!is.na(!!comp)) %>%
      select(varname, !!comp, !!!grouping, sd, Durchschnitt, Durchschnitt_low, Durchschnitt_upp)
    
    mean_table1 <- reshape(as.data.frame(mean_table1), timevar = comp_text, direction = "wide", idvar=c("varname", grouping_text))
    
    mean_table1["p"] <- p
    mean_table1["varname"] <- quo_name(formula[[2]]) # Name der Variable die untersucht wird
    
    output_table <- mean_table1
    
  }
  
  if (length(grouping)>=1) {
    sd <- data.frame(sd = numeric(),
                     stringsAsFactors = FALSE)
    p <- c()
    ## for first grouping variable
    for(i in 1:length(levels(design$variables[[grouping_text[1]]]))){
      design1 <- design %>% filter(!!(grouping[[1]])==levels(design$variables[[grouping_text[1]]])[i])
      # filtert nach der ersten grouping variable erst erstes und danach folgende level
      
      sd_append <- design1 %>% group_by(!!comp) %>% summarise(sd=survey_sd(!!target_var, na.rm=TRUE))
      sd <- rbind(sd, sd_append["sd"])
      
      if(test.option=="ttest") {
        test <-svyttest(formula, design1) 
        p <- c(p, round(test$p.value[[1]], digits=3))
      }
      
      if(test.option=="wilcox") {
        test <-svyranktest(formula, design1) 
        p <- c(p, round(test$p.value[[1]], digits=3))
      }
    }
    print(p)
    
    mean_table <- nice_svy_gmean(design, mean = !!target_var, !!(grouping[[1]]), !! comp, html = FALSE)
    mean_table <- cbind(mean_table, sd)
    mean_table["varname"] <- quo_name(formula[[2]])
    
    mean_table1 <- mean_table %>%
      filter(!is.na(!!comp)) %>%
      select(varname, !!comp, !!(grouping[[1]]), Durchschnitt, sd, Durchschnitt_low, Durchschnitt_upp)
    
    mean_table1 <- reshape(as.data.frame(mean_table1), timevar = comp_text, direction = "wide", idvar=c("varname", grouping_text[1]))
    
    mean_table1["p"] <- p
    mean_table1["varname"] <- quo_name(formula[[2]]) # Name der untersuchten variable
    
    if (length(grouping)>1) {
      output_table <- bind_rows(output_table, mean_table1)
    } else {
      output_table <- mean_table1
    }
  }
  
  # no grouping
  sd <- design %>% group_by(!!comp) %>% summarise(sd=survey_sd(!!target_var, na.rm=TRUE))
  sd <- sd["sd"]
  
  if(test.option=="ttest") {
    test <-svyttest(formula, design) 
    p <- round(test$p.value[[1]], digits=3)
  }
  
  if(test.option=="wilcox") {
    test <-svyranktest(formula, design) 
    p <-  round(test$p.value[[1]], digits=3)
  }
  
  
  mean_table <- nice_svy_gmean(design, mean=!!target_var, !! comp, html = FALSE)
  mean_table <- cbind(mean_table, sd)
  mean_table["varname"] <- quo_name(formula[[2]])
  
  mean_table1 <- mean_table %>%
    filter(!is.na(!!comp)) %>% 
    select(varname, sd, !!comp, Durchschnitt, Durchschnitt_low, Durchschnitt_upp)
  
  mean_table1 <- reshape(as.data.frame(mean_table1), timevar = comp_text, direction = "wide", idvar = "varname" )
  
  mean_table1["p"] <- p
  
  # different handling if there are groups or not
  if (length(grouping)>0) {
    output_table <- bind_rows(output_table, mean_table1)
  } else {
    output_table <- mean_table1
  }
  
  num_names <- Filter(is.numeric, output_table) %>% colnames()
  
  output_table[, num_names] <- round(output_table[, num_names], round)
  
  output_table[, c("varname", grouping_text)] <- sapply(output_table[, c("varname", grouping_text)], as.character)
  output_table[is.na(output_table)] <- ""
  output_table <- arrange(output_table, !!!grouping)
  
  if(excel==TRUE){
    
    write.xlsx(as.data.frame(output_table), file, row.names = row.names, sheet=sheet, append = append)
  }
  
  
  if(html==TRUE){
    
    output_html <- output_table %>%  kable() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>% 
      collapse_rows(columns = 1:length(grouping_text), valign = "top") 
    
    return(output_html)
    
  } else {
    return(output_table)
  }
}
