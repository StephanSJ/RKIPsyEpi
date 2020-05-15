# Prevelance compare with up to two stratifications
library(dplyr)
library(rlang)
library(survey)
library(srvyr)
library(kableExtra)
library(xlsx)


##Prevalances are compared an tested. in the formula the variable is provided which diefenes two groups tested on indepndence. The stratfication variable define subgroups in which the two groups are tested again.
## e.g. first only men an women are tested, Then specific age groups of men are compared with the same age groups of women. Then in the age_groups, migration backgrouns are differentied 
## in this age specific migration backgorunds men and women are compared again an the difference is tested

prev_compare <- function(design, ..., formula, prev, keys=c("SF", "Varianz", "KI 2,5%", "KI 97,5%"), comp, statistic="F", round=3,
                         html=TRUE, excel=FALSE, file , row.names = FALSE, sheet = "sheet1", append = FALSE) {
  grouping <- enquos(...)
  
  grouping_text <- as.character(grouping)
  grouping_text <- sub('.','',grouping_text)
  
  form_elements <- quo_name(formula[[2]])
  form_elements <- strsplit(form_elements, "\\s\\+\\s")[[1]]
  
  comp_text <- form_elements[2]
  comp <- sym(comp_text)
  
  
  if (length(grouping)>=2) {
    p <- c()
    ## for first and second grouping variable
    for(i in 1:length(levels(design$variables[[grouping_text[1]]]))){
      for(h in 1:length(levels(design$variables[[grouping_text[2]]]))){
        design1 <- design %>% filter(!!(grouping[[1]])==levels(design$variables[[grouping_text[1]]])[i],
                                     !!(grouping[[2]])==levels(design$variables[[grouping_text[2]]])[h])
        #filtert nach der ersten und zweiten grouping variable erst erstes und danach folgende level
        
        chisq <-svychisq(formula, design1, statistic=statistic)
        p <- c(p, round(chisq$p.value[[1]], digits=3))
      }
    }
    
    
    prop_table <- nice_svy_table(design, !! comp, !!!grouping, !! sym(form_elements[1]), type="prop", html = FALSE)
    # Tabelle mit Werten und erster grouping Variable
    prop_table1 <- prop_table %>% filter(!! sym(form_elements[1])==prev) %>%
      select(!!! syms(form_elements), !!!grouping, Anteil, !!! syms(keys))
    prop_table1 <- reshape(as.data.frame(prop_table1), timevar = comp_text, direction = "wide", idvar=c(form_elements[1], grouping_text))
    
    prop_table1["p"] <- p
    output_table <- prop_table1
    
  }
  
  if (length(grouping)>=1) {
    p <- c()
    ## for first grouping variable
    for(i in 1:length(levels(design$variables[[grouping_text[1]]]))){
      design1 <- design %>% filter(!!(grouping[[1]])==levels(design$variables[[grouping_text[1]]])[i])
      # filtert nach der ersten grouping variable erst erstes und danach folgende level
      
      chisq <-svychisq(formula, design1, statistic=statistic)
      p <- c(p, round(chisq$p.value[[1]], digits=3))
      # test wird nach Formel durchgef?hrt innerhalb eingeschr?nkter Gruppe
      
    }
    
    prop_table <- nice_svy_table(design, !! comp, !!(grouping[[1]]), !! sym(form_elements[1]), type="prop", html = FALSE)
    # Tabelle mit Werten und erster grouping Variable
    prop_table1 <- prop_table %>% filter(!! sym(form_elements[1])==prev) %>%
      select(!!! syms(form_elements), !!(grouping[[1]]), Anteil, !!! syms(keys))
    prop_table1 <- reshape(as.data.frame(prop_table1), timevar = comp_text, direction = "wide", idvar=c(form_elements[1], grouping_text[1] ))
    
    prop_table1["p"] <- p
    
    if (length(grouping)>1) {
      output_table <- bind_rows(output_table, prop_table1)
    } else {
      output_table <- prop_table1
    }
  }
  
  chisq <-svychisq(formula, design, statistic=statistic)
  p <- round(chisq$p.value[[1]], digits=3)
  
  prop_table <- nice_svy_table(design, !! comp, !! sym(form_elements[1]), type="prop", html = FALSE)
  
  prop_table1 <- prop_table %>% filter(!! sym(form_elements[1])==prev) %>%
    select(!!! syms(form_elements), Anteil, !!! syms(keys))
  prop_table1 <- reshape(as.data.frame(prop_table1), timevar = comp_text, direction = "wide", idvar=c(form_elements[1]))
  prop_table1["p"] <- p
  
  if (length(grouping)>0) {
    output_table <- bind_rows(output_table, prop_table1)
  } else {
    output_table <- prop_table1
  }
  
  num_names <- Filter(is.numeric, output_table) %>% colnames()
  
  output_table[, num_names] <- round(output_table[, num_names], round)#
  
  output_table[, c(form_elements[1], grouping_text)] <- sapply(output_table[, c(form_elements[1], grouping_text)], as.character)
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