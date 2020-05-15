####
library(dplyr)
library(survey)
library(srvyr)
library(xlsx)

### pub_describe

##Nochmal Varianz/Standarddeviation pruefen

#helpfunction
perc_function <- function(data=data_use, var_string, type) {
  if (type=="prop") {
    props  <- data %>% group_by(!!sym(var_string)) %>% summarize(props_weighted=survey_mean(na.rm=TRUE)) %>% na.omit()
    names(props)[1] <- "category"
    return(props)
  }
  if (type=="mean"){
    mean  <- data %>% summarize(mean_weighted=survey_mean(!!sym(var_string), na.rm=TRUE, vartype=c("var")))
    return(mean)
  }
}



pub_describe <- function(data, ..., round = 3, pub = TRUE, html = TRUE, excel=FALSE) {
  # keys = c("N", "missing", "percent (unweighted)", "percent (weighted)", "mean (unweighted)", "SD (unweighted)", "mean (weighted)", "SD (weighted)"
  # 1. Schritt: erkennen ob survey design oder nicht
  # 2. Schritt: Frequencies von allen und missings von allen
  # 3. Schritt: Numeric or factor
  # 4. mean oder frequencies berechnen (weighted und unweighted)
  # 5. in data frame einfÃ¼gen
  
  vars_quo <- enquos(...)
  if(length(vars_quo)==0){
    data_use <- data
  } else {
    data_use <- data %>% select(!!!vars_quo)
    
  }
  
  if(class(data)[1]=="data.frame"){
    
    num <- Filter(is.numeric, data_use) %>% colnames()
    cat <-Filter(is.factor, data_use) %>% colnames()
    obs <- nrow(data_use)
    missing <- colSums(is.na(data_use))
    
    N = obs - missing
    
    description_table <- data.frame(varnames=colnames(data_use), N=N, missing=missing)
    
    if (length(cat)>0) {
      if (length(cat)==1) {
        list_cat_freq <- list(table(data_use[, cat]))
        names(list_cat_freq) <- cat
      } else {
        list_cat_freq <- lapply(data_use[, cat], table)
      }
      
      
      list_cat_perc <- lapply(list_cat_freq, prop.table)
      list_cat_freq <- lapply(list_cat_freq, data.frame)
      list_cat_perc <- lapply(list_cat_perc, data.frame)
      
      
      cat_table <- bind_rows(list_cat_freq, .id="varnames")
      perc_table <- bind_rows(list_cat_perc)
      cat_table <- cbind(cat_table, perc_table["Freq"])
      colnames(cat_table) <- c("varnames", "category", "N", "percent (unweighted)")
      cat_table["percent (unweighted)"] <- cat_table["percent (unweighted)"]*100
      
      description_table <- bind_rows(description_table, cat_table)
      
    }
    
    if (length(num)>0) {
      if (length(num)==1) {
        description_table[description_table$varnames==num , "mean (unweighted)"] <- mean(as.matrix(data_use[, num]), na.rm=TRUE)
        description_table[description_table$varnames==num , "SD (unweighted)"] <- sd(as.matrix(data_use[, num]), na.rm=TRUE)
      } else {
        description_table[description_table$varnames %in% num , "mean (unweighted)"] <- sapply(data_use[, num], function(x) mean(x, na.rm=TRUE))
        description_table[description_table$varnames %in% num , "SD (unweighted)"] <- sapply(data_use[, num], function(x) sd(x, na.rm=TRUE))
      }
    }
    
    # Order table conditionally on the kind of variables choosen
    if(length(cat)>0 & length(num)==0){
      description_table <- description_table[, c("varnames", "category", "N", "missing", "percent (unweighted)")]
    } else if(length(num)>0 & length(cat)==0) {
      description_table <- description_table[, c("varnames", "N", "missing", "mean (unweighted)", "SD (unweighted)")]
    } else {
      description_table <- description_table[, c("varnames", "category", "N", "missing", "percent (unweighted)", "mean (unweighted)", "SD (unweighted)")]
    }
    
  } else if(class(data)[1]=="tbl_svy"){
    
    num <- Filter(is.numeric, data_use$variables) %>% colnames()
    cat <-Filter(is.factor, data_use$variables) %>% colnames()
    obs <- nrow(data_use$variables)
    missing <- colSums(is.na(data_use$variables))
    
    N = obs - missing
    
    description_table <- data.frame(varnames=colnames(data_use$variables), N=N, missing=missing)
    
    ## ungewichtet ##
    if (length(cat)>0) {
      
      if (length(cat)==1) {
        list_cat_freq <- list(table(data_use$variables[, cat]))
        names(list_cat_freq) <- cat
      } else {
        list_cat_freq <- lapply(data_use$variables[, cat], table)
      }
      
      list_cat_perc <- lapply(list_cat_freq, prop.table)
      list_cat_freq <- lapply(list_cat_freq, data.frame)
      list_cat_perc <- lapply(list_cat_perc, data.frame)
      
      cat_table <- bind_rows(list_cat_freq, .id="varnames")
      perc_table <- bind_rows(list_cat_perc)
      cat_table <- cbind(cat_table, perc_table["Freq"])
      
      colnames(cat_table) <- c("varnames", "category", "N", "percent (unweighted)")
      
    }
    if (length(num)>0) {
      if (length(num)==1) {
        
        description_table[description_table$varnames==num , "mean (unweighted)"] <- mean(as.matrix(data_use$variables[, num]), na.rm=TRUE)
        description_table[description_table$varnames==num , "SD (unweighted)"] <- sd(as.matrix(data_use$variables[, num]), na.rm=TRUE)
        
      } else {
        description_table[description_table$varnames %in% num , "mean (unweighted)"] <- sapply(data_use$variables[, num], function(x) mean(x, na.rm=TRUE))
        description_table[description_table$varnames %in% num , "SD (unweighted)"] <- sapply(data_use$variables[, num], function(x) sd(x, na.rm=TRUE))
      }
    }
    
    ## gewichtet ##
    
    
    if (length(cat)>0) {
      
      if (length(cat)==1) {
        cat_table[cat_table$varnames==cat, "percent (weighted)"] <- perc_function(data=data_use, cat, type="prop")["props_weighted"]
      } else {
        prop_weighted <- lapply(cat, function(x) perc_function(data=data_use, x, type="prop"))
        names(prop_weighted) <- cat
        prop_weighted <- bind_rows(prop_weighted, .id="varnames")
        prop_weighted <- as.data.frame(prop_weighted)
        cat_table[cat_table$varnames %in% cat, "percent (weighted)"] <- prop_weighted$props_weighted
      }
      
      percent_cols <- c("percent (unweighted)", "percent (weighted)")
      cat_table[percent_cols] <- sapply(cat_table[percent_cols], function(x) x*100)
      
      description_table <- bind_rows(description_table, cat_table)
    }
    
    if (length(num)>0) {
      if (length(num)==1) {
        description_table[description_table$varnames==num , c("mean (weighted)", "SD (weighted)")] <- perc_function(data=data_use, num, type="mean")
      } else {
        description_table[description_table$varnames %in% num, c("mean (weighted)", "SD (weighted)")] <- sapply(num, function(x) perc_function(data=data_use, x, type="mean")) %>% 
          t() %>%
          as.data.frame() %>%
          sapply(as.numeric)
      }
      description_table[description_table$varnames %in% num, "SD (weighted)"] <- sqrt(as.numeric(description_table[description_table$varnames %in% num, "SD (weighted)"]))
    }
    
    # Order table conditionally on the kind of variables choosen
    if(length(cat)>0 & length(num)==0){
      description_table <- description_table[, c("varnames", "category", "N", "missing", "percent (unweighted)",  "percent (weighted)")]
    } else if(length(num)>0 & length(cat)==0) {
      description_table <- description_table[, c("varnames", "N", "missing", "mean (unweighted)", "SD (unweighted)", "mean (weighted)", "SD (weighted)")]
    } else {
      description_table <- description_table[, c("varnames", "category", "N", "missing", "percent (unweighted)",  "percent (weighted)", "mean (unweighted)", "SD (unweighted)", "mean (weighted)", "SD (weighted)")]
    }
  }
  
  else stop('class of data must be data.frame or tbl_svy')
  
  num_cols <- Filter(is.numeric, description_table) %>% colnames()
  description_table <- description_table %>% arrange(!is.na(varnames), varnames)
  description_table[, num_cols] <- round(description_table[, num_cols], round)
  
  if(pub==TRUE) {
    description_table_nareplace <- description_table
    if(length(cat)>0){
      description_table_nareplace$category <- as.character(description_table_nareplace$category)
    }
    description_table_nareplace[is.na(description_table_nareplace)] <- ""
    description_table_nareplace[description_table_nareplace=="NULL"] <- ""
    return(description_table_nareplace)
    
  } else {
    return(description_table)
  }
}