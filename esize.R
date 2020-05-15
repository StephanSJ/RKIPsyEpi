# Survey effect size (Cohens D)
library(dplyr)
library(survey)
library(srvyr)
library(kableExtra)
library(xlsx)

## Calculates effect size. So far only Cohens D for data with and without survey design. By ref "rolling" a rolling reference can be chosen to which the
## mean difference is compared and with ref=numeric() an number of subgroups can be chosen to which all the other values are compared to.
## This function needs better documentation, the approach to choens d for survey data needs to be confirmed and other efeect size parameters need to be added

esize <- function(data, ... , mean, ref='rolling', html=TRUE, excel=FALSE, row.names=FALSE, file, sheet="sheet1", append=FALSE){
  mean <- enquo(mean)
  mean1 <- quo_text(mean)
  mean1 <- sym(mean1)
  mean1 <- formula(expr(~ !!mean1))
  
  grouping <- enquos(...)
  groups<-map(grouping, c(quo_text, sym))
  groups<-reduce(groups, function(x, y) expr(!! x + !! y))
  groups<- formula(expr(~ !!groups))
  
  
  
  if(class(data)[1]=="data.frame"){  
    tab <- data %>%
      group_by(!!!grouping) %>%
      summarize(mean = mean(!!mean, na.rm=TRUE), sd=sqrt(var(!!mean, na.rm=TRUE)), total=n())
    tab_selection <- tab %>% select(!!!grouping) %>% sapply(FUN=as.factor)
    tab[c(colnames(tab_selection))] <- tab_selection
    
    move<- as.data.frame(tab) %>% distinct(!!!grouping[-1]) %>% nrow() %>% as.numeric()
    
    end <- nrow(tab) %>% as.numeric()
    
    
    
    if(ref=='rolling'){
      tab2 <- tab[move+1:nrow(tab),]
      print(tab2)
      
      s <- sqrt(((tab['total']-1)*tab['sd']^2+(tab2['total']-1)*tab2['sd']^2)/(tab['total']+tab2['total']-2))
      D <- (tab2['mean']-tab['mean'])/s
      DiffMean <- tab2['mean']-tab['mean']
      tab['DiffMean']<-NA
      tab['CohensD']<-NA
      tab[(move+1):(nrow(tab)), 'DiffMean']<-DiffMean[1:as.numeric(nrow(D)-move),]
      tab[(move+1):(nrow(tab)), 'CohensD']<-D[1:as.numeric(nrow(D)-move),]
      tab <- tab %>% arrange(!!!rev(grouping))
      tab <-cbind(as.data.frame(tab[,length(grouping):1]), as.data.frame(tab[,-1:(-length(grouping))]))
    }
    
    else if(is.numeric(ref)==FALSE & ref!='rolling'){
      error("ref must be numeric or rolling")
    }
    
    else if(is.numeric(ref)==TRUE){
      if (ref>=1 & ref%%1==0){
        
        
        col<- quo_text(grouping[[1]])
        row <- levels(as.factor(tab[[col]]))[ref]
        
        
        tab2 <- tab[tab[col]==row,]
        s <- sqrt(((tab[['total']]-1)*tab[['sd']]^2+(tab2[['total']]-1)*tab2[['sd']]^2)/(tab[['total']]+tab2[['total']]-2))
        
        tab['DiffMean'] <- tab[['mean']]-tab2[['mean']]
        print(tab)
        tab['CohensD'] <- (tab[['mean']]-tab2[['mean']])/s
        tab[tab[col]==row, c('DiffMean', 'CohensD')] <- NA
        
        tab <- tab %>% arrange(!!!rev(grouping))
        
        tab <- cbind(as.data.frame(tab[,length(grouping):1]), as.data.frame(tab[,-1:(-length(grouping))]))
        
      }
      else stop("ref must be an integer and >=1")
      
    }
  }
  
  else if(class(data)[1]=="tbl_svy"){
    a <- data %>%
      group_by(!!!grouping) %>%
      summarize(mean=survey_mean(!!mean, na.rm=TRUE))
    
    
    b <- data %>% 
      filter(!is.na(!!mean)) %>%
      group_by(!!!grouping) %>%
      summarize(total=survey_total(na.rm=TRUE))
    
    move <- a %>% distinct(!!!grouping[-1]) %>% nrow() %>% as.numeric()
    
    
    sd <- svyby(mean1, groups, data, svyvar, na.rm=TRUE, na.rm.all = TRUE)
    sd <- sd %>% rename("sd"=quo_text(mean))
    sd["sd"]<-sqrt(sd["sd"])
    sd <- sd %>% arrange(!!!grouping) ##hier nachschauen
    sd <- sd["sd"]
    
    tab <- cbind(a[,1:grep("^mean$", colnames(a))],c(sd,b[,'total']))
    tab_selection <- tab %>% select(!!!grouping) %>% sapply(FUN=as.factor)
    tab[c(colnames(tab_selection))]<-tab_selection
    
    
    if(ref=='rolling'){
      tab2 <- tab[move+1:nrow(tab),]
      
      s <- sqrt(((tab['total']-1)*tab['sd']^2+(tab2['total']-1)*tab2['sd']^2)/(tab['total']+tab2['total']-2))
      D <- (tab2['mean']-tab['mean'])/s
      DiffMean <- tab2['mean']-tab['mean']
      tab['DiffMean']<-NA
      tab['CohensD']<-NA
      tab[(move+1):(nrow(tab)), 'DiffMean']<-DiffMean[1:as.numeric(nrow(D)-move),]
      tab[(move+1):(nrow(tab)), 'CohensD']<-D[1:as.numeric(nrow(D)-move),]
      tab <- tab %>% arrange(!!!rev(grouping))
      tab <-cbind(tab[,length(grouping):1], tab[,-1:(-length(grouping))])
    }
    
    else if(is.numeric(ref)==FALSE & ref!='rolling'){
      error("ref must be numeric or rolling")
    }
    
    else if(is.numeric(ref)==TRUE){
      if (ref>=1 & ref%%1==0){
        col<- quo_text(grouping[[1]])
        row <- levels(as.factor(tab[[col]]))[ref]
        
        
        tab2 <- tab[tab[col]==row,]
        s <- sqrt(((tab[['total']]-1)*tab[['sd']]^2+(tab2[['total']]-1)*tab2[['sd']]^2)/(tab[['total']]+tab2[['total']]-2))
        
        tab['DiffMean'] <- tab[['mean']]-tab2[['mean']]
        print(tab)
        tab['CohensD'] <- (tab[['mean']]-tab2[['mean']])/s
        tab[tab[col]==row, c('DiffMean', 'CohensD')] <- NA
        
        tab <- tab %>% arrange(!!!rev(grouping))
        
        tab <- cbind(as.data.frame(tab[,length(grouping):1]), as.data.frame(tab[,-1:(-length(grouping))]))}
      
      else stop("ref must be an integer and >=1")
      
    }
  }
  
  else stop('class of data must be data.frame or tbl_svy')
  
  tab_html<-tab %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>% 
    collapse_rows(columns = 1:(length(grouping)), valign = "top")
  
  
  
  if(excel==TRUE){
    write.xlsx(as.data.frame(tab), file, row.names = row.names, sheet=sheet, append = append)
  }
  
  if(html==FALSE){
    return(tab)
  }
  if(html==TRUE){
    return(tab_html)
  }
}