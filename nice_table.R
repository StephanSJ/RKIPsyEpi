library(kableExtra)
library(summarytools)
library(dplyr)

# Standard function for frequency tables without survey Design
nice_table <- function(data, variable, name=FALSE) {
  x <- data[variable]
  a <- freq(x)
  a <- cbind(data.frame(row.names(a)),a)
  if(name==FALSE) {
    names <- c(variable, "Anzahl", "% o. NA", "Kum. o. NA", "% m. NA", "Kum. m. NA")
  } else {
    names <- c(name, "Anzahl", "% o. NA", "Kum. o. NA", "% m. NA", "Kum. m. NA")
  }
  colnames(a)<-names
  a %>%  kable(row.names = FALSE) %>% kable_styling(bootstrap_options = c("striped", "hover"))
}