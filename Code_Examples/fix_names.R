library(tidyverse)
fix_names <- function(df,pattern,exclude = NA){
  if(is.na(exclude)==FALSE){
    for (i in 1:length(exclude)) {
      df <- df %>% rename_with(~paste0("xzy",i), exclude[i])
      
    }
  }
  for(name in pattern){
    tempvec <- subset(names(df), grepl(name,names(df)))
    for(i in 1:length(tempvec)){
      df <- df %>% rename_with(~paste0(name,i), tempvec[i])
    }
  }
  
  
  if(is.na(exclude)==FALSE){
    for (i in 1:length(exclude)) {
      df <- df %>% rename_with(~exclude[i], paste0("xzy",i))
      
    }
  }
  return(df)
}
