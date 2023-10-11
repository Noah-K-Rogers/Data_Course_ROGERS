library(tidyverse)

pivot_longer_multiple <- function(df,patterns,exclude = NA, fix_names = FALSE){
  if(is.na(exclude)==FALSE){
    for (i in 1:length(exclude)) {
    df <- df %>% rename_with(~paste0("xzy",i), exclude[i])
    
  }
  }
  
  if(fix_names == TRUE){
    for(name in patterns){
      tempvec <- subset(names(df), grepl(name,names(df)))
      for(i in 1:length(tempvec)){
        df <- df %>% rename_with(~paste0(name,i), tempvec[i])
      }
    }
  }
  
  
  
  
  
  
  tempdf <- list()
  for(i in 1:length(patterns)){
    tempdf[[i]] <- df %>% 
      select(-contains(patterns[patterns != patterns[i]])) %>% 
      pivot_longer(contains(patterns[i]),values_to = patterns[i],names_to = "names") %>%
      mutate(names = str_remove(names,patterns[i]))
    
  }
  df <- Reduce(full_join, tempdf)
  if(is.na(exclude)==FALSE){
    for (i in 1:length(exclude)) {
      df <- df %>% rename_with(~exclude[i], paste0("xzy",i))
      
    }
  
    
  } 
  return(df)
}
