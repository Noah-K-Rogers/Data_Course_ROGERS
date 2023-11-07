library(tidyverse)
library(ape)
library(janitor)
library(MASS)
library(phangorn)
library(seqinr)
library(Biostrings)
#FASTA####
#list fasta files
list_fasta <- c(list.files("../Fungi_Data/Fasta", pattern = ".fas",full.names = TRUE))
#Test with single file
testing <- readDNAStringSet(list_fasta[1])
df <- data.frame(
  Name = names(testing),
  Sequence = as.character(testing)
)
final <- df
#add the rest of the files
for (i in list_fasta[2:length(list_fasta)]){
  data <- readDNAStringSet(i)
  df <- data.frame(
    Name = names(data),
    Sequence = as.character(data)
  )
  final <- rbind(final,df)
}
#split the name column into 5 columns 
split_data <- strsplit(final$Name, "\\|")
split_df <- as.data.frame(do.call(rbind, split_data))
colnames(split_df) <- c("send_help","id", "species", "marker", "abundances")
final <- cbind(split_df,final$Sequence)
final <- final %>% mutate(id = str_remove(id, "SampleID_") %>% as.numeric()) %>% 
  mutate(marker = str_remove(marker,"marker_")) %>% 
  mutate(species = str_remove(species, "species_") %>% str_replace(pattern = "_", replacement = " "))
# Split the bad abundance column into "abundance" and "total"
df_split <- strsplit(final$abundance, "_")

# mutate the data frame with the split values
final <- final %>% mutate(
  abundances = as.numeric(sapply(df_split, function(x) x[2])),
  total = as.numeric(sapply(df_split, function(x) x[4]))
)


#Metadata####
#list metadata files
list_meta <- c(list.files("../Fungi_Data/Metadata/",full.names = TRUE))
#test single metadata file
final_meta <- read.table(list_meta[1],header = TRUE, sep = "\t")
for (i in list_meta[2:length(list_meta)]){
  df<- read.table(i,header = TRUE, sep = "\t")
  final_meta <- rbind(final_meta,df)
}
#different length?
final[final$id == 430,5]
final_meta[853,]
test <- full_join(final,final_meta)
#check <- final %>% dplyr::select(`Column 2`,`Column 3`)
#check <- distinct(check)
#same length 
#levels(factor(final$`Column 3`))
#levels(factor(final_meta$id))
?full_join
levels(factor(test$plants_dominant))




#SH####
list_sh <- c(list.files("../Fungi_Data/SH/",full.names = TRUE)) 
final_sh <- read.table(list_sh[1],header = TRUE, sep = "\t")  
for (i in list_sh[2:length(list_sh)]){
  df<- read.table(i,header = TRUE, sep = "\t")
  final_sh <- rbind(final_sh,df)
}
final_sh <- final_sh %>% clean_names()
  
test2 <- full_join(final, final_sh)

test3 <- full_join(test,test2)
