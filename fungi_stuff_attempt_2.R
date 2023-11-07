library(janitor)
library(Biostrings)
library(seqinr)
library(tidyverse)
list_fasta <- c(list.files("../Fungi_Data/Fasta", pattern = ".fas",full.names = TRUE))
list_meta <- c(list.files("../Fungi_Data/Metadata/",full.names = TRUE))
list_sh <- c(list.files("../Fungi_Data/SH/",full.names = TRUE)) 
#first one for testing and initialization
fasta_clean <- function(path){
 
  testing <- readDNAStringSet(path)
  final <- data.frame(
  Name = names(testing),
  Sequence = as.character(testing)
)
#split the name column into 5 columns 
split_data <- strsplit(final$Name, "\\|")
split_df <- as.data.frame(do.call(rbind, split_data))
colnames(split_df) <- c("hash_value","id", "species", "marker", "abundance")
final <- cbind(split_df,final$Sequence)
final <- final %>% mutate(sequence = `final$Sequence`) %>% dplyr::select(-`final$Sequence`)
final <- final %>% mutate(id = str_remove(id, "SampleID_") %>% as.numeric()) %>% 
  mutate(marker = str_remove(marker,"marker_")) %>% 
  mutate(species = str_remove(species, "species_") %>% str_replace(pattern = "_", replacement = " "))
# Split the bad abundance column into "abundance" and "total"
df_split <- strsplit(final$abundance, "_")

# mutate the data frame with the split values
final <- final %>% mutate(
  abundance = as.numeric(sapply(df_split, function(x) x[2])),
  abun_total = as.numeric(sapply(df_split, function(x) x[4]))
)
return(final)

}
path <- list_sh[3]
clean_sh <- function(path){
  df <- read.table(path,header = TRUE, sep = "\t")%>% clean_names() 
      result <- df %>% 
        group_by(marker, species,genus,family,order,class,phylum,kingdom) %>% 
        summarise(accession = paste(accession, collapse = ","))
      return(result)
}






full <- left_join(fasta_clean(list_fasta[1]),
                  read.table(list_meta[1],header = TRUE, sep = "\t") %>% 
                    mutate(marker = target_gene) %>% select(-target_gene),by = join_by(id == id, marker==marker)) %>% 
  left_join(clean_sh(list_sh[1]))
  
  
for (i in 2:length(list_fasta)){
  df <- left_join(fasta_clean(list_fasta[i]),
                  read.table(list_meta[i],header = TRUE, sep = "\t") %>% 
                    mutate(marker = target_gene) %>% select(-target_gene),by = join_by(id == id, marker==marker)) %>% 
    left_join(clean_sh(list_sh[i]))
  full <- rbind(full,df)
}  
warnings()

saveRDS(full,file = "Fungi_df")
