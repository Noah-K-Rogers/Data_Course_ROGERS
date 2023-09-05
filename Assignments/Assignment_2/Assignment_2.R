#read csv files into variable
csv_files <- c(list.files(path = "Data", pattern = ".csv",full.names = TRUE, recursive=TRUE))
#check the length of csv files
length(csv_files)
#save data
df<-read.csv("Data/wingspan_vs_mass.csv")
#check data
head(df,n = 5)
#find (and save) files that start with b
b_list <- list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)
#for loop to write first lines
for (file_path in b_list){
  print(readLines(file_path,n=1))
}
for (file_path in csv_files){
  print(readLines(file_path,n=1))
}
