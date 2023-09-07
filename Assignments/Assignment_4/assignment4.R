#read in data starting from assignment 4 directory
df <- read.csv("../../Data/ITS_mapping.csv", sep ="\t",header = TRUE)

#summarize data
summary(df)
#how the crap do box plots work
#?boxplot
#given png code
png(filename = "./silly_boxplot.png")
#i found out how box plots work
boxplot(df$Lat~df$Ecosystem,
        xlab = "Ecosystem",
        ylab = "Latitude",
        col = c("lightblue", "blue", "lightgreen","darkgreen","brown"))
title("Silly Boxplot")
#more given code
dev.off()