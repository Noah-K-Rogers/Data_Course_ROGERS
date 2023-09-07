#1.  Get a subset of the "iris" data frame where it's just even-numbered rows
data("iris")
dat <- iris
#seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150

even_iris <- dat[seq(2,150,2),]

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

iris_chr<- data.frame(lapply(dat, as.character))
# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width
dat$Sepal.Area <- dat$Sepal.Length*dat$Sepal.Width


# 4.  Add Sepal.Area to the iris data frame as a new column
#done above :)

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)
big_area_iris <- dat[dat$Sepal.Area > 20,]


