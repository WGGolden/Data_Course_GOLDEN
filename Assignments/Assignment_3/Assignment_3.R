# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

data("iris")

eveniris <- iris[seq(2,150,2),]

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

iris_chr <- lapply(iris,as.character)

class(iris_chr$Sepal.Length) #this confirms the object is a "list" that has the same 5 columns, all character class


# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

Sepal.Area <- iris$Sepal.Length*iris$Sepal.Width

# 4.  Add Sepal.Area to the iris data frame as a new column

iris$Sepal.Area <- Sepal.Area

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
      # (name it big_area_iris)

big_area_iris <- subset(x=iris,subset = iris$Sepal.Area>20) #this subset function selects every row that has sepal.area greater than 20 and creates a new dataset with these values

# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated

