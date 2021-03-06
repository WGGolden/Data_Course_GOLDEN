#Write a command that lists all of the .csv files found in the Data

csv_files<- list.files(path = "../../Data",
                       pattern = ".csv",
                       full.names = TRUE)

#Find how many files match that description

length(csv_files)

#Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” 

wvm <- list.files (path = "../../Data",
                   pattern = "wingspan_vs_mass.csv",
                   full.names = TRUE)

df <- read.csv(wvm)

# Inspect first five lines of wvm

head(df,5)

#Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)

list.files (path = "../../Data",
            pattern = "^b",
            all.files = TRUE,
            recursive = TRUE,
            full.names = TRUE)

#Write a command that displays the first line of each of those “b” files

bfiles <- list.files (path = "../../Data",
                      pattern = "^b",
                      all.files = TRUE,
                      recursive = TRUE,
                      full.names = TRUE)


for (i in bfiles){
  print(readLines(i)[1])
}

#Do the same thing for all files that end in “.csv”

for (x in csv_files){
  print(readLines(x)[1])
}

