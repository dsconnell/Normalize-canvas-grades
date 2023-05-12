# 1. load libraries
library(tidyverse)

# 2. create two functions (run both unchanged)
normalize <- function(df, column){
    col <- df %>% select(all_of(column)) # select the given column
    points_possible <- col %>% filter(row_number()==1) %>% pull() # extract points possible
    
    new_scores <- col[[1]]/points_possible * 100 # calculate percentage
    new_scores[[1]][1] <- points_possible # put the original points possible back in
    # uploading csv can't change the points possible in canvas :(
    return(new_scores)
}

normalize_many <- function(df, cols){
    new_df <- df
    for(i in 1:length(cols)){ # loop through column indexes passed to function
        new_df[[cols[[i]]]] <- normalize(new_df, cols[[i]])
        #print(tibble(old = df[[cols[[i]]]], new = new_df[[cols[[i]]]]))
    }
    return(new_df)
}

# 3. enter the absolute path to the folder with the gradebook csv (working directory)
setwd("")

# 4. enter filename for the gradebook csv exported from canvas
grades <- read_csv("grades.csv")

# 5. print names of columns in the gradebook
matrix(names(grades), length(names(grades))) 

# 6. enter indexes of columns to normalize
# in a vector e.g. c(6,10,11)
# or a sequence e.g. 6:19
cols <- 6:19

# 7. normalize the selected columns
new_grades <- normalize_many(grades, cols)

# 8. export new csv
write_csv(new_grades, "new_grades.csv")

# 9. import csv into canvas gradebook 
# 10. in canvas gradebook, change all the normalized assignments to be worth 100 points
