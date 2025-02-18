setwd("C:/Users/BCapo/Desktop/University of Edinburgh Masters/Sem 2/srs-assignment")

data = read.csv("data.csv")
head(data)

# Remove 'X.1' column and 'X' cols, R provides indices for us.
data = data[,-c(1,2)]
head(data)

# Get column indices and names by using a named list/vector as a 'dictionary'.
cols = as.list(1:ncol(data))
names(cols) = names(data)

# Remove SIMD data as useless.
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]

# Reset the cols data. 
cols = as.list(1:ncol(data))
names(cols) = names(data)

# EDA

# DATA CLEANING/FEATURE ENGINEERING?

# MODELS