#setwd("/Users/cyrusseyrafi/srs-assignment/assignment_2")
setwd("C:/Users/BCapo/Desktop/University of Edinburgh Masters/Sem 2/srs-assignment/assignment_2")

# NOTES #
# Q1 is the 20% of areas with lowest participation in higher education
# Need to make all columns but INSITUTION_NAME numeric.
# Canterbury Christ Church Uni seems to have a too high total???

# READ DATA #

data = read.csv("data.csv")
head(data)

# Remove 'X.1' column and 'X' cols, R provides indices for us.
data = data[,-c(1,2,3)]
head(data)
str(data)

# FIX CONTINUATION BY LOOKING AT IT:
data$continuation

# APPEARS TO BE "n/a" converting it to string column
data$continuation <- gsub("n/a", NA, data$continuation)
data$continuation <- as.numeric(data$continuation)

# Get column indices and names by using a named list/vector as a 'dictionary'.
cols = as.list(1:ncol(data))
names(cols) = names(data)

# Remove SIMD data as useless.
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]

str(data)

# Reset the cols data. 
cols = as.list(1:ncol(data))
names(cols) = names(data)

# NEED TO GET RID OF NA VALUES FIRST.
data <- na.omit(data)
cor(data[,-1])
#plot(data)

# EDA #

pairs(data[,-1])
baseline_model <- lm(satisfied_feedback ~ ., data = data[,-1])
summary(baseline_model)
plot(baseline_model)
#plot(data)

# DATA CLEANING/FEATURE ENGINEERING? #

# MODELS #
