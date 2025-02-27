setwd("/Users/cyrusseyrafi/srs-assignment/assignment_2")

# NOTES #
# Q1 is the 20% of areas with lowest participation in higher education

# READ DATA #

data = read.csv("data.csv")
head(data)

# Remove 'X.1' column and 'X' cols, R provides indices for us.
data = data[,-c(1,2,3)]
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

# Initialize correlations.csv.
correlations <- cor(data, use = "complete.obs")

# Convert correlations to a data frame.
cor_df <- as.data.frame(correlations)
cor_df <- cbind(Variable = rownames(cor_df), cor_df)
rownames(cor_df) <- NULL

cor_df <- data.frame(cor_df)
# Save correlations to a CSV file.
write.csv(cor_df, "correlations.csv", row.names = FALSE)

# Print results
print(cor_df)

# EDA #

plot(data)

# DATA CLEANING/FEATURE ENGINEERING? #


# MODELS #
