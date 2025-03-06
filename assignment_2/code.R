#setwd("/Users/cyrusseyrafi/srs-assignment/assignment_2")
setwd("C:/Users/BCapo/Desktop/University of Edinburgh Masters/Sem 2/srs-assignment/assignment_2")

# NOTES #
# Q1 is the 20% of areas with lowest participation in higher education
# Need to make all columns but INSTITUTION_NAME numeric.
# Canterbury Christ Church Uni seems to have a too high total???

######                            IMPORTS                               ######

library(ggplot2)
library(glmnet)

######                       READ DATA + CLEAN                          ######

# Read the data and read the first few rows.
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
cols = c(1:ncol(data))
names(cols) = names(data)

# Remove SIMD data as useless.
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]

str(data)

# Reset the cols data. 
cols = c(1:ncol(data))
names(cols) = names(data)

# NEED TO GET RID OF NA VALUES FIRST.
data <- na.omit(data)
cor(data[,-1])
#plot(data)

# Get the index for the data through the INSTITUTION_NAME
INSTITUTION_NAME <- data$INSTITUTION_NAME
rownames(data) <- INSTITUTION_NAME

######                                EDA                               ######

#pairs(data[,-1])
for(col in names(cols)[-1]){
  print(col)
  plotted <- ggplot(data[,-1], aes(x = .data[[col]], y = satisfied_feedback)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) + 
    theme_minimal()  
  print(plotted)
}
# POLAR4.Q4 bad cor but the rest are good?? Added value and total: bad. Could do
# feature engineering/transformations on ethnic/gender columns to improve but
# overall not great.

######                DATA CLEANING/FEATURE ENGINEERING?                ######


#other_ethnic_outlier <- data[which(data$Other.ethnic.group==max(data$Other.ethnic.group)),]
#other_ethnic_outlier
#apply(data[,cols[c("Men", "Women")]], 1, sum)
#apply(data[,cols[c("White.ethnic.group", "Black.ethnic.group",
#                   "Asian.ethnic.group", "Other.ethnic.group", "Mixed.ethnic.group")]], 1, sum)
#apply(data[,cols[c("POLAR4.Q1", "POLAR4.Q2", "POLAR4.Q4", "POLAR4.Q5", "POLAR4.Q3")]], 1, sum)

model_data <- data[cols[c("satisfied_feedback", "satisfied_teaching", 
                          "students_staff_ratio", "spent_per_student",
                          "avg_entry_tariff", "career_after_15_month",
                          "continuation", "Women")]]
model_data[,-1] <- scale(model_data[,-1])

continuation_sq <- (model_data$continuation)^2
satisfied_teaching_sq <- (model_data$satisfied_teaching)^2

model_data <- cbind(model_data, continuation_sq, satisfied_teaching_sq)

# Get column indices and names by using a named list/vector as a 'dictionary'.
model_cols = c(1:ncol(model_data))
names(model_cols) = names(model_data)

######                                MODELS                            ######

# RESIDUAL PLOTS
residual_plots <- function(model, data, response) {
  
  # Extract predicted values.
  predicted <- predict(model, data, type = "response")
  
  # Compute residuals.
  residuals <- data[[response]] - predicted
  
  # Standardize residuals.
  sigma_hat <- sd(residuals)
  residuals <- residuals / sigma_hat
  
  # Initialize plots.
  par(mfrow = c(1, 3))
  
  ## Residuals vs. Fitted plot.
  plot(predicted, residuals, main = "Residuals vs. Predicted",
       xlab = "Predicted Values", ylab = paste("Residuals"), pch = 19)
  abline(h = 0, col = "red", lty = 2)
  
  ## Histogram of Residuals.
  hist(residuals, breaks = 30, probability = TRUE, 
       main = "Histogram of Residuals", xlab = paste("Residuals"))
  curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), add = TRUE, col = "blue")
  
  ## Q-Q Plot.
  qqnorm(residuals, main = "Q-Q Plot of Residuals")
  qqline(residuals, col = "red", lwd = 2)
}

# BASLINE MODEL
baseline_model <- lm(satisfied_feedback ~ ., data = model_data)
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)

i = 1
par(mfrow = c(2,2))
for(col in names(model_cols)[-1]){
  if(i%%4 == 0){
    par(mfrow = c(2,2))
  }
  plot(model_data[,col], residuals(baseline_model), xlab = col)
  abline(h = 0)
}

# Drop Subject Specific Unis?
# model_data <- model_data[!(INSITUTION_NAME %in% c("SOAS ", "Goldsmiths ", "University of the Arts London ")),]
# INSTITUTION_NAME <- rownames(model_data)

# STEP MODEL
step_model <- step(baseline_model, direction = "both")
summary(step_model)
par(mfrow = c(2,2))
plot(step_model)


# LASSO (with CV)
mod.glm <- cv.glmnet(x = as.matrix(model_data[,-1]), y = model_data$satisfied_feedback) ## lasso regression
mod.glm.mse <- mean((model_data$satisfied_feedback - predict(mod.glm, newx = as.matrix(as.matrix(model_data[,-1]))))^2)
coef(mod.glm) ## show coefficients

# JAGS/INLA MODEL

