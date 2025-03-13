#setwd("/Users/cyrusseyrafi/srs-assignment/assignment_2")
setwd("C:/Users/BCapo/Desktop/University of Edinburgh Masters/Sem 2/srs-assignment/assignment_2")

# NOTES #
# Q1 is the 20% of areas with lowest participation in higher education
# Need to make all columns but INSTITUTION_NAME numeric.
# Canterbury Christ Church Uni seems to have a too high total???
# Women:Entry_tariff could be a good interaction
# Need to fix TOTAL for CCCU if we use that column
# Also need to fix the outlier in OTHER ETHNIC GROUP.
# Avg entry tariff and women interaction

######                            IMPORTS                               ######

library(ggplot2)
library(glmnet)
library(brms)

######                       READ DATA + CLEAN                          ######

# Read the data and read the first few rows.
data = read.csv("data.csv")
head(data)
# Get the index for the data through the INSTITUTION_NAME
INSTITUTION_NAME <- data$INSTITUTION_NAME
rownames(data) <- INSTITUTION_NAME

# Remove 'X.1' column and 'X' cols, R provides indices for us.
data = data[,-c(1,2,3)]
head(data)
str(data)
summary(data)

# Total seems to have an outlier at the maximum when compared to the 3rd quartile.
# Can see from summaries that continuation appears to be a character column when
# it should be numeric. FIX CONTINUATION BY LOOKING AT IT:
data$continuation

# APPEARS TO BE "n/a" converting it to string column
data$continuation <- gsub("n/a", NA, data$continuation)
data$continuation <- as.numeric(data$continuation)

# Look at other NA values in the data by column
sapply(data, function(x) sum(is.na(x)))

# Look at NA value of continuation. Appears to be Falmouth (Not sure the best way
# to impute that one, return to later)
data[is.na(data$cont), ]

# Unsurprisingly Cambridge here (Oxford in most other years too). Assume the same
# as Oxford due to their rivalry.
data[is.na(data$satisfied_feedback), ]
data["Cambridge ","satisfied_teaching"] <- data["Oxford ","satisfied_teaching"]
data["Cambridge ","satisfied_feedback"] = data["Oxford ","satisfied_feedback"]

# Get column indices and names by using a named list/vector as a 'dictionary'.
cols = c(1:ncol(data))
names(cols) = names(data)

# Remove SIMD data as useless.
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]

# NEED TO GET RID OF NA VALUES FIRST.
data <- na.omit(data)
cor(data[,-1])

# Reset the cols data and get the new INSTITUTION_NAME
cols = c(1:ncol(data))
names(cols) = names(data)
INSTITUTION_NAME <- rownames(data) 

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
ethnic_cols <- c("White.ethnic.group", "Black.ethnic.group",
                 "Asian.ethnic.group", "Other.ethnic.group", 
                 "Mixed.ethnic.group")
POLAR_cols <- c("POLAR4.Q1", "POLAR4.Q2", "POLAR4.Q4", "POLAR4.Q5", 
                "POLAR4.Q3")
sex_cols <- c("Women", "Men")

#rowSums(data[,cols[ethnic_cols]])
#rowSums(data[,cols[POLAR_cols]])
#rowSums(data[,cols[sex_cols]])

data[,cols[ethnic_cols]] <- data[,cols[ethnic_cols]] / 
                            rowSums(data[,cols[ethnic_cols]])
data[,cols[POLAR_cols]] <- data[,cols[POLAR_cols]] / 
                            rowSums(data[,cols[POLAR_cols]])
data[,cols[sex_cols]] <- data[,cols[sex_cols]] / 
                            rowSums(data[,cols[sex_cols]])

model_data <- data[cols[c("satisfied_feedback", "satisfied_teaching", 
                          "students_staff_ratio", "spent_per_student",
                          "avg_entry_tariff", "career_after_15_month",
                          "continuation", "Women")]]
model_data[,-1] <- scale(model_data[,-1])

G5 <- c("Oxford ", "UCL ", "Imperial College ", "London School of Economics ", 
        "Cambridge ")

RG <- c(G5, "Birmingham ", "Bristol ", "Cardiff ", "Durham ", "Edinburgh ",
        "Exeter ", "Glasgow ", "King's College London ", "Leeds ", "Liverpool ",
        "Manchester ", "Newcastle ", "Nottingham ",  "Queen Mary ", 
        "Queen's Belfast", "Sheffield ", "Southampton ", "Warwick ", "York ")
model_data$G5 <- 0
model_data[G5,]$G5 <- 1
model_data$RG <- 0
model_data[RG,]$RG <- 1
model_data$continuation_sq <- (model_data$continuation)^2
model_data$satisfied_teaching_sq <- (model_data$satisfied_teaching)^2

# Get column indices and names by using a named list/vector as a 'dictionary'.
model_cols = c(1:ncol(model_data))
names(model_cols) = names(model_data)

######                                MODELS                             ######

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
  print(model_data[,col])
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

## STEP MODEL WITH 2ND ORDER INTERACTIONS
base_model_int <- lm(satisfied_feedback ~ (.)^2, data = model_data)
step_model_int <- step(base_model_int, direction = 'backward', trace = 0)
summary(step_model_int) ## higher R^2 (but probably too many coefficients) -> could use the terms in lasso to get the "most meaningful" ones
plot(step_model_int)

## function to create interactions manually for lasso
create.interactions <- function(interaction.names, data){
  interactions <- interaction.names[grep(':', interaction.names)]
  for (name in interactions){
    name.split <- strsplit(name, ':')[[1]]
    ## get individual names
    name1 <- name.split[1]
    name2 <- name.split[2]
    ## add interaction (= product)
    data[[name]] <- data[[name1]] * data[[name2]]
  }
  return(data)
}

model_data_glm <- create.interactions(names(coef(step_model_int)), model_data)
## step can lead to overfitting and unstable results
mod.glm.step <- cv.glmnet(x = as.matrix(model_data_glm[, -1]), y = model_data_glm$satisfied_feedback)
coef(mod.glm.step) ## get rid of less significant interactions/covariates


# BRMS (BAYESIAN REGRESSION MODELS USING STAN)

## can also use brms (bayesian regression using stan) and the skew normal distribution
## -> can capture the slightly skewed distribution

## non-excluded features from lasso
non.excluded <- rownames(res.lasso)[which(res.lasso != 0)][-1]
## add formula to brms automatically
brms.formula <- paste(non.excluded, collapse = ' + ')

mod.brms <- brm(as.formula(paste("satisfied_feedback ~", brms.formula)),
                data = model_data_glm, family = skew_normal())
summary(mod.brms)
pp_check(mod.brms)
## MSE
post.pred <- colMeans(posterior_predict(mod.brms))
mse.mod.brms <- mean((model_data_glm$satisfied_feedback - post.pred)^2)


# LASSO (with CV)
mod.glm <- cv.glmnet(x = as.matrix(model_data[,-1]), y = model_data$satisfied_feedback) ## lasso regression
mod.glm.mse <- mean((model_data$satisfied_feedback - predict(mod.glm, newx = as.matrix(as.matrix(model_data[,-1]))))^2)
coef(mod.glm) ## show coefficients

# JAGS/INLA MODEL

# Plot histogram
hist(data$satisfied_teaching, probability = TRUE)
#, breaks = 30, probability = TRUE, 
#col = rgb(0, 0, 1, 0.5), border = "black",
#main = "Histogram with Fitted Distributions", xlab = "Satisfaction Score")

# Generate x values for curves
x_vals <- seq(min(data$satisfied_teaching), max(data$satisfied_teaching), length.out = 100)

# Fit distributions
fit_gamma <- fitdistr(data$satisfied_teaching, "gamma")
fit_lognorm <- fitdistr(data$satisfied_teaching, "lognormal")
fit_exp <- fitdistr(data$satisfied_teaching, "exponential")
fit_weibull <- fitdistr(data$satisfied_teaching, "weibull")

# Overlay fitted distribution curves
curve(dgamma(x, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]), 
      col = "red", lwd = 2, add = TRUE)

curve(dlnorm(x, meanlog = fit_lognorm$estimate["meanlog"], sdlog = fit_lognorm$estimate["sdlog"]), 
      col = "green", lwd = 2, add = TRUE, lty = 2)

curve(dexp(x, rate = fit_exp$estimate["rate"]), 
      col = "purple", lwd = 2, add = TRUE, lty = 3)

curve(dweibull(x, shape = fit_weibull$estimate["shape"], scale = fit_weibull$estimate["scale"]), 
      col = "orange", lwd = 2, add = TRUE, lty = 4)

# Add legend
legend("topright", legend = c("Gamma", "Log-Normal", "Exponential", "Weibull"), 
       col = c("red", "green", "purple", "orange"), lwd = 2, lty = c(1, 2, 3, 4))
# Specify a Gamma prior for variance (in the INLA model)
library(INLA)
Beta.Prior <- list(mean.intercept = 0, prec.intercept = 0.0001, mean = 0, prec = 0.0001)
prec.prior <- list(prec = list(prior = "loggamma", param = c(1,0.11)))

model1 <- inla(satisfied_teaching ~ satisfied_feedback + spent_per_student + avg_entry_tariff
               + career_after_15_month + continuation, data = data, family = "gamma",control.family = list(hyper=prec.prior), control.fixed = Beta.Prior,control.compute = list(cpo=TRUE, waic=TRUE))

cat("WAIC:",model1$waic$waic, "\n")
cat("NSLCPO:",-sum(log(model1$cpo$cpo)), "\n")
summary(model)           

model2<- glm(satisfied_teaching ~ satisfied_feedback + spent_per_student + avg_entry_tariff
             + career_after_15_month + continuation, data = data, family = Gamma)
summary(model2)

