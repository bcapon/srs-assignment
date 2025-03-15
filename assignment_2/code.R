#setwd("/Users/cyrusseyrafi/srs-assignment/assignment_2")
setwd("C:/Users/BCapo/Desktop/University of Edinburgh Masters/Sem 2/srs-assignment/assignment_2")


######                        NOTES/FUNCTIONS                            ######

# Need to fix TOTAL for CCCU if we use that column
# Also need to fix the outlier in OTHER ETHNIC GROUP. Won't if using BAME.

# RESIDUAL PLOTS
#residual_plots <- function(model, data, response) {
  # Extract predicted values.
  #predicted <- predict(model, data, type = "response")
  # Compute residuals.
  #residuals <- data[[response]] - predicted
  # Standardize residuals.
  #sigma_hat <- sd(residuals)
  #residuals <- residuals / sigma_hat
  # Initialize plots.
  #par(mfrow = c(1, 3))
  ## Residuals vs. Fitted plot.
  #plot(predicted, residuals, main = "Residuals vs. Predicted",
   #    xlab = "Predicted Values", ylab = paste("Residuals"), pch = 19)
  #abline(h = 0, col = "red", lty = 2)
  ## Histogram of Residuals.
  #hist(residuals, breaks = 30, probability = TRUE, 
   #    main = "Histogram of Residuals", xlab = paste("Residuals"))
  #curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), add = TRUE, col = "blue")
  ## Q-Q Plot.
  #qqnorm(residuals, main = "Q-Q Plot of Residuals")
  #qqline(residuals, col = "red", lwd = 2)
#}


######                            IMPORTS                                ######

library(ggplot2)
library(glmnet)
library(brms)
library(MASS)
library(INLA)


######                       READ DATA + CLEAN                           ######

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
# APPEARS TO BE "n/a" so replace with NA and  convert it to numeric column:
data$continuation <- gsub("n/a", NA, data$continuation)
data$continuation <- as.numeric(data$continuation)

# Look at other NA values in the data by column
sapply(data, function(x) sum(is.na(x)))
# Look at NA value of continuation. Appears to be just Falmouth so let's drop for
# the time being as we are unsure how to impute it fairly:
data[is.na(data$cont), ]
# Unsurprisingly Cambridge here (Oxford in most other years too). Assume the same
# as Oxford due to their rivalry.
data[is.na(data$satisfied_feedback), ]
data["Cambridge ","satisfied_teaching"] <- data["Oxford ","satisfied_teaching"]
data["Cambridge ","satisfied_feedback"] = data["Oxford ","satisfied_feedback"]

# Get column indices and names by using a named list/vector as a 'dictionary'.
cols = c(1:ncol(data))
names(cols) = names(data)
# Remove SIMD data as useless and reset the column indices and names:
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]
cols = c(1:ncol(data))
names(cols) = names(data)

# Now we have all the columns of interest, remove the only remaining column with
# an NA value (Falmouth) and get the new index:
data <- na.omit(data)
INSTITUTION_NAME <- rownames(data) 

# Split the columns into their different types:
institutional_cols <- c("satisfied_feedback", "satisfied_teaching", 
                        "students_staff_ratio", "spent_per_student",
                        "avg_entry_tariff")
outcome_cols <- c("added_value", "career_after_15_month", "continuation")
ethnic_cols <- c("White.ethnic.group", "Black.ethnic.group",
                 "Asian.ethnic.group", "Other.ethnic.group", 
                 "Mixed.ethnic.group")
POLAR_cols <- c("POLAR4.Q5", "POLAR4.Q1", "POLAR4.Q2", "POLAR4.Q4", "POLAR4.Q3")
sex_cols <- c("Men", "Women")

# The sums of these rows do not add to 100%. This indicates non respondents so
# let's assume non-respondents are uniform across categories and standardise:
rowSums(data[,cols[ethnic_cols]])
rowSums(data[,cols[POLAR_cols]])
rowSums(data[,cols[sex_cols]])
data[,cols[ethnic_cols]] <- data[,cols[ethnic_cols]] / 
  rowSums(data[,cols[ethnic_cols]])
data[,cols[POLAR_cols]] <- data[,cols[POLAR_cols]] / 
  rowSums(data[,cols[POLAR_cols]])
data[,cols[sex_cols]] <- data[,cols[sex_cols]] / 
  rowSums(data[,cols[sex_cols]])

# N.B. we have a large outlier in Birmingham Newman Uni in the Other ethnic group.
# Will need to deal with this if we use it in the model but leave for now.
other_ethnic_outlier <- data[which(data$Other.ethnic.group==max(data$Other.ethnic.group)),]
other_ethnic_outlier

######                                EDA                               ######

# Plot histogram showing the response with a kde:
par(mfrow = c(1,1))
hist(data$satisfied_feedback, freq = FALSE, breaks = 10, ylim = c(0,0.12), col="lightblue") 
# Estimate the density
dens <- density(data$satisfied_feedback)
# Overlay the density curve
lines(dens, col = "red")
# This is approximately normal with a slight left skew. Let's look into this later.

# Let's look at linear correlations of features with each other and with the 
# response variable with scatterplots and a heatmap.
i = 0
for(col in names(cols)[-1]){
  if(i %% 4 == 0){
    par(mfrow = c(2,2))
  }
  # Plot the scatterplot
  plot(data[,col], data$satisfied_feedback, col="lightblue", main=col, xlab = col)
  # Fit a linear model and add the line to the scatterplot
  model <- lm(data$satisfied_feedback ~ data[,col])
  # Add Regression Line
  abline(model, col="red")
  i = i + 1
  ## GGPLOT2 VERSION
 # print(col)
  #plotted <- ggplot(data[,-1], aes(x = .data[[col]], y = satisfied_feedback)) +
   # geom_point() +
  #  geom_smooth(method = "lm", se = TRUE) + 
  #  theme_minimal()  
  #print(plotted)
}
library(pheatmap)
cor_matrix <- cor(data[,-1])
pheatmap(cor_matrix,
         color = colorRampPalette(c("blue", "white", "red"))(100),  # Color gradient
         display_numbers = TRUE,  # Display correlation values
         number_format = "%.2f",  # Format numbers to 2 decimal places
         cluster_rows = FALSE,  # Disable row clustering
         cluster_cols = FALSE)  # Disable column clustering
pairs(data[,-1])
# POLAR4 Q1-Q3 have positive correlations with feedback, Q4 has none, and Q5 is
# negatively correlated.
# Added_value and sex columns seem uncorrelated with feedback. Ethnicity columns
# seem problematic to include despite having some correlations but they are small
# and likely depend on economic background which we can't stratify by. e.g. a
# uni with more POLAR5.Q5 does not imply the black students there are.
# Continuation and satisfied_teaching looking a bit quadratic, consider later.

# POLAR4.Q4 bad cor but the rest are good?? Added value and total: bad. Could do
# feature engineering/transformations on ethnic/gender columns to improve but
# overall not great.
# Some boxplots to look at the distribution of all of the covariates grouped
# by there type side by side:
par(mfrow = c(1,1))
boxplot(data[,sex_cols], main="Distribution of Sex", col=rainbow(5))
boxplot(data[,POLAR_cols], main="Distribution of POLAR4 Scores", col=rainbow(5))
boxplot(data[,ethnic_cols], main="Distribution of Ethnicities", col=rainbow(5))
# Other and mixed ethnicities are small in percentages so can combine. No extreme
# outliers here though.

i = 0
for(col in names(cols)[-1]){
  if(i %% 4 == 0){
    par(mfrow = c(2,2))
  }
  hist(data[,col], main = col, xlab = col, col = "lightblue", breaks = 10, freq = FALSE)
  # Estimate density
  dens <- density(data[,col])
  # Overlay density curve
  lines(dens, col = "red")
  i = i + 1
}
# All relatively normal distributed but some are slightly skewed. Can consider
# later.

######                FEATURE ENGINEERING?                ######

# Other and mixed are small and there may be some overlap between these groups so
# let's combine them for the model data and update col vectors:
# data[, "Other.Mixed.ethnic.group"] <- data[,"Other.ethnic.group"] + data[,"Mixed.ethnic.group"]
data[, "BAME"] <- 1 - data[,"White.ethnic.group"]
ethnic_cols <- append(ethnic_cols,"BAME")
cor(data$BAME,data$satisfied_feedback)
hist(data[,"BAME"], main = "BAME", xlab = "BAME", col = "lightblue", breaks = 10, freq = FALSE)
dens <- density(data[,"BAME"])
lines(dens, col = "red")
# No correlations with satisfied feedback and right skewed.

# Combine POLAR4 quintiles 1 and 2 as these are used for contextual offers at
# many UK Unis:
data$POLAR4.Q1Q2 <- data$POLAR4.Q1 + data$POLAR4.Q2
POLAR_cols <- append(POLAR_cols, "POLAR4.Q1Q2")
cor(data$POLAR4.Q1Q2,data$satisfied_feedback)
hist(data[,"POLAR4.Q1Q2"], main = "POLAR4.Q1Q2", xlab = "POLAR4.Q1Q2", col = "lightblue", breaks = 10, freq = FALSE)
dens <- density(data[,"POLAR4.Q1Q2"])
lines(dens, col = "red")
# Decent correlationm so should include this. Slightly right skewed but otherwise good.

# Reset the colnames vector
cols = c(1:ncol(data))
names(cols) = names(data)

model_data <- data[cols[c(institutional_cols, outcome_cols[-1], # Don't want added_value
                          POLAR_cols[length(POLAR_cols)] # Only POLAR4.Q1Q2
                        # ,ethnic_cols[length(ethnic_cols)],sex_cols[-1]
                          )]]
model_data[,-1] <- scale(model_data[,-1])

# Add the Russell Group unis as this is of particular interest in the UK when 
# discussing universities.
G5 <- c("Oxford ", "UCL ", "Imperial College ", "London School of Economics ", 
        "Cambridge ")
RG <- c(G5, "Birmingham ", "Bristol ", "Cardiff ", "Durham ", "Edinburgh ",
        "Exeter ", "Glasgow ", "King's College London ", "Leeds ", "Liverpool ",
        "Manchester ", "Newcastle ", "Nottingham ",  "Queen Mary ", 
        "Queen's Belfast", "Sheffield ", "Southampton ", "Warwick ", "York ")
model_data$RG <- 0
model_data[RG,]$RG <- 1
# Update the cols to match the model data:
cols = c(1:ncol(model_data))
names(cols) = names(model_data)

# Use a heatmap to see any multicolinearity with our final data
cor_matrix <- cor(model_data)
pheatmap(cor_matrix,
         color = colorRampPalette(c("blue", "white", "red"))(100),  # Color gradient
         display_numbers = TRUE,  # Display correlation values
         number_format = "%.2f",  # Format numbers to 2 decimal places
         cluster_rows = FALSE,  # Disable row clustering
         cluster_cols = FALSE)  # Disable column clustering

######                             LINEAR MODEL                           ######

# BASLINE MODEL
baseline_model <- lm(satisfied_feedback ~ ., data = model_data)# weights = data$Total
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)

# Remove these two unis as they are outliers and are two subject specific.
model_data <- model_data[!(row.names(model_data) %in% 
                             c("University of the Arts London ", "SOAS ")),]

# BASLINE MODEL without UAL and SOAS
baseline_model <- lm(satisfied_feedback ~ ., data = model_data)# weights = data$Total
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)

i = 1
par(mfrow = c(2,2))
for(col in names(cols)[-1]){
  if(i%%4 == 0){
    par(mfrow = c(2,2))
  }
  plot(model_data[,col], residuals(baseline_model), xlab = col)
  abline(h = 0)
}


# BASLINE MODEL WITH INTERACTIONS
#interaction_string <- function(cols1, cols2){
 # result <- c()
#  for(ethnic in cols1){
#    for(POLAR in cols2){
#      result <- c(result, paste(ethnic, ":", POLAR, sep = ""))
#    }
#  }
#  interactions <- paste(result, collapse = " + ")
#  return(interactions)
#}
#RG_interactions <- add_interactions(c("RG"), names(cols[-c(1,14)]))

#model_interactions <- lm(as.formula(paste("satisfied_feedback ~ .", 
 #                               RG_interactions, sep = " + ")), data = model_data)

# Add the qudratic terms
model_quadratics <- lm(satisfied_feedback ~ . + I(satisfied_teaching^2)
                                  + I(continuation^2) 
                                   ,data = model_data)
summary(model_quadratics)
par(mfrow = c(2,2))
plot(model_quadratics)
anova(model_quadratics, baseline_model) #Significant ***

# Add some interactions
model_interactions <- lm(satisfied_feedback ~ . + I(satisfied_teaching^2)
                                  + I(continuation^2) 
                                  + satisfied_teaching:students_staff_ratio 
                                  + POLAR4.Q1Q2:avg_entry_tariff
                                  ,data = model_data)
summary(model_interactions)
par(mfrow = c(2,2))
plot(model_interactions)
anova(model_interactions, model_quadratics) # Significant interactions *

# Create a formula to use in GLM setting
model_formula <- as.formula(satisfied_feedback ~ . + I(satisfied_teaching^2)
                            + I(continuation^2) 
                            + satisfied_teaching:students_staff_ratio 
                            + POLAR4.Q1Q2:avg_entry_tariff)

# STEP model could also be used instead?
step_model <- step(model_interactions, direction = "both")
summary(step_model)
par(mfrow = c(2,2))
plot(step_model)
anova(step_model, model_interactions) # Smaller model (STEP) should be used.


## STEP MODEL WITH 2ND ORDER INTERACTIONS.
base_model_int <- lm(satisfied_feedback ~ (.)^2, data = model_data)
step_model_int <- step(base_model_int, direction = 'both', trace = 0)
summary(step_model_int) ## higher R^2 (but probably too many coefficients) -> 
#could use the terms in lasso to get the "most meaningful" ones
plot(step_model_int)
anova(base_model_int, model_interactions) 
# model_interactions is smaller here and should be used.


######                    Generalised Regression Model                    ######

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
res.lasso <- coef(mod.glm.step) ## get rid of less significant interactions/covariates


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


## NEW BRMS model:
## use prior with relatively small variance to keep coefficients near zero (L2 regularisation)
coef_prior <- set_prior('normal(0, 1)', class = 'b')
## use beta family to model scaled feedback in [0, 1] (/100 as feedback in [0, 100])
brms_mod_interactions <- brm(satisfied_feedback/100 ~ . + I(satisfied_teaching^2)
                              + I(continuation^2) 
                              + satisfied_teaching:students_staff_ratio
                              + POLAR4.Q1Q2:avg_entry_tariff
                              + POLAR4.Q1Q2:added_value, data = model_data[, -c(9, 11)], ## exclude ethnicity and gender
                             family = Beta(), prior = coef_prior, iter = 6000)
summary(brms_mod_interactions)
pp_check(brms_mod_interactions, ndraws = 30)
## empirical cdf
pp_check(brms_mod_interactions, type = 'ecdf_overlay', ndraws = 30)
## scatter plot for average over posterior distributions
pp_check(brms_mod_interactions, type = 'scatter_avg', ndraws = 30)

post.pred <- colMeans(posterior_predict(brms_mod_interactions)) * 100 ## transform back
cat("MSE brms with beta:", mean((model_data$satisfied_feedback - post.pred)^2))


# LASSO (with CV)
mod.glm <- cv.glmnet(x = as.matrix(model_data[,-1]), y = model_data$satisfied_feedback) 
mod.glm.mse <- mean((model_data$satisfied_feedback - 
                       predict(mod.glm,newx = as.matrix(as.matrix(model_data[,-1]))))^2)
coef(mod.glm) ## show coefficients



## ELASTIC NET
## include interactions and squared terms
## (terminology here not completely right, continuation:continuation would normally
## be equal to continuation, not continuation^2)
data.glm <- create.interactions(c("satisfied_teaching:students_staff_ratio",
                                  "POLAR4.Q1Q2:avg_entry_tariff", "POLAR4.Q1Q2:added_value",
                                  'satisfied_teaching:satisfied_teaching', 'continuation:continuation'),
                                model_data[, -c(1, 9, 11)]) ## exclude response, women, and ethnicity
glm.elnet <- cv.glmnet(x = as.matrix(data.glm), y = model_data$satisfied_feedback, alpha = 0.5)  ## elastic net
glm.elnet.mse <- mean((model_data$satisfied_feedback - 
                       predict(glm.elnet,newx = as.matrix(data.glm)))^2)
coef(glm.elnet) ## show coefficients

##   JAGS/INLA MODEL    ##

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
Beta.Prior <- list(mean.intercept = 0, prec.intercept = 0.0001, mean = 0, prec = 0.0001)
prec.prior <- list(prec = list(prior = "loggamma", param = c(1,0.11)))

model1 <- inla(satisfied_teaching ~ satisfied_feedback + spent_per_student + avg_entry_tariff
               + career_after_15_month + continuation, data = data, family = "gamma",
               control.family = list(hyper=prec.prior), control.fixed = 
                 Beta.Prior,control.compute = list(cpo=TRUE, waic=TRUE))

cat("WAIC:",model1$waic$waic, "\n")
cat("NSLCPO:",-sum(log(model1$cpo$cpo)), "\n")
summary(model1)           

model2<- glm(satisfied_teaching ~ satisfied_feedback + spent_per_student + avg_entry_tariff
             + career_after_15_month + continuation, data = data, family = Gamma)
summary(model2)
