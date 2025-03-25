#setwd("/Users/cyrusseyrafi/Documents/GitHub/srs-assignment/assignment_2")
setwd("C:/Users/BCapo/Desktop/University of Edinburgh Masters/Sem 2/srs-assignment/assignment_2")
#setwd('/Users/user/Documents/Statistics with Data Science/Semester 2/Statistical Research Skills/Assignment2&3')


######                        NOTES/UNUSED CODE                           ######

# ElasticNet needs to use an updated version of model.interactions as there    #
# is now a 3 way interaction feature.                                          #
# In GRM models, need to invert the link function to retrieve the actual coefs # 

######                            IMPORTS                                ######

library(ggplot2)
library(brms)
library(fBasics)


######                       READ DATA + CLEAN                           ######

# Read the data and read the first few rows.
data = read.csv("data.csv")
head(data)
# Set the index for the data through the INSTITUTION_NAME.
INSTITUTION_NAME <- data$INSTITUTION_NAME
rownames(data) <- INSTITUTION_NAME
# Remove 'X.1', 'X', and 'INSITUTION_NAME' cols.
data = data[,-c(1,2,3)]

# Show the first few rows and look at the summary statistics/data types.
head(data)
str(data)
summary(data)

# Total seems to have an outlier at the maximum when compared to the 3rd 
# quartile but we don't anticipate on using this column so we can leave it.
# Can see from summaries that continuation appears to be a character column when
# it should be numeric. FIX CONTINUATION BY LOOKING AT IT.
data$continuation
# APPEARS TO BE "n/a" so replace with NA and convert it to a numeric column.
data$continuation <- gsub("n/a", NA, data$continuation)
data$continuation <- as.numeric(data$continuation)

# Look at other NA values in the data by column
sapply(data, function(x) sum(is.na(x)))
# Look at NA value of continuation. Appears to be just Falmouth so let's drop as
# we don't want to make assumptions about their continuation.
data[is.na(data$cont), ]

# Our other NA value is in satisfied feedback. Unsurprisingly Cambridge here 
#(Oxford in most other years too). Assume the same as Oxford due to rivalry.
data[is.na(data$satisfied_feedback), ]
data["Cambridge ","satisfied_teaching"] <- data["Oxford ","satisfied_teaching"]
data["Cambridge ","satisfied_feedback"] <- data["Oxford ","satisfied_feedback"]

# Get column indices and names by using a named vector as a 'dictionary'.
cols = c(1:ncol(data))
names(cols) = names(data)
# Remove SIMD data as useless and reset the column indices and names.
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]
cols = c(1:ncol(data))
names(cols) = names(data)

# Now we have all the columns of interest, remove the only remaining column with
# a remaining NA value (Falmouth) and get the new index.
data <- na.omit(data)
INSTITUTION_NAME <- rownames(data) 

# Split the columns into their different types.
institutional_cols <- c("satisfied_feedback", "satisfied_teaching", 
                        "students_staff_ratio", "spent_per_student",
                        "avg_entry_tariff")
outcome_cols <- c("added_value", "career_after_15_month", "continuation")
ethnic_cols <- c("White.ethnic.group", "Black.ethnic.group",
                 "Asian.ethnic.group", "Other.ethnic.group", 
                 "Mixed.ethnic.group")
POLAR_cols <- c("POLAR4.Q1", "POLAR4.Q2", "POLAR4.Q3", "POLAR4.Q4", "POLAR4.Q5")
sex_cols <- c("Men", "Women")

# Check that the percentages add to 100 for these columns.
rowSums(data[,cols[ethnic_cols]])
rowSums(data[,cols[POLAR_cols]])
rowSums(data[,cols[sex_cols]])

# The sums of these rows do not add to 100%. This indicates non respondents so
# let's assume non-respondents are uniform across categories and standardise:
data[,cols[ethnic_cols]] <- data[,cols[ethnic_cols]] / 
  rowSums(data[,cols[ethnic_cols]])
data[,cols[POLAR_cols]] <- data[,cols[POLAR_cols]] / 
  rowSums(data[,cols[POLAR_cols]])
data[,cols[sex_cols]] <- data[,cols[sex_cols]] / 
  rowSums(data[,cols[sex_cols]])

# From the summary, we can see there is a large outlier in the other ethnicity
# column so let's look at what Uni it is. 
other_ethnic_outlier <- data[which(data$Other.ethnic.group ==
                                     max(data$Other.ethnic.group)),]
other_ethnic_outlier
# Appears to be Birmingham Newman Uni. We will need to fix this if we use this 
# column in the modelling.


######                                EDA                               ######

# Plot histogram showing the response, before adding the kde to look at the shape.
par(mfrow = c(1,1))
hist(data$satisfied_feedback, freq = FALSE, breaks = 10, 
     ylim = c(0,0.12), col="lightblue") 
dens <- density(data$satisfied_feedback)
lines(dens, col = "red")
# This is approximately normal with a slight left skew. It's not too extreme so
# lets leave it for now.

# Let's look at linear correlations of features with each other and with the 
# response variable using scatter plots, a heatmap, and a pairplot.
i = 0
for(col in names(cols)[-1]){
  if(i %% 4 == 0){
    par(mfrow = c(2,2))
  }
  # Plot the scatterplot
  plot(data[,col], data$satisfied_feedback, col="lightblue", 
       main=col, xlab = col)
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
         display_numbers = TRUE,  # Display correlation values
         number_format = "%.2f",  # Format numbers to 2 decimal places
         cluster_rows = FALSE,  # Remove hieracichal clustering
         cluster_cols = FALSE)  
pairs(data[,-1])

# POLAR4 Q1-Q3 have positive correlations with feedback, Q4 has none, and Q5 is
# negatively correlated.
# Added_value and sex columns seem uncorrelated with feedback. Ethnicity columns
# seem problematic to include despite having some correlations but they are tiny
# and likely depend on economic background which we can't stratify by. e.g. a
# uni with more POLAR5.Q5 does not imply the black students there are.
# Continuation and satisfied_teaching looking a bit quadratic so could try these
# as terms in a linear model.

## edinburgh colours
edi_blue = rgb(4/255.0, 30/255.0, 66/255.0)
edi_red = rgb(193/255.0, 0, 67/255.0)

# Some boxplots to look at the distribution of all of the covariates grouped
# by there type side by side.
par(mfrow = c(1,1))
boxplot(data[,sex_cols], main="Distribution of Sex", col=rainbow(5))
# Lots more Women in higher education than men as expected. No extreme outliers 
# here.

boxplot(data[,POLAR_cols], main = "Distribution of POLAR4 Scores")
boxplot(data[,POLAR_cols], main="Distribution of POLAR4 Scores", col=rainbow(5))

boxplot(data[,POLAR_cols], main="Distribution of POLAR4 Scores",
        col=rgb(4/255.0, 30/255.0, 66/255.0), whiskcol = rgb(193/255.0, 0, 67/255.0),
        staplecol = rgb(193/255.0, 0, 67/255.0), border = 'white',
        outcol = rgb(193/255.0, 0, 67/255.0), boxwex = 0.7, cex.main = 2,
        cex.axis = 1.6, las = 1, whisklwd = 3, staplelwd = 3,
        medlwd = 4, medcol = rgb(193/255.0, 0, 67/255.0))
# Clear increasing trend with POLAR4 scores so their use appears justified 
# in the use of contextual offers. POLAR4.5 has the largest IQR by far.
boxplot(data[,ethnic_cols], main="Distribution of Ethnicities", col=rainbow(5))
# Other and mixed ethnicity are small in percentages so can combine. Can see
# Birmingham Newman Uni in the other ethnic group as an outlier. Therest look
# fine.

# Let's look at the distributions of each of the covariates by plotting 
# histograms and kdes.
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
# All relatively normally distributed but some are slightly skewed. Can consider
# the skewness of the covariates in the modelling.


######                FEATURE ENGINEERING?                ######

# Other and mixed ethnicity are small and there may be some overlap between 
# these groups so let's combine them for the model data and update col vectors:
data[, "Other.Mixed.ethnic.group"] <- data[,"Other.ethnic.group"] + data[,"Mixed.ethnic.group"]

# UK universities tend to have "BAME" groups. It should be noted these are seldom
# used in admissions, where POLAR4 quantiles are a larger decider on contextual
# offers. Let's add the group to the dataset.
data[, "BAME"] <- 1 - data[,"White.ethnic.group"]
ethnic_cols <- append(ethnic_cols, c("BAME", "Other.Mixed.ethnic.group"))

# Let's now plot and see whether there are any trends with the response variable.
par(mfrow = c(2,2))
plot(data[,"BAME"], data$satisfied_feedback, col="lightblue", main="BAME", xlab = "BAME")
model <- lm(data$satisfied_feedback ~ data[,"BAME"])
abline(model, col="red")
hist(data[,"BAME"], main = "BAME", xlab = "BAME", col = "lightblue", breaks = 10, freq = FALSE)
dens <- density(data[,"BAME"])
lines(dens, col = "red")
cor(data$BAME,data$satisfied_feedback)
# No correlations with satisfied feedback and right skewed. Differences in 
# economic background lead to substantial differences in life experiences between
# wealthy and poor BAME students and we can't stratify by this so let's leave it
# out of the model.

# Combine POLAR4 quintiles 1 and 2 as these are used for contextual offers at
# many UK Unis:
data$POLAR4.Q1Q2 <- data$POLAR4.Q1 + data$POLAR4.Q2
POLAR_cols <- append(POLAR_cols, "POLAR4.Q1Q2")

plot(data[,"POLAR4.Q1Q2"], data$satisfied_feedback, col="lightblue", main="POLAR4.Q1Q2", xlab = "POLAR4.Q1Q2")
model <- lm(data$satisfied_feedback ~ data[,"POLAR4.Q1Q2"])
abline(model, col="red")
hist(data[,"POLAR4.Q1Q2"], main = "POLAR4.Q1Q2", xlab = "POLAR4.Q1Q2", col = "lightblue", breaks = 10, freq = FALSE)
dens <- density(data[,"POLAR4.Q1Q2"])
lines(dens, col = "red")
cor(data$POLAR4.Q1Q2,data$satisfied_feedback)

# Decent correlationm so should include this. Slightly right skewed but otherwise 
# seems like a good feature to include.

# PCA ATTEMPTS?
par(mfrow = c(1,1))
pca_result <- prcomp(data[,c(POLAR_cols[1:5],ethnic_cols[1:5])], scale = TRUE)
screeplot(pca_result, type = "lines", main = "Scree Plot")

# 3 components are needed to explain most of the variance in the model so let's
# add these to the dataset.
data <- cbind(data, pca_result$x[,1:3])
PCA_cols <- c("PC1", "PC2", "PC3")

# Add the Russell Group unis as this is of particular interest in the UK when 
# discussing universities.
G5 <- c("Oxford ", "UCL ", "Imperial College ", "London School of Economics ", 
        "Cambridge ")
RG <- c(G5, "Birmingham ", "Bristol ", "Cardiff ", "Durham ", "Edinburgh ",
        "Exeter ", "Glasgow ", "King's College London ", "Leeds ", "Liverpool ",
        "Manchester ", "Newcastle ", "Nottingham ",  "Queen Mary ", 
        "Queen's Belfast", "Sheffield ", "Southampton ", "Warwick ", "York ")
data$RG <- 0
data[RG,]$RG <- 1
institutional_cols <- append(institutional_cols, "RG")

# Reset the colnames vector and select the covariates we want to include.
cols = c(1:ncol(data))
names(cols) = names(data)
model_data <- data[cols[c(institutional_cols, outcome_cols[-1], # Don't want added_value
                          POLAR_cols[length(POLAR_cols)] # Only POLAR4.Q1Q2
                          # ,ethnic_cols[length(ethnic_cols)],sex_cols[-1]
)]]

# Make a new named vector for columns in the model dataframe.
model_cols = c(1:ncol(model_data))
names(model_cols) = names(model_data)

# Scale all but the response variable and binary column in the model dataset.
model_data[,-model_cols[c("satisfied_feedback", "RG")]] <- scale(
  model_data[,-model_cols[c("satisfied_feedback", "RG")]])

# Use a heatmap to see any correlations with the response and any
# multicolinearity with our final data.
cor_matrix <- cor(model_data)
pheatmap(cor_matrix,
         display_numbers = TRUE,  # Display correlation values
         number_format = "%.2f",  # Use 2 decimal places
         cluster_rows = FALSE, # Hierachichal clustering is removed.
         cluster_cols = FALSE)

# Lets start by using a frequentist linear model to see how suitable it is.

covariates_added <- paste(colnames(model_data)[-1], collapse = " + ")
model_formula <- as.formula(paste(c("satisfied_feedback/100 ~ ", covariates_added, 
                                    " + I(satisfied_teaching^2) + I(continuation^2)"), collapse = ""))
baseline_model <- lm(model_formula, data = model_data)
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)

# Remove these three unis as they are outliers and rather different to the 
# majority of the other universities.
model_data <- model_data[!(row.names(model_data) %in% 
                             c("University of the Arts London ", "SOAS ", "Goldsmiths ")),]

# Refit 
baseline_model <- lm(model_formula, data = model_data)
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)



# We now see a lot of left skew which is similar to the response.

######                    Generalised Regression Model                    ######

# Let's new fit a Bayesian Linear Regression model using brms

# Our Linear Model  fits the response variable well but we have some left skew.
# This is unsurprising due to the skewage we can see in the earlier histograms. 
# Let's expand our work to a GLM which can account for left skew. We will use 
# the same linear predictor as before.

# A formula to confine satisfied_feedback to [0,1] so we can use the Beta model.
model_formula_beta <- as.formula(paste(c("satisfied_feedback/100 ~ ", covariates_added, 
                                         " + I(satisfied_teaching^2) + I(continuation^2)"), collapse = ""))

## Fit the different brms models. ##

# Set priors
#skew_prior <- set_prior('normal(-0.5, 0.5)', class = 'alpha')
coef_prior <- set_prior('normal(0, 100^2)', class = 'b')
phi_prior <- set_prior('gamma(1, 0.01)', class = 'phi')

# Normal #
mod.brms <- brm(model_formula_beta,
                data = model_data, family = gaussian(), iter = 5000)
# Skew Normal #
mod.brms.sn <- brm(model_formula_beta,
                   data = model_data, family = skew_normal(), prior = c(skew_prior,
                                                                        coef_prior), iter = 5000)

# Beta #
mod.brms.beta <- brm(model_formula_beta, data = model_data, 
                     family = Beta(), prior = c(coef_prior, phi_prior), iter = 5000)
# Student-t # 
mod.brms.t <- brm(model_formula_beta,
                  data = model_data, family = student(), iter = 5000)

# Compute looic
loo_normal <- loo(mod.brms) #,  to fix high k Pareto
loo_skewnormal <- loo(mod.brms.sn)
loo_beta <- loo(mod.brms.beta)
loo_t <- loo(mod.brms.t)

# Compare looic for each model
data.frame(model = c("Normal", "Skew Normal", "Beta", "Student-t"),
           loo = c(loo_normal$looic, loo_skewnormal$looic,
                   loo_beta$looic, loo_t$looic))

loo_compare(loo_normal, loo_skewnormal,
            loo_beta, loo_t)
# Appears Beta is the best model but interpretation is a bit trickier. Let's
# see if it is worth it from posterior predictive checks.

# Define a function to carry out all of the posterior predictive checks for us. 
model_checks <- function(model){
  print(summary(model), digits = 3)
  
  summary_statistics <- c("mean", "median", "min", "max", "skewness", "sd")
  summary_statistics_titles <- c("Mean", "Median", "Minimum", "Maximum",
                                 "Skewness", "Standard Devation")
  for(i in 1:length(summary_statistics)){
    ppcheck <- pp_check(model, type = "stat", stat = summary_statistics[i])
    ppcheck$layers[[2]]$aes_params$linewidth <- 2.5
    fig <- ppcheck +
      ggtitle(paste("Posterior Predictive Check of", summary_statistics_titles[i])) + 
      theme(plot.title = element_text(hjust = 0.6, size = 30),
            axis.title = element_text(size = 30),
            axis.text = element_text(size = 30),
            legend.text = element_text(size = 30),
            legend.title = element_text(size = 30)) +
      ylab("Density") + scale_color_manual(values = c(rgb(193/255.0, 0, 67/255.0))) +
      scale_fill_manual(values = rgb(4/255.0, 30/255.0, 66/255.0))
    if(summary_statistics[i] == "skewness"){
      fig <- fig + xlab("Skew")  
    }else{
      fig <- fig + xlab("Satisfied Feedback")
    }
    print(fig)
  }
  ppcheck <- pp_check(model, ndraws = 30)
  ppcheck + ggtitle("Posterior Predictive Check of Distribution") + 
    theme(plot.title = element_text(hjust = 0.6, size = 30),
          axis.title = element_text(size = 30),
          axis.text = element_text(size = 30),
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 30)) +
    geom_line(data = data.frame(x = density(model$data$`satisfied_feedback/100`)$x,
                                y = density(model$data$`satisfied_feedback/100`)$y),
              aes(x = x, y = y),
              color = rgb(193/255.0, 0, 67/255.0), size = 2.5) +
    xlab("Satisfied Feedback") + 
    ylab("Density") + scale_color_manual(values = c(rgb(193/255.0, 0, 67/255.0),
                                                    rgb(4/255.0, 30/255.0, 66/255.0, 1)))
}
model_checks(mod.brms)
model_checks(mod.brms.beta)

# It appears that the Beta model outshines the skew normal in almost every way.
# Lets look at some more plots to check for convergence.
# Empirical cdf
pp_check(mod.brms.beta, type = 'ecdf_overlay', ndraws = 30)
# Scatter plot for average over posterior distributions
pp_check(mod.brms.beta, type = 'scatter_avg', ndraws = 30)
# Traceplots
plot(mod.brms.beta)
