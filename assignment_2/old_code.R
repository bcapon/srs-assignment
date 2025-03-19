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
#curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), 
#      add = TRUE, col = "blue")
## Q-Q Plot.
#qqnorm(residuals, main = "Q-Q Plot of Residuals")
#qqline(residuals, col = "red", lwd = 2)
#}


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
#                         RG_interactions, sep = " + ")), data = model_data)

# INLA CODE

##                                JAGS/INLA MODEL                             ##

# Generate x values for curves
#x_vals <- seq(min(data$satisfied_teaching), max(data$satisfied_teaching), length.out = 100)
# Fit distributions
#fit_gamma <- fitdistr(data$satisfied_teaching, "gamma")
#fit_lognorm <- fitdistr(data$satisfied_teaching, "lognormal")
#fit_exp <- fitdistr(data$satisfied_teaching, "exponential")
#fit_weibull <- fitdistr(data$satisfied_teaching, "weibull")
# Overlay fitted distribution curves
#curve(dgamma(x, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]), 
#      col = "red", lwd = 2, add = TRUE)

#curve(dlnorm(x, meanlog = fit_lognorm$estimate["meanlog"], sdlog = fit_lognorm$estimate["sdlog"]), 
#      col = "green", lwd = 2, add = TRUE, lty = 2)

#curve(dexp(x, rate = fit_exp$estimate["rate"]), 
#      col = "purple", lwd = 2, add = TRUE, lty = 3)

#curve(dweibull(x, shape = fit_weibull$estimate["shape"], scale = fit_weibull$estimate["scale"]), 
#      col = "orange", lwd = 2, add = TRUE, lty = 4)
# Add legend
#legend("topright", legend = c("Gamma", "Log-Normal", "Exponential", "Weibull"), 
#       col = c("red", "green", "purple", "orange"), lwd = 2, lty = c(1, 2, 3, 4))


# Specify a Gaussian model as a baseline
prec.prior <- list(prec=list(prior = "loggamma", param = c(0.1, 0.1)))

prior.beta <- list(mean.intercept = 80, prec.intercept = 0.01,
                   mean = 0, prec = 0.01)

INLA_gaussian <- inla(model_formula, 
                      data=model_data,
                      control.family=list(hyper=prec.prior),
                      control.fixed=prior.beta,
                      control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE))
summary(INLA_gaussian)           
cat("MSE is ", mean((INLA_gaussian$summary.fitted.values$mean - 
                       model_data$satisfied_feedback)^2))

# Now let's try 3 new models. Starting by allocating priors:
prec.prior.weibull <- list(alpha = list(prior = "pc.alphaw", param = 5))
prec.prior.gamma <- list(prec = list(prior = "loggamma", param = c(1,0.01))) 
prec.prior.beta <- list(prec = list(prior = "loggamma", param = c(1,0.01))) 

# Gamma
INLA_gamma <- inla(model_formula, data=model_data, 
                   family = "gamma",
                   control.family=list(hyper=prec.prior.gamma),
                   control.fixed=prior.beta, 
                   control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE))
cat("MSE is ", mean((INLA_gamma$summary.fitted.values$mean - 
                       model_data$satisfied_feedback)^2))
# Weibull
#INLA_weibull <- inla(model_formula, data=model_data, 
#                  family = "weibull", 
#                 control.family=list(hyper=prec.prior.weibull, variant = 1),
#                control.fixed=prior.beta, 
#                 control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE))
#cat("MSE is ", mean((INLA_weibull$summary.fitted.values$mean - 
#                       model_data$satisfied_feedback)^2))

# Beta
INLA_beta <- inla(model_formula_beta, data=model_data, 
                  family = "beta", 
                  #control.family=list(hyper=prec.prior.weibull, variant = 1),
                  control.fixed=prior.beta, 
                  control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE))

# Skew Normal
INLA_sn <- inla(model_formula, data=model_data, 
                family = "sn", 
                control.fixed=prior.beta, 
                control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE))
cat("MSE is ", mean((INLA_sn$summary.fitted.values$mean - 
                       model_data$satisfied_feedback)^2))
summary(INLA_sn)

# Compare the results for each model.
cat("Gaussian WAIC:",INLA_gaussian$waic$waic, "\n")
cat("Gamma WAIC:",INLA_gamma$waic$waic, "\n")
cat("Beta WAIC:",-INLA_beta$waic$waic, "\n")
cat("Skew Normal WAIC:",INLA_sn$waic$waic, "\n")



## ELASTIC NET
## include interactions and squared terms
## (terminology here not completely right, continuation:continuation would normally
## be equal to continuation, not continuation^2)
#data.glm <- create.interactions(c("satisfied_teaching:spent_per_student:students_staff_ratio"
#                                  ,"POLAR4.Q1Q2:avg_entry_tariff",
#                                  'satisfied_teaching:satisfied_teaching', 'continuation:continuation'),
#                                model_data[, -model_cols["satisfied_feedback"]]) ## exclude response
#glm.elnet <- cv.glmnet(x = as.matrix(data.glm), y = model_data$satisfied_feedback, alpha = 0.5)  ## elastic net
#glm.elnet.mse <- mean((model_data$satisfied_feedback - 
#                       predict(glm.elnet,newx = as.matrix(data.glm)))^2)
#coef(glm.elnet) ## show coefficients

# LASSO (with CV)
mod.glm <- cv.glmnet(x = as.matrix(model_data[,-1]), y = model_data$satisfied_feedback) 
mod.glm.mse <- mean((model_data$satisfied_feedback - 
                       predict(mod.glm,newx = as.matrix(as.matrix(model_data[,-1]))))^2)
coef(mod.glm) ## show coefficients


                        ########  LINEAR MODEL CODE  #########

## STEP MODEL WITH 2ND ORDER INTERACTIONS.
base_model_int <- lm(satisfied_feedback ~ (.)^2, data = model_data)
step_model_int <- step(base_model_int, direction = 'both', trace = 0)
summary(step_model_int) ## higher R^2 (but probably too many coefficients) -> 
#could use the terms in lasso to get the "most meaningful" ones
plot(step_model_int)
anova(base_model_int, model_interactions) 
# This model doesn't outperform our carefully selected one which is a good sign.

# BASLINE MODEL without UAL and SOAS
baseline_model <- lm(satisfied_feedback ~ ., data = model_data)# weights = data$Total
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)

# Look at the relationships between the covariates and the residuals.
i = 1
par(mfrow = c(2,2))
for(col in names(model_cols)[-1]){
  if(i%%4 == 0){
    par(mfrow = c(2,2))
  }
  plot(model_data[,col], residuals(baseline_model), xlab = col)
  abline(h = 0)
}

# The residual plots justify trying adding the qudratic terms mentioned earlier.
model_quadratics <- lm(satisfied_feedback ~ . + I(satisfied_teaching^2)
                       + I(continuation^2) 
                       ,data = model_data)
summary(model_quadratics)
par(mfrow = c(2,2))
plot(model_quadratics)
anova(model_quadratics, baseline_model) #Significant ***


# Add some interactions
model_interactions <- lm((satisfied_feedback) ~ . + I(satisfied_teaching^2)
                         + I(continuation^2) 
                         + spent_per_student:students_staff_ratio:satisfied_teaching
                         + POLAR4.Q1Q2:avg_entry_tariff
                         ,data = model_data)
summary(model_interactions)
par(mfrow = c(2,2))
plot(model_interactions)
anova(model_interactions, model_quadratics) # Significant interactions *

# STEP model could also be used instead?
step_model <- step(model_interactions, direction = "both")
summary(step_model)
par(mfrow = c(2,2))
plot(step_model)
anova(step_model, model_interactions) # Same model, perfect!