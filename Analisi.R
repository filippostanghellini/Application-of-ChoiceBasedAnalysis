#install.packages("idefix")
#install.packages("mlogit")
#install.packages("MASS")
#install.packages("reshape2")
#install.packages("car")

################################################################
### Analysis of Choice Based Conjoint survey data            ###
### The Multinomial Logit and Mixed Multinomial Logit models ###
################################################################


# load library for fitting multinomial logit models
library(dfidx)
library(mlogit)

# import the data about the choice_data Survey for conjoint analysis
choice_data <- read.csv("data/Choice_Data_Converted.csv", sep=";")
head(choice_data)

# see some descriptive statistics
summary(choice_data)
xtabs(choice ~ spec, data=choice_data)
xtabs(choice ~ vel, data=choice_data)
xtabs(choice ~ qual, data=choice_data)
xtabs(choice ~ priv, data=choice_data)
xtabs(choice ~ cost, data=choice_data)

# recode some variables
choice_data$spec <- factor(choice_data$spec, levels=c("Assistente","Codice","Content")) # change order of categories
choice_data$vel <- factor(choice_data$vel, levels=c("Lento","Veloce")) # change order of categories
choice_data$qual <- factor(choice_data$qual, levels=c("Sufficente","Ottimale")) # change order of categories
choice_data$priv <- factor(choice_data$priv, levels=c("Bassa","Alta")) # change order of categories
choice_data$cost <- as.factor(choice_data$cost) # convert the variable as qualitative

# Fitting a choice model with "mlogit" function
# mlogit requires the choice data to be in a special data format created using the
# dfidx() function. You pass your choice data to dfidx, along
# with a few parameters telling it how the data is organized.
# dfidx() accepts data in either a "long" or a "wide" format and you tell it
# which you have using the shape parameter.

# 1) Create ID
choice_data$choice_id <- with(choice_data, paste(resp.id, ques, sep = "_"))

# 2) dfix for choice_data
choice_data.mlogit <- dfidx(
  choice_data,
  idx    = list(c("choice_id", "resp.id"), "alt"),
  choice = "choice",
  shape  = "long"
)


# The resulting choice_data.mlogit is a "dfidx" class object that can be used to estimate
# a model with mlogit(). The syntax for mlogit uses formula notation
# similarly to other functions for regression models in R.
# However, it requires the use of symbol "|" to distinguish between alternative-specific
# and non-alternative specific variables.

m1 <- mlogit(choice ~ spec + vel + qual + priv + cost, data = choice_data.mlogit)
summary(m1)

# Fit the model without intercept parameters 
m2 <- mlogit(choice ~ spec + vel + qual + priv + cost | -1, data = choice_data.mlogit)
summary(m2)

# Test the restriction on the intercepts by comparing the two models
# through a likelihood ratio test
lrtest(m2, m1)

# Fit the model without intercept parameters and with price as a quantitative variable
m3 <- mlogit(choice ~ spec + vel + qual + priv
             + as.numeric(as.character(cost)) | -1, data = choice_data.mlogit)
summary(m3)
lrtest(m3, m2)

# We use model m3

# Compute the willingness to pay for privacy alta
#WTP privacy
-coef(m3)["privAlta"]/(coef(m3)["as.numeric(as.character(cost))"])

#WTP codice
-coef(m3)["specCodice"]/(coef(m3)["as.numeric(as.character(cost))"])

#WTP content
-coef(m3)["specContent"]/(coef(m3)["as.numeric(as.character(cost))"])

#WTP veloce
-coef(m3)["velVeloce"]/(coef(m3)["as.numeric(as.character(cost))"])

#WTP quality
-coef(m3)["qualOttimale"]/(coef(m3)["as.numeric(as.character(cost))"])

################################################################
################################################################

# Delta method for WTP
# It allow us to calculate standard error and CI for each WTP
library(car)

attributes <- c("specCodice", "specContent", "velVeloce", "qualOttimale", "privAlta")

wtp_table <- data.frame()

for(attr in attributes) {
  formula_str <- paste0("-", attr, " / `as.numeric(as.character(cost))`")
  dm <- deltaMethod(m3, formula_str)
  
  wtp_table <- rbind(wtp_table, data.frame(
    Attributo = attr,
    WTP = round(dm$Estimate, 2),
    SE = round(dm$SE, 2),
    CI_Lower = round(dm$`2.5 %`, 2),
    CI_Upper = round(dm$`97.5 %`, 2)
  ))
}

print(wtp_table)

################################################################
################################################################

# Simulate preference shares using the "predict.mnl" function
# Define the function
predict.mnl <- function(model, data) {
  # Function for predicting preference shares from a MNL model
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to
  # predict shares.  Same format at the data used to estimate model.
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  logitUtility <- data.model%*%model$coef
  share <- exp(logitUtility)/sum(exp(logitUtility))
  cbind(share, data)
}

################################################################
################################################################

# In order to use "predict.mnl", you need to define a data frame containing the set of designs
# for which you want to predict the preference shares.
# One way to do this is to create the full set of possible designs
# using expand.grid() and select the designs we want by row number
attributes <- list(spec=names(table(choice_data.mlogit$spec)),
               vel=names(table(choice_data.mlogit$vel)),
               qual=names(table(choice_data.mlogit$qual)),
               priv=names(table(choice_data.mlogit$priv)),
               cost=names(table(choice_data.mlogit$cost)))

allDesign <- expand.grid(attributes)
allDesign #all possible design

# we choose a reasonable and realistic subset (where the first row indicates our design), such as
new.data <- allDesign[c(2, 32, 28, 67, 6, 69), ]
new.data

# We then pass these designs to predict.mnl() to determine what customers
# would choose if they had to pick among these six choice_data alternatives:
predict.mnl(m3, new.data) # using m3 specification, price numerica

# Compute and plot preference share sensitivity
# Producing a sensitivity chart using R is relatively simple: we just need to loop through all
# the attribute levels, compute a preference share prediction, and save the predicted preference share for
# the target design. The "sensitivity.mnl" function does that.
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a preference share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame contining design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}
base.data <- new.data[1,]
competitor.data <- new.data[-1,]
(tradeoff <- sensitivity.mnl(m2, attributes, base.data, competitor.data))

barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level,
        ylab="Change in Share for the Planned Product Design",
        ylim=c(-0.1,0.11))
grid(nx=NA, ny=NULL)

# Colored barplot for our future implementation
cols <- rep("gray", length(tradeoff$level))

cols[tradeoff$level == "Alta"] <- "red"

barplot(tradeoff$increase,
        horiz = FALSE,
        names.arg = tradeoff$level,
        ylab = "Change in Share for the Planned Product Design",
        ylim = c(-0.1, 0.11),
        col = cols)
grid(nx = NA, ny = NULL)

################################################################
################################################################

# we want to test our product preference share 

new.data_new <- allDesign[c(14, 32, 28, 67, 6, 69), ]
new.data_new

predict.mnl(m3, new.data_new)

################################################################
################################################################

### Controlling for consumer heterogeneity

# Fit a mixed MNL model
# The statistical term for coefficients that vary across respondents (or customers) is
# random coefficients or random effects. To estimate a multinomial
# logit model with random coefficients using "mlogit", we define a vector indicating
# which coefficients should vary across customers. "mlogit" requires a character
# vector the same length as the coefficient vector with a letter code indicating what
# distribution the random coefficients should follow across the respondents: "n" for
# normal, "l" for log normal, "t" for truncated normal, and "u" for uniform. For this
# analysis, we assume that all the coefficients are normally distributed across the population
# and call our vector "m2.rpar".
m2.rpar <- rep("n", length=length(m2$coef))
names(m2.rpar) <- names(m2$coef)
m2.rpar

# We pass this vector to mlogit as the rpar parameter, which is short for "random
# parameters". In addition, we tell mlogit that we have multiple choice observations
# for each respondent (panel=TRUE) and whether we want to allow the random
# parameters to be correlated with each other. For this first run, we assume that we do
# not want random parameters to be correlated (correlation=FALSE), a setting
# we reconsider below.
m2.mixed <- mlogit(choice ~ spec + vel + qual + priv + cost | -1,
                  data = choice_data.mlogit,
                  panel=TRUE, rpar = m2.rpar, correlation = FALSE)
summary(m2.mixed)

# We can get a visual summary of the distribution of random effects and hence of the level of heterogeneity
plot(m2.mixed)

# We can extract the distribution of specific random effects using the function rpar()
specCodice.distr <- rpar(m2.mixed, "specCodice")
summary(specCodice.distr)
mean(specCodice.distr)
med(specCodice.distr)
plot(specCodice.distr)

# We can add that the random coefficients can be correlated
# This is easily done by including "correlations = TRUE"
# as an option in the call to mlogit or by using the update function
# provided by mlogit
m2.mixed2 <- update(m2.mixed, correlation = TRUE)
summary(m2.mixed2)

# To get a better sense of the strength of the association among random coefficients,
# we can extract the covariance matrix using "cov.mlogit"
# and then convert it to a correlation matrix using "cov2cor" from base R.
cov2cor(cov.mlogit(m2.mixed2))

# Calculate correlation matrix
cor_mat <- cov2cor(cov.mlogit(m2.mixed2))
library(ggplot2)
library(reshape2)

cor_mat <- cov2cor(cov.mlogit(m2.mixed2))

cor_long <- melt(cor_mat)
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation")

# We can also obtain the standard errors of the correlations among random effects,
# and hence perform significance test
summary(vcov(m2.mixed2, what = "rpar", type = "cor"))

# We may restrict the correlation to only random parameters with significant association
m2.mixed3 <- update(m2.mixed2, correlation = c("specCodice", "specContent", "privAlta", "velVeloce", "cost20", "cost25"))

# The significant presence of random coefficients and their correlation
# can be further investigated using the ML tests, such as the ML ratio test
lrtest(m2, m2.mixed) #Fixed effects vs. uncorrelated random effects
lrtest(m2.mixed, m2.mixed2) #Uncorrelated random effects vs. all correlated random effects
lrtest(m2.mixed3, m2.mixed2) #Partially correlated random effects vs. all correlated random effects

# m2.mixed2 results as the best model

# Simulating shares
# To compute share predictions with a mixed MNL model,
# we can use the "predict.mixed.mnl" function, which works in the same way as "predict.mnl",
# but with the difference that we now compute the preference shares for each of "nresp"
# newly sampled, representative respondents. The part worths for these respondents
# are drawn from a multivariate normal distribution with mean set at our estimated
# value of mu and covariance equal to our estimated value of Sigma (draws <-
# mvrnorm(n=nresp, coef.mu, coef.Sigma). The computation for each
# respondent is exactly the same as our computation in predict.mnl. Once we
# have the preference shares for all of the representative respondents, we average across respondents
# to get our overall preference share predictions.

library(MASS)
predict.mixed.mnl <- function(model, data, nresp=1000) {
  # Function for predicting shares from a mixed MNL model
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to
  #       predict shares. Same format at the data used to estimate model.
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}

set.seed(1111)
predict.mixed.mnl(m2.mixed2, data=new.data)
predict.mixed.mnl(m2.mixed2, data=new.data_new) # prediciton with our new product 

################################################################
################################################################

#INIZIO ANALISI DEMOGRAFICA

################################################################
################################################################

# Ensure the respondent ID column is named consistently across both datasets for merging
# Assuming 'demographic_data' also has a 'resp.id' or similar column.
# If the demographic data uses a different column name for respondent ID, e.g., 'id', it needs to be renamed.
# For example, if it's named 'id' in demographic_data:
# names(demographic_data)[names(demographic_data) == "id"] <- "resp.id"

# The choice_data.mlogit object is already prepared for mlogit, but for merging with demographic
# info on a respondent level, we might want to use the original choice_data frame or
# ensure resp.id is accessible from the mlogit object if necessary.

# Merge the choice_data with demographic_data using 'resp.id'
# Since choice_data has multiple rows per respondent (for each choice occasion and alternative),
# and demographic_data has one row per respondent, we'll merge them. This will add demographic
# columns to each row of choice_data based on resp.id.

# Ensure `resp.id` in demographic_data is treated as factor if `choice_data$resp.id` is a factor
# or if it needs to match types for a proper merge.

# First, identify distinct respondent IDs in the choice_data to ensure we only merge respondent-level data once
# or merge directly if demographic data applies to all choice instances for a given resp.id.

# Let's check if resp.id exists in demographic_data, and what its name is.
# Assuming demographic_data contains a column named 'resp.id' that matches `choice_data$resp.id`
# If it doesn't, you would need to adjust the column name in `demographic_data` or `by` argument.

# For simplicity, let's assume 'resp.id' is the common identifier.
# If `demographic_data` doesn't have a 'resp.id' column, we need to find the correct one.
# For now, let's try to inspect the loaded `demographic_data` to confirm its column names.

# Assuming `demographic_data` has a `resp.id` column already, or one that can be easily renamed.
# Let's inspect the `demographic_information.csv` structure if it's not `resp.id`
# The user context does not provide `demographic_information.csv` structure, so I will make an assumption.

# A common approach is to merge the full choice_data with the demographic_data.
# It's crucial that demographic_data only has one row per resp.id to avoid duplicate merges.

# If demographic_data has 'resp.id' as the key:
# Check structure first to avoid errors. The 'minivan' dataset was used in a later cell to get carpool data.
# It's possible `demographic_information.csv` is similar to `minivan` but contains more general info.

# Let's load the demographic data and assume the unique respondent ID is 'resp.id' based on prior context.
# If this is not the case, further inspection of 'demographic_information.csv' will be required.
demographic_data <- read.csv("data/demographic_information.csv", sep = ",") # Assuming comma as separator for this file
head(demographic_data)
summary(demographic_data)

# Check column names in demographic_data and choice_data
print(names(choice_data))
print(names(demographic_data))

# Assuming 'resp.id' is present in both and is the linking key.
# If not, you might need to rename a column in `demographic_data` or specify a different `by.x`, `by.y`.

# Perform the merge
# This will add demographic columns to each row of choice_data where resp.id matches.
# Correcting the merge by specifying 'by.x' and 'by.y'
choice_data_with_demographics <- merge(choice_data, demographic_data, by.x = "resp.id", by.y = "Respondent_ID", all.x = TRUE)

head(choice_data_with_demographics)
summary(choice_data_with_demographics)

# Now, if you need to re-run the mlogit model with demographic variables, you would use this new dataset
# and potentially incorporate demographic variables into the model formula.

################################################################
################################################################

PW.ind <- fitted(m2.mixed2, type = "parameters")
head(PW.ind) # part worth per person

################################################################
################################################################

names(PW.ind)[names(PW.ind) == "id"] <- "resp.id"
PW.ind_with_demographics <- merge(PW.ind, demographic_data, by.x = "resp.id", by.y = "Respondent_ID", all.x = TRUE)
head(PW.ind_with_demographics)

################################################################
################################################################

library(lattice)
#TODO: grafico età

histogram(~ privAlta | Age_Group, data = PW.ind_with_demographics,
          main = "Histogram of privAlta Part-Worth by Age Group",
          xlab = "privAlta Part-Worth", ylab = "Density")

histogram(~ privAlta | Education_Level, data = PW.ind_with_demographics,
          main = "Histogram of privAlta Part-Worth by Education_Level",
          xlab = "privAlta Part-Worth", ylab = "Density")

histogram(~ privAlta | Gender, data = PW.ind_with_demographics,
          main = "Histogram of privAlta Part-Worth by Gender",
          xlab = "privAlta Part-Worth", ylab = "Density")

histogram(~ specCodice | Education_Level, data = PW.ind_with_demographics,
          main = "Histogram of specCodice Part-Worth by Education_Level",
          xlab = "specCodice Part-Worth", ylab = "Density")

histogram(~ cost20 | Age_Group, data = PW.ind_with_demographics,
          main = "Histogram of cost20 Part-Worth by Age_group",
          xlab = "cost20 Part-Worth", ylab = "Density")

histogram(~ cost25 | Age_Group, data = PW.ind_with_demographics,
          main = "Histogram of cost25 Part-Worth by Age_group",
          xlab = "cost25 Part-Worth", ylab = "Density")

################################################################
################################################################

boxplot(privAlta ~ Age_Group, data = PW.ind_with_demographics,
        main = "Boxplot of privAlta Part-Worth by Age Group",
        xlab = "Age Group", ylab = "velVeloce Part-Worth")

boxplot(privAlta ~ Education_Level, data = PW.ind_with_demographics,
        main = "Boxplot of privAlta Part-Worth by Education_Level",
        xlab = "Education_Level", ylab = "privAlta Part-Worth")

boxplot(privAlta ~ Gender, data = PW.ind_with_demographics,
        main = "Boxplot of privAlta Part-Worth by Gender",
        xlab = "Gender", ylab = "privAlta Part-Worth")

boxplot(specCodice ~ Education_Level, data = PW.ind_with_demographics,
        main = "Boxplot of specCodice Part-Worth by Education_Level",
        xlab = "Education_Level", ylab = "specCodice Part-Worth")

boxplot(cost20 ~ Age_Group, data = PW.ind_with_demographics,
        main = "Boxplot of cost20 Part-Worth by Age_group",
        xlab = "Age_group", ylab = "cost20 Part-Worth")

################################################################
################################################################

# Perform statistical tests to assess the impact of 'Age_Group' on 'velVeloce' preferences. 
# Since 'Age_Group' has multiple categories, an ANOVA (Analysis of Variance) test is appropriate to determine if there are significant differences in the mean 'velVeloce' part-worth across the age groups.
anova_result <- aov(velVeloce ~ Age_Group, data = PW.ind_with_demographics)
summary(anova_result)

################################################################
################################################################

histogram(~ velVeloce | Gender, data = PW.ind_with_demographics,
          main = "Histogram of velVeloce Part-Worth by Gender",
          xlab = "velVeloce Part-Worth", ylab = "Density")

################################################################
################################################################

boxplot(velVeloce ~ Gender, data = PW.ind_with_demographics,
        main = "Boxplot of velVeloce Part-Worth by Gender",
        xlab = "Gender", ylab = "velVeloce Part-Worth")

################################################################
################################################################

by(PW.ind_with_demographics$velVeloce, PW.ind_with_demographics$Gender, mean)

################################################################
################################################################

anova_gender_result <- aov(velVeloce ~ Gender, data = PW.ind_with_demographics)
summary(anova_gender_result)

################################################################
################################################################

# Define the variables of interest with default values
part_worth_variable <- "velVeloce"
demographic_variable <- "Age_Group"

# Ensure the necessary library for plotting is loaded
library(lattice)

# 1. Generate a histogram of part_worth_variable by demographic_variable
histogram(as.formula(paste("~ ", part_worth_variable, " | ", demographic_variable)),
          data = PW.ind_with_demographics,
          main = paste("Histogram of", part_worth_variable, "by", demographic_variable),
          xlab = paste(part_worth_variable, "Part-Worth"), ylab = "Density")

# 2. Generate a boxplot of part_worth_variable by demographic_variable
boxplot(as.formula(paste(part_worth_variable, "~ ", demographic_variable)),
        data = PW.ind_with_demographics,
        main = paste("Boxplot of", part_worth_variable, "by", demographic_variable),
        xlab = demographic_variable, ylab = paste(part_worth_variable, "Part-Worth"))

# 3. Calculate the mean part_worth_variable for each category of demographic_variable
cat(paste("\nMean", part_worth_variable, "by", demographic_variable, ":\n"))
print(by(PW.ind_with_demographics[[part_worth_variable]],
         PW.ind_with_demographics[[demographic_variable]],
         mean))

# 4. Perform an appropriate statistical test
num_categories <- length(unique(PW.ind_with_demographics[[demographic_variable]]))

if (num_categories > 2) {
  # Perform ANOVA test if more than two unique categories
  cat(paste("\nPerforming ANOVA test for", part_worth_variable, "by", demographic_variable, ":\n"))
  anova_result <- aov(as.formula(paste(part_worth_variable, "~ ", demographic_variable)),
                      data = PW.ind_with_demographics)
  print(summary(anova_result))
} else if (num_categories == 2) {
  # Perform t-test if exactly two unique categories
  cat(paste("\nPerforming t-test for", part_worth_variable, "by", demographic_variable, ":\n"))
  t_test_result <- t.test(as.formula(paste(part_worth_variable, "~ ", demographic_variable)),
                          data = PW.ind_with_demographics)
  print(t_test_result)
} else {
  cat("\nNot enough categories in the demographic variable to perform a statistical test.\n")
}

