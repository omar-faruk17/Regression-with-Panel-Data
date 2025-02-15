# For this chapter we use the dataset Fatalities from the AER package.
#..........................................................................................................

# This code is adapted from *Introduction to Econometrics with R* by Hank, Arnold, Gerber, and Schmelzer (Chapter: Regression with Panel Data).
# Fatalities dataset is a panel data the demonstrate annual state level observations on US traffic fatalities for the period 1982 through 1988.
# For panel data regression, we have to use plm library.

library(AER)
library(plm)
library(stargazer)
library(tidyverse)

# In contrast to cross-section data where we have observations on n subjects (entities), panel data has observation on n entites at T >= 2 time periods.

# Load Fatalities dataset.
data(Fatalities)
fatal <- Fatalities # Created a copy of the Fatalities dataset named 'fatal' for easier handling 

# Obtain the dimension and inspect the structure 
is.data.frame(fatal)
dim(fatal)
str(fatal)

# list of first few observations
head(fatal)

# summarize the variables 'state' and 'year'
summary(fatal$state)
summary(fatal$year)
summary(fatal [,c(1,2)]) # alternative code

# We found that the dataset consists of 34 variable and 336 observations. this dataset containing 48 states and has 7 levels of identifying the time period when the observaion was made. This gives us 7*8 = 336 observations in total.
# Since all variables are observed for all entites and over all time periods, the panel is balaced. If there were missing data for at least one entity in at least one time period we would call the panel is unbalanced.



#...................................................................................................................
# Finding relation between traffic deaths and alcohol taxes for years 1982 and 1988
# .............................................................................
# the traffic fatality rate, measured as the number of fatalities per 10000 inhabitants.
# define the fatality rate
fatal$fatal_rate <- fatal$fatal / fatal$pop * 10000

# subset the data based on year
fatal1982 <- subset(fatal, year == "1982")
fatal1988 <- subset(fatal, year == "1988")

# estimate simple regression models using 1982 and 1988 data
m_1982 <- lm(fatal_rate ~ beertax, data = fatal1982)
m_1988 <- lm(fatal_rate ~ beertax, data = fatal1988)

# showing the regression result
summary(m_1982)  # Uses OLS standard errors, assuming homoskedasticity  
coeftest(m_1982, vcov. = vcovHC, type = "HC1")  # Uses heteroskedasticity-robust standard errors  
coeftest(m_1988, vcov. = vcovHC, type = "HC1") 


# scatter plot the observations and add the estimated regression line for 1982 data 
plot (x= fatal1982$beertax,
      y = fatal1982$fatal_rate,
      xlab = "Beer tax (in 1988 dollars)",
      ylab = "Fatality rate (fatalities per 10000)",
      main = "Traffic fatality rates and beer taxes in 1982",
      ylim = c(0, 4.5),   # Syntax: ylim = c(min_value, max_value) [Set Y-axis limits between 0 and 4.5]
      pch = 20,           # Uses a solid circle as the point shape
      col = "steelblue")  # Colors the points steel blue
abline(m_1982, lwd = 1.5)


# Equivalent Code Using ggplot2
library(ggplot2)
ggplot(fatal1982, aes(x = beertax, y = fatal_rate)) +
  geom_point(color = "steelblue", size = 2) +  # Scatter plot with blue points
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.5) +  # Regression line
  labs(
    title = "Traffic Fatality Rates and Beer Taxes in 1982",
    x = "Beer Tax (in 1988 dollars)",
    y = "Fatality Rate (fatalities per 10,000)"
  ) +
  theme_minimal() +  # Uses a clean theme
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

# scatter plot for 1988 data
ggplot(fatal1988, aes(x = beertax, y = fatal)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "lightcoral", linewidth = 1) +
  labs(
    title = "Traffic Fatality Rates and Beer Taxes in 1988",
    x = "Beer Tax (in 1988 dollars)",
    y = "Fatality Rate (fatalities per 10,000)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# The regression results show a positive relationship between beer tax and fatality rates for both years which is contrary to our expectations that alcohol taxes are supposed to lower the rate of traffic fatalities, with the 1988 coefficient almost three times larger than the 1982 coefficient.
# This is likely due to omitted variable bias, and using multiple regression or panel data could help control for factors that differ across states but remain constant over time.



#.....................................................................................
# Panel data with two time periods
# .............................................................................
# A regression based on the differenced data and plot the estimated regression function

# Compute the differences 
dif_fatal_rate <- fatal1988$fatal_rate - fatal1982$fatal_rate
dif_beertax <- fatal1988$beertax - fatal1982$beertax

# calculating minimum and maximum value
range(dif_fatal_rate)
range(dif_beertax)

# estimate a regression using differenced data
m_dif_fatal <- lm(dif_fatal_rate ~ dif_beertax)
coeftest(m_dif_fatal, vcov = vcovHC, type = "HC1")
# The estimated coefficient on beer tax is now negative and significantly different from zero at 5%.
# Raising the beer tax by 1 dillar causes traffic fatalities to decrease by 1.04 per 10000 people. 

# This is rather large as the average fatality rate is appoximately 2 persons per 10000 people.
mean(fatal$fatal_rate) # compute mean fatality rate over all states for all time periods

# scatter plot of the differenced data
ggplot(data = data.frame(dif_beertax, dif_fatal_rate), aes(x = dif_beertax, y = dif_fatal_rate)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "lightcoral", linewidth = 1) +
  labs(
    title = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
    x = "Change in Beer Tax (in 1988 dollars)",
    y = "Change in Fatality Rate (fatalities per 10,000)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



#.....................................................................................
# Fixed effects regression
# .............................................................................

# Application to traffic deaths
# ...........................................................
# The simple fixed effects model for estimation of the relation between traffic fatality rates and the beer taxes is 
m_fatal_fe <- lm(fatal_rate ~ beertax + state - 1, data = fatal) # -1 for removing the intercept term in the model. The model will estimate separate coefficients for each state, without a reference category (i.e., no baseline). 
summary(m_fatal_fe)
# So it is possible to estimate B1 by applying OLS to the demeaned data, that is, to run the regression (FatalityRate = B1BeerTaxit + Uit)

# Obtained demeaned data
fatal_demeaned <- with(fatal, data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                         beertax = beertax - ave(beertax, state)))
# The function ave is convenient for computing group averages. We use it to obtain states specific average of the fatal rate and the beer tax.
# estimate the regression
summary(lm(fatal_rate ~ beertax -1, data = fatal_demeaned))

# Alternative one may use from the package with the same name.
# Since the fixed effects estimator is also called the within estimator =, we set model = "within"

# estimate the fixed effects regression with plm()
fatal_fe_model <- plm(fatal_rate ~ beertax,
                      data = fatal,
                      index = c ("state", "year"),
                      model = "within")
# print summary using robust standard errors
coeftest(fatal_fe_model, vcov = vcovHC, type = "HC1")
# The estimated coefficient is again - 0.66. Note that plm() uses the entity demeaned OLS algorithm and thus does not report dummy coefficients.
# The estimated regression function is: FatalRate = -0.66 * Beertax + StateFixedEffects.
# The coefficient on BeerTax is negative and significant, suggesting that a $1 increase in the real beer tax reduces traffic fatalities by 0.66 per 10,000 people. 
# While state fixed effects control for biases from factors that differ between states but not over time, there may still be time-varying omitted variables causing bias.




#.....................................................................................
# Regression with time Fixed effects 
# .............................................................................
# To estimate the combined entity and time fixed effects model of the relation between fatalities and beer tax: 
#. FatalityRate_it = B1 Beertax_it + StateEffects + TimeFixedEffects + U_it

# By using both lm() and plm() we can estimate the above equation.
# .............................................

# via lm()
fatal_te_fe_lm <- lm(fatal_rate ~beertax +state + year - 1, data = fatal)
fatal_te_fe_lm

# via plm()
fatal_te_fe_plm <- plm(fatal_rate ~beertax,
                       data = fatal,
                       index = c ("state", "year"),
                       model = "within",
                       effect = "twoways")
coeftest(fatal_te_fe_plm, vcov = vcovHC, type = "HC1")

# Before discussing the outcomes we convince ourselves that state and year are of the class factor.
# check the calss of 'state' and 'year'
class(fatal$state)
class(fatal$year)

# The lm() function automatically turns categorical variables (factors) into dummy variables. Since we remove the intercept by adding -1 to the regression formula, 
# the lm() function estimates coefficients for a total of 54 dummy variables [n+T-1] (48 for states and 6 for years). However, the plm() function only displays the estimated coefficient for BeerTax.

# The estimated regression function is: Fatality Rate = -0.64 × BeerTax + State Effects + TimeFixedEffects
# The result of -0.66 is similar to the estimated coefficient from the regression model that includes only entity fixed effects. 
# As expected, the estimate is less precise but still significantly different from zero at the 10% level. This suggests that the relationship between traffic fatalities and the real beer tax is not influenced by omitted variable bias from factors that remain constant over time.




#.....................................................................................
# The Fixed effects Regression assumptions and standard errors for fixed effects regression
# .............................................................................

# Assumptions
#..............
# 1. The first assumption is that the error is uncorrelated with all observations of the variable X for the entity i over time. If this assumption is violated, we face omitted variables bias.
# 2. The Xit are allowed to be autocorrelated within entities. This is a common proporty of time series data. the same is allowed for errors Uit.
# 3. Large outliers are unlikely have nonzero finite fourth moments. Ols is sensitive to outliers, so we need to exclude them before estimating a model.
# 4. There is no perfect multicollinerity.


# Standard errors for fixed effects regression
#...............................................
# Standard errors in fixed effects models which differ from standard errors in multiple regression as the regression error can exhibit serial correlation in panel models.
# Similar as for Heteroskedasticity, autocorrelation invalidates the useal standard error formulas as well as heteroskedasticity-robust standard errors since these are derived under the assumption that there is no autocorrelation.
# To account for both heteroskedasticity and autocorrelation, "heteroskedasticity and autocorrelation consistent" (HAC) standard errors are needed. Clustered standard errors belong to these type of standard errors. 
# They allow for heteroskedasticity and autocorrelated errors within an entity but not correlation across entities. covHC() in coeftest() function recognizes panel model objects (objects of class plm) and computes clustered standard errors by default.



#...................................................
# Drunk driving Laws and Traffic Deaths
#.....................................................
#In the previous model we did not incorporate the variables like economic conditions and driving laws to predict the relationship between traffic fatalities and beer taxes.
# The covariates we can use from the dataset are:
str(fatal[, c("unemp", "income", "drinkage", "jail", "service", "miles")])

# categorizes the minimum legal drinking age into categories (bins)
fatal$drinkage_category <- cut(fatal$drinkage,  # The variable to be categorized
                               breaks = 18:22,  # Define breakpoints for age groups (18 to 22)
                               include.lowest = TRUE,  # Ensure 18 is included in the first bin
                               right = FALSE)  # Create left-closed, right-open intervals [a, b)

# Example of how the categorization works:
# If drinkage = 18   → Assigned to category [18,19)
# If drinkage = 18.8 → Assigned to category [18,19) (because 18 ≤ 18.8 < 19)
# If drinkage = 19   → Assigned to category [19,20)
# If drinkage = 20   → Assigned to category [20,21)
# If drinkage = 21   → Assigned to category [21,22)

# setting the (21,22) as the refrence or baseline level
fatal$drinkage_category <- relevel(fatal$drinkage_category, "[21,22]")

# Create a new categorical variable 'punish' in the 'fatal' dataset
fatal$punish <- with(fatal, factor(jail == "yes" | service == "yes",  # Check if either 'jail' or 'service' is "yes"
                            labels = c("no", "yes")))  # Label FALSE as "no" and TRUE as "yes"


# Keeping observations for only 1982 and 1988
fatal1982_1988 <- fatal[with(fatal, year ==1982 | year== 1988), ]
#alternative code of Keeping observations for only 1982 and 1988
#fatal1982_1988 <- fatal %>% 
  #filter(year ==1982 | year== 1988)


# Now estimating models
#.................................
model1 <- lm(fatal_rate ~ beertax, data = fatal)

model2 <- plm(fatal_rate ~ beertax + state, data = fatal)

model3 <- plm(fatal_rate ~ beertax + state + year, 
              index = c("state", "year"),
              model = "within",
              effects = "twoways",
              data = fatal)

model4 <- plm(fatal_rate ~ beertax + state + year + drinkage_category + punish + miles + unemp+ log(income), 
              index = c("state", "year"),
              model = "within",
              effects = "twoways",
              data = fatal)

model5 <- plm(fatal_rate ~ beertax + state + year + drinkage_category + punish + miles, 
              index = c("state", "year"),
              model = "within",
              effects = "twoways",
              data = fatal)

model6 <- plm(fatal_rate ~ beertax + year + drinkage + punish + miles + unemp + log(income), 
              index = c("state", "year"),
              model = "within",
              effects = "twoways",
              data = fatal)

model7 <- plm(fatal_rate ~ beertax + state + year + drinkage_category + punish + miles + unemp + log(income), 
              index = c("state", "year"),
              model = "within",
              effects = "twoways",
              data = fatal1982_1988)

library(stargazer)
# Gather clustered standard errors in a list
rob_se <- list(
  sqrt(diag(vcovHC(model1, type = "HC1"))),  # Robust SE for model1
  sqrt(diag(vcovHC(model2, type = "HC1"))),  # Robust SE for model2
  sqrt(diag(vcovHC(model3, type = "HC1"))),  # Robust SE for model3
  sqrt(diag(vcovHC(model4, type = "HC1"))),  # Robust SE for model4
  sqrt(diag(vcovHC(model5, type = "HC1"))),  # Robust SE for model5
  sqrt(diag(vcovHC(model6, type = "HC1"))),  # Robust SE for model6
  sqrt(diag(vcovHC(model7, type = "HC1")))   # Robust SE for model7
)

# generate the table
stargazer(model1, model2, model3, model4, model5, model6, model7,
          digits = 3,
          header = FALSE,
          type = "text",
          se = rob_se,
          title = "Linear Panel Regression Models of Traffic Fatalities",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))

# Model specifications (4) to (7) include covariates capturing state economic conditions and legal frameworks. Taking (4) as the baseline, we find:
# 1. Adding covariates does not significantly reduce the estimated beer tax effect, which remains statistically insignificant at the 5% level.
# 2. The minimum legal drinking age has no significant impact on traffic fatalities. None of the three dummies are statistically different from zero, and an F-test confirms this by failing to reject the joint hypothesis:
# Test if legal drinking age has no explanatory power
linearHypothesis(model4, 
                 test = "F", 
                 c("drinkage_category[18,19) = 0", 
                   "drinkage_category[19,20) = 0", 
                   "drinkage_category[20,21) = 0"), 
                 vcov = vcovHC, type = "HC1")

# 3. There is no strong evidence that punishing first-time offenders reduces drunk driving, as the corresponding coefficient is not statistically significant at the 10% level.
# 4. Economic factors play a significant role in explaining traffic fatalities. Both the employment rate and per capita income are jointly significant at the 0.1% level.
# Test if economic indicators (income and unemployment) have no explanatory power
linearHypothesis(model4, 
                 test = "F", 
                 c("log(income)", "unemp"), 
                 vcov = vcovHC, type = "HC1")


# Model (5) suggests that economic factors play an important role, as removing them alters the beer tax coefficient. 
# Model (6) indicates that the legal drinking age has little explanatory power, and changes in how it is modeled do not significantly affect the results. 
# Model (7) shows that using a smaller sample increases standard errors but does not drastically change the coefficient estimates.

# Conclusion
#.............
# Our findings suggest that stricter punishments and higher drinking ages do not significantly reduce traffic fatalities, while alcohol taxes may have a negative but imprecise effect. 
# If standard panel regression methods prove insufficient, instrumental variables regression can help address potential omitted variable bias.

## References
Hank, C., Arnold, M., Gerber, A., & Schmelzer, M. (Year). *Introduction to Econometrics with R*. Publisher.






















