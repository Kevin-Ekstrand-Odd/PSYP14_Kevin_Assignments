data_sample_1=read.csv("https://tinyurl.com/ha-dataset1")
install.packages("tidyverse")
install.packages("psych")
library(tidyverse)
library(psych)
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
library(car)
library(lmtest)
library(sandwich)
install.packages("lm.beta")
library(lm.beta)
install.packages("gridExtra")
library(gridExtra)

view(data_sample_1)
summary(data_sample_1)
describe(data_sample_1)

## As the data is explored regarding the relevant variables two outliers are found. ID_93 has the age 444, ID_150 shows a value for the STAI_trait of 3.9 and ID_109 shows a negative household_income of -3732. 
## These discrepancies can also be seen in their skew and kurtosis, the age variable shows 11.96 skew and 145.67 kurtosis and the STAI_trait shows -1.31 for skew and 8.38 for kurtosis. This can't be seen for the household income, however.
## The errors could be mistakes when they were put in the data, the age 444 could just be a 4 too many while the STAI_trait value of 3.9 looks like the "." was misplaced, as the assignment instuctions mentions that the scale ranges from 20 to 80.
## The negative household_income is not as easily interpreted as a different value, as it is both negative but also a 4-figure number, as all others are 5-figure numbers or above.
## As it is better to keep as much data as possible one could argue that the errors should be fixed and the new values kept. But as I am not familiar with the data collection I choose to exclude the datapoints instead and replace them with NA.
## (Source: Exercise_04)
##(page 154 about removing missing values or not, since it is so few it should not affect the dataset too much.)

data_sample_corrected = data_sample_1 %>%
  mutate(age = replace(age, age=="444", NA)) %>%
  mutate(STAI_trait = replace(STAI_trait, STAI_trait=="3.9", NA)) %>%
  mutate(household_income = replace (household_income, household_income=="-3732", NA))

data_sample_corrected_no_na = data_sample_corrected %>%
  drop_na()

view(data_sample_corrected_no_na)
summary(data_sample_corrected_no_na)
describe(data_sample_corrected_no_na)


model_1 = lm(pain ~ age + sex, data = data_sample_corrected_no_na)
model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_corrected_no_na)
summary(model_1)
summary(model_2)

### As the variables cortisol_serum and cortisol_saliva measure the same substance they should be highly correlated. To avoid running the other checks again I decide to deal with this expected multicollinearity first.
## As removing predictive variables changes the p-value and predictive power in the other variables.

vif(model_2)
data_sample_corrected_no_na %>%
  select(cortisol_serum, cortisol_saliva) %>%
  cor()

## The two variables are highly correlated at 0.89.
## The test shows that the vif is 6.57 for the cortisol_serum variable and 7.45 for the cortisol_saliva variable, which are both far above 3.
## This issue can be addressed in two ways that are relevant to this assignment. They could either be combined and use their mean as a combined cortisol variable, as they measure the same substance. Or one of the variables could be removed. 
## The background information mentions that the cortisol measured through blood is more reliable in the use to relate to stress. This motivates the choice of the second option: to remove one variable, namely cortisol_saliva. (Source: Exercise_11_Model_diagnostics.pds)
## model_2_serum is therefor introduced as the new model 2, without cortisol_saliva.

model_2_serum = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_corrected_no_na)
summary(model_2_serum)
model_2_serum
vif(model_2_serum)
coef_table(model_2_serum)

## The vif is now much better with all values below 3.

### The summary of the model shows that not all predictors add a significant predictive effect to the model, as STAI_trait and sex have non-significant p-values.
## STAI_trait will be removed from the model and the p-values will be checked again.

model_2_serum_2 = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum, data = data_sample_corrected_no_na)
summary(model_2_serum_2)
AIC(model_2_serum)
AIC(model_2_serum_2)
anova(model_2_serum, model_2_serum_2)
coef_table(model_2_serum_2)

## The AIC-scores are not significantly different and the anova does not show a significant difference (F (-1, 151)=0.64, p=0.424), but as the STAI_trait variable is so far from significant it is removed.
## Age is now significant (p=0.0267) while sex stayed non-significant (p=0.0808). The sex variable will also be removed.

model_2_serum_3 = lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = data_sample_corrected_no_na)
summary(model_2_serum_3)
AIC(model_2_serum_2)
AIC(model_2_serum_3)
anova(model_2_serum_2, model_2_serum_3)

## The AIC-scores are not significant different here as the difference is less than 2. The anova shows that there is no significant difference in the residual errors by removing sex as a predictor(F (-1, 151) = 2.75, p = 0.0993). 
## This leaves the choice to the theoretical background of the two models. The theory states that sex is a predictor for pain experienced that can be dependent on the type of the procedure.
## With that it mind it is possible that sex isn't as good as a predictor for pain experienced after wisdom tooth surgery as it is for other types of pain. 
## As it is better to keep the model with fewer predictors to avoid overfitting the variable should be removed, this is also supported by the variable not being significant.
## However the adjusted R^2 is lower for the model without sex (Adj. R^2=0.4682) compared to the model with the variable kept (Adj. R^2=0.4747). This difference is however very small so the above arguments are followed instead.
## Source (If both models seem plausible theoretically, we can retain the model containing less predictors) (ex 08)

## All included variables now show a significant p-value and should therefore add significant predictive power to the model.
## The multicolinearity check is run again.

vif(model_2_serum_3)

## No issues are showns as they all have value below 3.

### To check for outliers the variables are first visualized together with the outcome variable - pain.

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=age, label=ID) +
  geom_label()+
  geom_smooth(method="lm")

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=pain_cat, label=ID) +
  geom_label()+
  geom_smooth(method="lm")

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=mindfulness, label=ID) +
  geom_label()+
  geom_smooth(method="lm")

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=cortisol_serum, label=ID) +
  geom_label() +
  geom_smooth(method="lm")

### Visually no value seems to stick out to a high degree, but it will have to be explored through Cook's distance as well. ID_100 might be somewhat of an outlier for a few variables as it is the only case with a value of 10 on the pain scale. Pain_cat and Cortisol_serum shows that it is somewhat outside the expected value for the predictors.

model_2_serum_3 %>% plot(which = 5)
model_2_serum_3 %>% plot(which = 4)

## ID112 shows the highest value for Cook's distance, at almost 0.06. Checking the plots again shows that it sticks out with STAI-trait and pain_cat as predictors.
## If the criterion is to be used where cases with a higher Cook's distance than 4/N is to be used, then it shows that there are many outliers in this model. As 4/197=0.02 and multiple datapoints shows a Cook's distance much higher than that. 
## Case 67, 99 and 112 are pointed out as outliers in the plot which=4, they must be checked more closely for mistakes via the slice-function.

data_sample_corrected %>%
  slice(c(67, 99, 112))
summary(data_sample_corrected_no_na)

## Looking at the three cases none of them seem extreme or to be caused by mistakes, they have no extremes in any variable.
## All of these values are viabl as none of them seem to be mistakes as the age=444, STAI_trait=3.9 and household_income=-3732 were. None of them are close to the other rule of thumb regarding Cook's distance, that cases with a value over 1 would be considered problematic. (source: Exercise_11)

### Moving on to checking for violation of normality of the residuals, this can be visualized through a Q-Q plot and a histogram of the residuals.

model_2_serum_3 %>% plot(which=2)

residuals_model_2_serum_3 = enframe(residuals(model_2_serum_3))
residuals_model_2_serum_3 %>%
  ggplot() +
  aes(x=value) +
  geom_histogram()

describe(residuals(model_2_serum_3))

## The Q-Q plot shows that the residuals seem to follow the line fairly well. The histogram is difficult to interpret, it does seem normaly distributed but with a peak at around -1.5.
## The describe function for the residuals shows skewness with a value of -0.11 and kurtosis shows -0.48. These are within the range of -1 and 1 which implies that the residuals are normaly distributed. (Source: 11 video)

### To check the assumption of linear relationship the residualPlots() function is used.

model_2_serum_3 %>% residualPlots()

## Looking at the plots the mindfulness shows the most curviture, but to a very small degree. As none of the curvature tests shows significant results the assumptions holds true.

### Homoscedasticty is the last assumption to be checked which can be done visually with a fitted values plot, as well as through two tests: the Breusch-Pagan test and the ncvTest.

model_2_serum_3 %>% plot(which = 3)
model_2_serum_3 %>% bptest()
model_2_serum_3 %>% ncvTest()

## The plot does not show any unexpected results and both tests shows p-values of >0.05, which implies that the assumption has not been violated.

### Presenting the results of the two models.

## Using the custom code for coef_table from ZK lecture.

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

summary(model_2_serum_3)
confint(model_2_serum_3)
lm.beta(model_2_serum_3)
coef_table(model_2_serum_3)
AIC(model_2_serum_3)


summary(model_1)
confint(model_1)
lm.beta(model_1)
coef_table(model_1)
AIC(model_1)

anova(model_1, model_2_serum_3)




##### 
## Assignment 2

## The model is built on the corrected dataset from the first assignment.

model_back = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_corrected_no_na)
summary(model_back)

## As the same data is used the data check won't be needed. The discrepancy of the negative household_income datapoint has been caught and made NA.

### Model diagnostics.

### One cortisol variable has already been excluded so I do not have to start with multicolinearity as before, but to follow the same steps in the same order, I will.

vif(model_back)

## No vif score is higher than 3, the diagnostics can proceed.

### To check for outliers the new variables can be visually examined.

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=pain_cat, label=ID) +
  geom_label() +
  geom_smooth(method="lm")

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=weight, label=ID) +
  geom_label() +
  geom_smooth(method="lm")

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=IQ, label=ID) +
  geom_label() +
  geom_smooth(method="lm")

data_sample_corrected_no_na %>%
  ggplot() +
  aes(y=pain, x=household_income, label=ID) +
  geom_label() +
  geom_smooth(method="lm")

## ID_10 seems to stick out for weight on the extreme high at over 100. And ID_100 can be seen again for its high pain variable.
## Cook's distance need's to be checked.

model_back %>% plot(which = 5)
model_back %>% plot(which = 4)

## The cases 3, 102 and 112 are pointed out as possible outliers in the above tests, but they do not have much higher value for Cook's distance than the other cases. 3 and 112 are close to 0.055 and 0.070 respectively.
## They will be checked with the slice function.

data_sample_corrected %>%
  slice(c(3, 102, 112))
summary(data_sample_corrected_no_na)

## None of the cases seem to be made by mistakes or the like, the same issues that 112 had before remains. The cases do not need to be excluded.

### To check the normalcy of the residuals.

model_back %>% plot(which=2)

residuals_model_back = enframe(residuals(model_back))
residuals_model_back %>%
  ggplot() +
  aes(x=value) +
  geom_histogram()

describe(residuals(model_back))

## Both the Q-Q plot and histogram doesn't show any strange shapes, which is reflected in the skew and curtosis results which are both between -1 and 1 (Skew = -0.08, kurtosis=-0.42).

### The assumption of linearity is checked with the residualPlots function.

model_back %>% residualPlots()

## None of the curvature tests are significant and the plots show only small curves.

### Homoscedasticty 

model_back %>% plot(which = 3)
model_back %>% bptest()
model_back %>% ncvTest()

## Neither test are significant so the assumption holds. 

### Now to do backwards regression

backward_model = step(model_back, direction = "backward")

## The resulting model contains the variables sex, household_income, mindfulness, age, pain_cat and cortisol_serum as the predictive variables.

summary(backward_model)

## Now that several variables have been removed the assumption checks are run again.
## Multicolinearity

vif(backward_model)

## Outliers

backward_model %>% plot(which = 5)
backward_model %>% plot(which = 4)

## Normality of the residuals

backward_model %>% plot(which=2)

residuals_backward_model = enframe(residuals(backward_model))
residuals_backward_model %>%
  ggplot() +
  aes(x=value) +
  geom_histogram()

describe(residuals(backward_model))

## Homoscalearity

backward_model %>% plot(which = 3)
backward_model %>% bptest()
backward_model %>% ncvTest()

## All assumptions still hold true.

## Now to compare the two models using first AIC and then anova, as the two models are hierachical since the theory based model is nested within the backwards model. (source page 494)

theory_based_model = lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = data_sample_corrected_no_na)

AIC(theory_based_model)
AIC(backward_model)
AIC(model_back)
anova(theory_based_model, backward_model)
summary(theory_based_model)
summary(backward_model)

## The backward_model shows a lower AIC-score at 492.72 compared to the theory_based_model's 494.28, but it is not significant as it has to be at least 2 to confirm the difference between the two models.
## (Source for the 2 difference: Ex 08)
## The anova is not significant either (F(2, 150)=2.71, p=0.070), which also suggests that there is no significant difference between the models.
## The RSS is lower for the backwards_model, however, at 191.49 compared to the theory based model at 198.39.

### Now the new dataset that the models will be used on is loaded.

data_sample_2=read.csv("https://tinyurl.com/ha-dataset2")

## Data_sample_2 will first be checked for errors.

view(data_sample_2)
summary(data_sample_2)

## ID_8 shows a value for mindfulness of 7.17, which is outside the possible range between 1 and 6.

data_sample_2_corrected = data_sample_2 %>%
  mutate(mindfulness = replace(mindfulness, mindfulness=="7.17", NA))

data_sample_2_corrected_no_na = data_sample_2_corrected %>%
  drop_na()

view(data_sample_2_corrected_no_na)
summary(data_sample_2_corrected_no_na)

### Using the two models on the new dataset. (source:Exercise 09)

pred_theory_based <- predict(theory_based_model, data_sample_2_corrected_no_na)
pred_backward <- predict(backward_model, data_sample_2_corrected_no_na)
RSS_theory_based = sum((data_sample_2_corrected_no_na[, "pain"] - pred_theory_based)^2)
RSS_backward =sum((data_sample_2_corrected_no_na[, "pain"] - pred_backward)^2)

RSS_theory_based
RSS_backward

## The residual sum of squares shows a lower value for the theory based model (RSS=235.13) than for the backwards model (R=243.92), which suggests that the theory based model has less error than the backward model.
## Another way to see which model fits best is to test the correlation between pain in the second data set with the two predicted variables created by the two models.
## This can be done by implementing these variables in a dataset and running the tests from there.

data_with_model = cbind(data_sample_2_corrected_no_na, pred_theory_based)
data_with_models = cbind(data_with_model, pred_backward)

data_with_models %>%
  select(pain, pred_theory_based, pred_backward) %>%
  cor()

## Visual check with plotting pain vs the two predictions for pain

data_with_models %>%
  ggplot() +
  aes(y=pain, x=pred_theory_based) +
  geom_point() +
  geom_smooth()

data_with_models %>%
  ggplot() +
  aes(y=pain, x=pred_backward) +
  geom_point() +
  geom_smooth()

## The plots are almost identical, and so are the correlation coefficients as the predictive values from the theory based model has a correlation coefficient of 0.59 and the model based on backwards regression has a coefficient of 0.58.
## The predicted values correlate almost perfectly with one another with a coefficient at 0.98.
## These tests don't help in the choice of model to use.

##### What does help the choice is that 2 variables in the backwards model aren't significant.

## Coefficient table for the backward model

coef_table(backward_model)


