# R code, resutls, conclusion, and explanation
# Exam 2 = 10% overall grade
#  graphics/figures are not required for uploading

library(oibiostat); data("famuss")

View(famuss)
#head(famuss, 5)
#dim(famuss)
str(famuss)
summary(famuss)

library(skimr)
skim(famuss)

library(DataExplorer)
DataExplorer::create_report(famuss)

library(oibiostat); data("famuss")

# Problem 1 - Inference
# a) Mean of age with 95% CI 
  # One-sample t-test; z* value for a 95% confidence interval is 1.96.
t.test(famuss$age, na.rm = TRUE, conf.level = 0.95)$conf.int  # 23.93359 (lower) 24.86977 (upper)
# Explanation: I'm 95% confident that the mean age in the famuss dataset is between (23.93, 24.87) years.

# b) Mean of age greater than 24? 
# Null Hypothesis: Mean of age equal to 24 (H0 : μ = 24)
# Alternative Hypothesis: Mean of age greater than 24 (HA : μ > 24)
# Significant level: α = 0.05
t.test(famuss$age, mu = 24, alternative = "greater") # Mean: 24.40168 
# Explanation: The pvalue is less than CI therefore we reject the null hypothesis. There is sufficient evidence at 95% CI to accept the alternative hypotheisis that mean age is greater than 24.  

# c) drm.ch is less than ndrm.ch? 
plot(famuss$ndrm.ch ~ famuss$drm.ch) # weak positive correlation
# Null Hypothesis: drm.ch equal to ndrm.ch 
# Alternative Hypothesis: drm.ch lower than ndrm.ch or ndrm.ch is greater than drm.ch
# Significant level: α = 0.05
t.test(famuss$ndrm.ch,famuss$drm.ch, alternative = 'greater', paired = TRUE) # Mean diff = 42.94084 
# Explanation: The pvalue is less than CI therefore we reject the null hypothesis. There is sufficient evidence at 95% CI to accept the alternative hypothesis that drm.ch lower than ndrm.ch in other words ndrm.ch is greater than drm.ch
# ** NOTE the t.test is answering the question but addressing ndrm.ch greater than drm.ch instead of other way around **

# d) BMI is higher in actn3.r577 x genotype 'CC' than 'TT'?
boxplot(famuss$bmi ~ famuss$actn3.r577x,
        ylab = "BMI", xlab = "Genotype at actn3.r577x")

summary(famuss[c("bmi", "actn3.r577x")]
# Expatiation: The BMI is greater in actn3.r577xgenotype CC than with TT.

# e) ndrm.ch is different across different race groups?
# Null Hypothesis: ndrm.ch is equal to race groups
# Alternative Hypothesis: At least one group is different 
boxplot(famuss$ndrm.ch ~ famuss$race,
        ylab = "ndrm.ch", xlab = "Race")
oneway.test(ndrm.ch ~ race, data = famuss, var.equal = TRUE)
# Explanation: Pvalue (0.032) is less than 0.05 showing confidence that there is a difference between the race groups and ndrm.ch


# f) Without adjusting P-value for multiple testing, which race group show different ndrm.ch? set p.adjust = 'none'
summary(aov(famuss$ndrm.ch ~ famuss$race))
pairwise.t.test(famuss$ndrm.ch, famuss$race,
                p.adj = "none")
# Explanation: The following race group shows significant relationship to ndrm.ch: Caucasian:Asian (0.014), Hispanic:Caucasian (0.031)

# g) With P-value adjusted using FDR, which race groups show different ndrm.ch? set p.adjust = 'fdr'
pairwise.t.test(famuss$ndrm.ch, famuss$race,
                p.adj = "fdr")
# Explanation: After FDR correction, all the pvalue is greater than 0.05 stating that there is no significant relationship.


# Problem 2 - Multiple linear regression
# a) build a model to predict ndrm.ch using all features
  # i) show modeling result using summary()
model.1 = lm(ndrm.ch ~ drm.ch +sex + age + race + height + weight + actn3.r577x + bmi, data = famuss)
summary(model.1) #adj.r.squared: 0.2526191

  # ii) which feature is significant with significance level of 0.01
# Explanation: Following features are significant (0.01) : drm.ch, sexMale age, actn3.r577x(CT,TT). And the r-squared value is 0.2526

  # iii) plot Q-Q plot for residues, and interpret results + model performace
# Test for linearity, constant variability, independent observation, normality of residuals
# R2 represents the proportion of variability in the response variable explained by the model
qqnorm(resid(model.1), pch = 21,main = "Q-Q Plot of Model Residuals")
qqline(resid(model.1))
# Explanation: Besides skewness at the top and few outlines(extreme values) at the end, the data is normally distributed.

# b) rebuild the model with only significant variables
  # i) show results using summary()
model.2 = lm(ndrm.ch ~ drm.ch +sex +age+ actn3.r577x, data = famuss)
summary(model.2) #adj.r.squared: 0.2473 

  # ii) significant features with significant level of 0.1
# Explanation: After keeping on significant features, the r-squared value now is 0.2473  which is lower than before, stating the model performance went down.
qqnorm(resid(model.2), pch = 21,main = "Q-Q Plot of Model Residuals")
qqline(resid(model.2))
# Explanation: After using the selected features, the data is now closer to the line and shows its linearity and normal distribution. However, the apparent skewness with few outlines (extreme values) is still maintained and seek for more normalization processing steps.
# The model performance however is not getting better as r.adj value is lower in model.2 than model.1


# c) rebuild the model with only significant variables and interaction between sex and actn3.r577x
  # i) show results using summary()
model.3=lm(ndrm.ch ~ drm.ch+age+sex*actn3.r577x, data=famuss)
summary(model.3) #adj.r.squared: 0.2524646
  # ii) significant features with significance level of 0.1
# Following features are significant: drm.ch, age, sexMale, actn3.r577x(CT,TT), sexMale:actn3.r577x(CT,TT)

  # iii) interaction item improved model performance? Compare using adjusted R-square to evaluate the model performance
qqnorm(resid(model.3), pch = 21,main = "Q-Q Plot of Model Residuals")
qqline(resid(model.3))
# Explanation: The model performance based on adj.r-squared (0.2524646) is much better compare to model.2 (0.2473 ) and approx. same as model.1 (0.2526191).

