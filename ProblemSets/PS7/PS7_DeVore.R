# Problem 3
library("Amelia")
library("mice")
library("modelsummary")
library("tibble")
library("tidyverse")


# Problem 4
wages <- read.csv("wages.csv")
wages$college <- as.factor(wages$college)
wages$married <- as.factor(wages$married)



# Problem 5
wages <- wages %>% drop_na(hgc)
wages <- wages %>% drop_na(tenure)


# Problem 6
md.pattern(wages)
datasummary_skim(wages, output = "PS7_Table1.tex")



# Problem 7
# Complete Cases (Listwise Deletion)
wagescc <- wages %>% drop_na(logwage)
sccreg <- lm(logwage ~ hgc + college + tenure + (tenure * tenure) + age + married, wagescc)


# Mean Imputation
wagesmi <- mice(data = wages, method = "mean")
completewagesmi <- complete(wagesmi, 1)
mireg <- lm(logwage ~ hgc + college + tenure + (tenure * tenure) + age + married, completewagesmi)


# Predicted Values from Complete Cases
wagesp <- wages
wagesp[is.na(wagesp)] <- predict(sccreg)[is.na(wagesp)]
preg <- lm(logwage ~ hgc + college + tenure + (tenure * tenure) + age + married, completewagesp)


# Multiple Implementation Regression Model
wagesmce <- mice(wages, m = 5, printFlag = FALSE)
regmce <- with(wagesmce, lm(logwage ~ hgc + college + tenure + (tenure * tenure) + age + married))
regmce <- mice::pool(regmce)


model <- list("Listwise Deletion" = sccreg, "Mean Imputation" = mireg, "Predicted Values" = preg, "Multiple Implementation" = regmce)
modelsummary(model, out = "NA_Regressions.tex")
