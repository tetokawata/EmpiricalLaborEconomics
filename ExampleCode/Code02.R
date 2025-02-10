
library(tidyverse)
library(estimatr)
library(lmw)

data("GSS7402", package = "AER")

lm_robust(education ~ ethnicity, GSS7402)

lm_robust(education ~ ethnicity + age + year + immigrant, GSS7402) # 平均のみバランス

lm_robust(education ~ ethnicity + (age + year + immigrant)**2 + 
            I(age^2) + I(year^2), 
          GSS7402) # 平均 + 分散 + 共分散もバランス

Weight = lmw(education ~ ethnicity + (age + year + immigrant)**2 + 
      I(age^2) + I(year^2), 
    GSS7402)

plot(summary(Weight), abs = FALSE)

GSS7402$Weight = Weight$weights

# Ctr + A -> Ctr + Enter
