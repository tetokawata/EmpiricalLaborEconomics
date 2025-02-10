

library(tidyverse)

Data = read_csv("Hallow.csv")

Model = lm(log(Vac) ~ log(See), Data, subset = Year >= 2010)

Data$Optimal = -(1/Model$coefficients[2])*1 # (1/beta_1)*Social cost of seeker/vacancy (=1)

Data$Realized = Data$Vac/Data$See

ggplot(
  Data,
  aes(
    x = Year,
    y = Realized
  )
) +
  geom_line() +
  facet_wrap(~ Q) +
  geom_line(
    aes(
      y = Optimal,
      color = "Optimal"
    )
  )
