
library(tidyverse)

Data = read_csv("SearchPanel.csv")

Kinki = filter(Data, Pref == "近畿")

Kinki = mutate(Kinki,
               Log_Tightness = log(Vacancy/Seeker),
               Log_Job_Find = log(Hir/Seeker),
               After = if_else(Year >= 2020,1,0)
               )

lm(Log_Job_Find ~ Log_Tightness + After, Kinki) # Time series

Model = lm(Log_Job_Find ~ Log_Tightness + Pref + factor(Year), Data) # Panel Data

Data$Gamma = Model$coefficients[2]

Test = mutate(Data, Log_Tightness = 0)

Data$A = exp(predict(Model, Test))

Data = mutate(Data, Index = (A^(1/Gamma))*Vacancy)

Data = mutate(Data, 
              U = sum(Seeker),
              Weight = Index/sum(Index),
              .by = Period)


Data$Optimal = Data$U*Data$Weight

Data$Gap = Data$Optimal - Data$Seeker

ggplot(
  filter(Data, Period == "2024 Q3"),
  aes(
    x = Seeker,
    y = Optimal,
    label = Pref
  )
  ) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point() +
  ggrepel::geom_text_repel()

Data$SimHir = Data$A*(Data$Vacancy^(Data$Gamma))*(Data$Seeker^(1 - Data$Gamma))

Data$OprimalHir = Data$A*(Data$Vacancy^(Data$Gamma))*(Data$Optimal^(1 - Data$Gamma))

ggplot(
  filter(Data, Period == "2024 Q3"),
  aes(
    x = SimHir,
    y = OprimalHir,
    label = Pref
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point() +
  ggrepel::geom_text_repel()



# ctr + A -> ctr + Enter
# 
