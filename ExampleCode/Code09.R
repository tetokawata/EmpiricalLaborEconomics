
library(tidyverse)

Data = read_csv("SearchPanel.csv")

Kanto = filter(Data, Pref == "南関東")

Kanto = arrange(Kanto, Period)

Kanto = mutate(
  Kanto,
  LogTightnee = log(Vacancy/Seeker), # 対数求人倍率
  DifferenceLogTightnee = LogTightnee - lag(LogTightnee,1), # 労働者の余剰の変化
  Quarter = str_sub(Period, 6)
  )

ggplot(
  Kanto,
  aes(
    x = Year,
    y = DifferenceLogTightnee
  )) +
  geom_line() +
  facet_wrap(~Quarter)
