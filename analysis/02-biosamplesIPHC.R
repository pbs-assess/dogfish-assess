library(dplyr)

d <- read.csv("data/raw/IPHC_dogfish_lengths2021.csv") %>%
  filter(reg_area == "2B", sex %in% c("F", "M")
  )
glimpse(d)

unique(d$reg_area)
unique(d$sex)
sort(unique(d$year))
range(na.omit(d$length)) #units are cm

#Exploratory figures
d %>% group_by(year, sex) %>%
  summarize(count_sex = n()) %>%
  ggplot() +
  geom_line(aes(year, count_sex, group = sex, col = sex)) +
  geom_point(aes(year, count_sex, group = sex, col = sex))

d %>%
  drop_na(length) %>%
  ggplot() +
  geom_jitter(aes(as.factor(year), length, group = sex, col = sex), alpha = 0.05) +
  geom_boxplot(aes(as.factor(year), length, col = sex)) +
  facet_wrap(~sex)
