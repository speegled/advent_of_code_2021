dd <- read.csv(here::here("data/day_01"), header = F)
sum(diff(dd$V1) > 0)

sum(diff(filter(dd$V1, filter = c(1,1,1))) > 0, na.rm = T)
