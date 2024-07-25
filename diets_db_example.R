library(dplyr)
library(odbc)
library(here)
library(tidyverse)

con <- dbConnect(odbc(), filedsn = here("dsn/amlr-pinniped-db-test.dsn"))

diets <- tbl(con, "vDiets") %>% collect()

diets.scat <- diets %>% 
  filter(sample_type %in% c("scat", "Scat"))

ggplot(diets, aes(x = season_name)) +
  geom_bar()

xy <- ggplot(diets, aes(x = season_name)) +
  geom_bar(fill = "brown") 

xy + scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle(label = "Scat Collections per Season") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(label = "Season Year") + ylab(label = "Number of Scats") +
  scale_y_continuous(limits = c(0, 140),
                     breaks = seq(0, 140, 20))
