library(dplyr)
library(odbc)
library(here)
library(tidyverse)

con <- dbConnect(odbc(), filedsn = here("dsn/amlr-pinniped-db-test.dsn"))

diets <- tbl(con, "vDiets") %>% collect()

diets.scat <- diets %>% 
  filter(sample_type %in% c("scat", "Scat")) %>% 
  filter(!(sex %in% "M"))


?pivot_longer


diets.freq <- diets.scat %>%
  pivot_longer(
    cols = krill_type:squid_type,
    names_to = "Species",
    values_to = "Presence"
  ) %>%
  group_by(season_name) %>%
  mutate(n_scats = n_distinct(sample_num)) %>% 
  group_by(season_name, n_scats, Species) %>%
  summarize(Presence_in_Collection = sum(Presence == "Yes")) %>%
  group_by(season_name) %>%
  mutate(prop.prey = Presence_in_Collection / sum(Presence_in_Collection), 
         freq_prey = Presence_in_Collection / n_scats) %>%
  ungroup() %>%
  mutate(Species = stringi::stri_trans_totitle(gsub('_type', '', Species))) 

# diets.prop %>% ggplot(aes(season_name, prop.prey)) +
#   geom_bar(aes(fill = Species), stat = 'identity', position = 'stack') +
#   scale_fill_manual(
#     name = "Prey Type", 
#     values = c(Krill = "indianred3", Fish = "bisque4", Squid = "thistle")
#   ) +
#   scale_x_discrete(guide = guide_axis(angle = 90)) +
#   labs(x = "Season Year", y = "Proportion", title = "Proportion of Prey Species in Scat") +
#   theme(
#     plot.title = element_text(hjust = 0.5), 
#     axis.title.y = element_text(size = 9),
#     axis.title.x = element_text(size = 9)
#   ) 
diets.freq %>% 
  ggplot(aes(season_name, freq_prey)) +
  geom_bar(aes(fill = Species), stat = 'identity', position = 'dodge') + theme_bw() +
  scale_fill_manual(
    name = "Prey Type", 
    values = c(Krill = "indianred4", Fish = "bisque4", Squid = "peru")
  ) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Season Year", y = "Frequency", title = "Frequency of Prey Species in Scat") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9)
  ) 


# p2 <- ggplot(scat.diet.plot, aes(x=season_name, y=`Number of Scats`)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~Species)
# p2


## Scat count per Season
# ggplot(diets.scat, aes(x = season_name)) +
#   geom_bar(stat = "identity")
# 
# library(dplyr)
# library(odbc)
# library(here)
# library(tidyverse)

# diets.scat <- diets %>% 
#   filter(sample_type %in% c("scat", "Scat"))

xy <- ggplot(diets.scat, aes(x = season_name)) +
  geom_bar(fill = "tomato4") 

xy + scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle(label = "Scat Collections per Season") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(label = "Season Year") + ylab(label = "Number of Scats") +
  scale_y_continuous(limits = c(0, 140),
                     breaks = seq(0, 140, 20))


#downloading as csv
# write.csv(diets.scat, "diets.scat.csv")
# getwd()
# ?write.csv
