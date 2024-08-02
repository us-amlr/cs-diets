library(dplyr)
library(odbc)
library(here)
library(tidyverse)

con <- dbConnect(odbc(), filedsn = here("dsn/amlr-pinniped-db-test.dsn"))

diets <- tbl(con, "vDiets") %>% collect()

diets.scat <- diets %>% 
  filter(sample_type %in% c("scat", "Scat"))

scat.diet.plot <- diets.scat %>%
  pivot_longer(cols = krill_type:squid_type,
               names_to = "Species",
               values_to = "Presence") %>%
  group_by(season_name, Species) %>%
  summarize(Presence_in_Collection = sum(Presence == "Yes")) #%>%
  #mutate(prey.prop = Species/)
#Add a proportion column on here
?pivot_longer


#Do a proportion plot for seasons instead to visualize better
ggplot(scat.diet.plot, aes(fill = Species, x = season_name, y = Presence_in_Collection)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
 # scale_fill_manual(values = c("bisque4", "indianred3", "lavenderblush")) +
  xlab(label = "Season Year") + ggtitle(label = "Prey Species Presence per Season") +
  scale_fill_discrete(name = "Prey Type", labels = c("Fish", "Krill", "Squid")) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 8)) + scale_x_discrete(guide = guide_axis(angle = 90))

# p2 <- ggplot(scat.diet.plot, aes(x=season_name, y=`Number of Scats`)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~Species)
# p2



## Scat count per Season
ggplot(diets.scat, aes(x = season_name)) +
  geom_bar(stat = "identity")

xy <- ggplot(diets.scat, aes(x = season_name)) +
  geom_bar(fill = "coral4") 

xy + scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle(label = "Scat Collections per Season") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(label = "Season Year") + ylab(label = "Number of Scats") +
  scale_y_continuous(limits = c(0, 140),
                     breaks = seq(0, 140, 20))
