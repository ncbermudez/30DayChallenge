### 30DayChartChallenge | May 2023 - Day 6| OWID ###
# Data source is ourworldindata.org/water-access#access-to-safe-drinking-water

# load libraries
library(tidyverse)
library(showtext)
library(janitor)
library(MetBrewer)
library(sysfonts)
library(psych)

# add font
font_add_google(name="Julius Sans One", family = "Julius Sans One")
font_add_google(name="Nunito Sans", family = "Nunito Sans")
font_t <- "Julius Sans One"
font <- "Nunito Sans"

# turn on showtext
showtext_auto()
showtext_opts(dpi=320)

# load data
setwd("C:/Users/ncber/OneDrive/Documentos/Practice in R/30daysChallenge/Day_6")
data <-readr::read_csv("water-and-sanitation.csv")
describe(data)

# wrangle and creat df
df <- data %>%
  select(92, Entity, 1, 4, 5) %>% 
  na.omit() %>% 
  filter(Entity %in% c("High income", "Upper-middle income", "Lower-middle income", "Low income")) %>% 
  clean_names() %>%
  mutate(entity = str_to_title(entity)) %>% 
  gather(!c(year, entity), key = access, value = percent) %>% 
  mutate(access = case_when(access == "access_to_improved_drinking_water" ~ "Improved",
                            access == "access_to_unimproved_drinking_water" ~ "Unimproved",
                            access == "no_access_to_drinking_water" ~ "No Access (Surface Water)")) %>% 
  mutate(access = fct_relevel(access, "Improved", "Unimproved", "No Access (Surface Water)"))

# create plot 
df %>% 
  ggplot(aes(x = year, y = percent, fill = access)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(met.brewer("Cassatt2", 3))) +
  facet_wrap(~ factor(entity, c("High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income"))) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 10, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 4, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 5, family = font, color = "#000000", hjust = 0.5),
        strip.text = element_text(size = 5, family = font, color = "#000000", hjust = 0.5, margin = margin(b = 2.5)),
        legend.position = "top",
        legend.margin = margin(b = 10),
        legend.text = element_text(size = 5, family = font, color = "#000000", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(size = 5, family = font, color = "#000000"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Drinking Water",
       subtitle = "Global access to improved drinking water has increased significantly over this decade, \n\n\nwith all income groups benefiting. However, by the end of the decade, approximately 117 million people still rely on surface water \n\n\nas their primary source of drinking water, most of whom are from lower-middle and low-income countries",
       caption = "\n\n\n#30DayChartChallenge - Day 6| Data: ourworldindata.org | Design: Ryan Hart | Adapted: Neili Bermudez")

# save plot 
ggsave(paste0("day_6_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)

