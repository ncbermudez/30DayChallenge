### 30DayChartChallenge | May 2023 - Day 5| Slope ###
# Data source is floridadep.gov 

# load libraries
library(tidyverse)
library(showtext)
library(janitor)
library(sysfonts)

# add font
font_add_google(name="Sigmar One", family="Sigmar One")
font_t<-"Sigmar One"

font_add_google(name="Hind", family="Hind")
font<-"Hind"

# load data
setwd("C:/Users/ncber/OneDrive/Documentos/Practice in R/30daysChallenge/Day_5")
data <- readr::read_csv("florida_managed_landfilled_v1.csv")
show_col_types = FALSE
view(data)

# wrangle and create df 
df <- data %>%  
  na.omit() %>%
  clean_names() %>% 
  select(!starts_with("total"))%>%
  arrange(pop_rank)%>%
  mutate(difference = landfilled_tons_2020 - landfilled_tons_2015) %>% 
  mutate(percent = round(difference / landfilled_tons_2015, 3)) %>% 
  pivot_longer(cols = starts_with("landfilled"), names_to = "year", values_to = "amount") %>% 
  mutate_at("year", str_replace, "landfilled_tons_", "") %>% 
  filter(year %in% c("2015", "2020")) %>% 
  filter(pop_rank <= 25) %>% 
  group_by(county, year)

df_down <- df %>%
  filter(percent<0)

df_up <- df %>%
  filter(percent >1.5)

ligthblue <- "#87bcbd"
black <- "#000000"

# create plot
df %>% 
  ggplot(aes(x = year, y = amount, group = county)) +
  geom_line(data = df %>% filter(percent < 1), color = "#E0E0E0") +
  geom_line(data = df %>% filter(percent > 1.5), color = black, linewidth = 1.1) +
  geom_line(data = df %>% filter(percent < 0), color = ligthblue, linewidth = 1.1) +
  geom_text(data = df_up, aes(x = year, y = amount, label = paste0(county, ": ", scales::comma(amount), " tons"),  hjust = if_else(year == "2015", 1.05, -0.05)), family = font, size = 5, color = black) +
  geom_text(data = df_down, aes(x = year, y = amount, label = paste0(county, ": ", scales::comma(amount), " tons"),  hjust = if_else(year == "2015", 1.05, -0.05)), family = font, size = 5, color = ligthblue) +
  geom_vline(xintercept = "2015", linewidth = 0.5, color = "#000000") + 
  geom_vline(xintercept = "2020", linewidth = 0.5, color = "#000000") + 
  geom_point(data = df_down, aes(x = year, y = amount), color = ligthblue) +
  geom_point(data = df_up, aes(x = year, y = amount), color = black) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 26, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 14, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 12, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(family = font, size = 12, hjust = 0.5, color = "#000000", vjust = -2),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Landfill Waste in Florida",
       subtitle = "Over these five years, Florida's top 25 most populous counties are piling up trash, except\nfor Osceola County, which reduced landfilling by 47.9%. On the other hand, two counties\nmanaged to increase landfill by more than 150%\n\n", 
       caption = "\n\n\n#30DayChartChallenge | Data: floridadep.gov | Design: Ryan Hart | Adapted: Neili Bermudez")
  
# save plot 
ggsave(paste0("day_5_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)


