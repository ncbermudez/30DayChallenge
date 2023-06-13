### 30DayChartChallenge | May 2023 - Day 12 | them day: BBC News ###
# Data Source is data.gov.uk

# install packages
install.packages("png")
devtools::install_github("bbc/bbplot")

# load libraries
library(tidyverse)
library(sysfonts)
library(png)
library(bbplot)

# load data
setwd("C:/Users/ncber/OneDrive/Documentos/Practice in R/30daysChallenge/Day_12")
data2 <- readr::read_csv("historic_potholes.csv")

# load default font
font_add_google(name="Roboto", family="Roboto")
font <- "Roboto"

# wrangle and create df
df <- data2 %>%
  #filter(LOCALITY=="LEEDS") %>%
  group_by(LOCALITY) %>%
  summarise(n=n())

# create plot 
fig <- df %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 50, colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  bbc_style() +
  theme(plot.title = element_text(margin = margin(0, 0, 0, 0)),
        plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(size=10, margin = margin(0, 30, 0, 0))) +
  labs(title = "Historic potholes in Leeds",
       subtitle = "Distribution of the total number of potholes by locality \nrepaired between Jan 2018 and Sept 2020",
       caption = "#30DayChallenge, Day 12 | Design: Ryan Hart | Adapted: Neili Bermudez")

# save plot 
finalise_plot(plot_name = fig,
              source = "Source: data.gov.uk",
              save_filepath = paste0("day_12_", format(Sys.time(), "%d%m%Y"), ".png"),
              width_pixels = 640,
              height_pixels = 450)
