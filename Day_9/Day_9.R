### 30DayChartChallenge | May 2023 - Day 9| high/low ###
# Data source is www.midiworld.com

# install packages
install.packages("ggridges")
install.packages("tuneR")
install.packages("devtools")

# load libraries 
library(psych)
library(tidyverse)
library(tuneR)
library(showtext)
library(ggridges)
library(sysfonts)
library(janitor)
devtools::install_github("r-lib/conflicted")

# add fonts
font_add_google(name = "Anton", family = "Anton")
font_add_google(name = "Quicksand", family = "Quicksand")

font_t <- "Anton"
font <- "Quicksand"

# load data
setwd("C:/Users/ncber/OneDrive/Documentos/Practice in R/30daysChallenge/Day_9")
data <- readMidi("Led_Zeppelin_-_Black_Dog.mid")
data_notes <- getMidiNotes(data)
describe(data_notes)

# create data frame
data_df <- data %>%
  filter(!is.na(parameterMetaSystem)) %>%
  filter(parameterMetaSystem %in% c("Guitar 1", "Guitar 2", "Bass", "Drums", "Vocal")) %>%
  left_join(data_notes, by = "track") %>%
  select(parameterMetaSystem, note, notename, length)

# set y-axis order
#data_df$parameterMetaSystem <- factor(data_df$parameterMetaSystem, levels = c("Vocal", "Guitar 1", "Guitar 2", "Bass", "Drums"))

# create plot
data_df %>%
  ggplot(aes(x = note, y = parameterMetaSystem)) +
  geom_density_ridges(fill="#E0E0E0", scale=3, alpha=0.8, color="#FFFFFF", size=0.6) +
  annotate("text", x=40, y=8, label="Black Dog", family=font_t, fontface="bold", size=14, color="#FFFFFF", hjust=0.5) +
  annotate("text", x=40, y=7.45, label="Led Zeppelin IV", family=font, size=8, color="#FFFFFF", hjust=0.5)+
  annotate("text", x=40, y=7, label="1971", family=font, size=8, color="#FFFFFF", hjust=0.5) +
  scale_y_discrete(expand=c(0, 0.25)) +
  scale_x_continuous(expand=c(0,0), breaks=c(28, 88), labels=c("Low", "High")) +
  coord_cartesian(clip="off") +
  theme_void() +
  theme(plot.caption = element_text(family=font_t, size=12, color="#FFFFFF", hjust=0.5),
        plot.caption.position = "plot",
        axis.line.x = element_line(),
        axis.title.x = element_text(size=10, family=font_t, color="#FFFFFF"),
        axis.text.y = element_text(size = 10, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(r = 5, b = 5)),
        axis.text.x = element_text(size = 10, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 5)),
        legend.position = "Null",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = "#134b73", fill = "#134b73")) +
  labs(x = "\nMusical notes from song arranged low to high by midi track",
       caption = "\n\n\n#30DayChartChallenge - Day 9 | Data: midiworld.com | Design: Ryan Hart | Adapted: Neili Bermudez")

# save plot 
ggsave(paste0("day_5_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)

  