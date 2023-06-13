### 30DayChartChallenge | May 2023 - Day 3| fauna/flora ###
## Day 3 - Data source is ncnhp.org

# install and load libraries
install.packages(c("ggimage", "ggraph", "ggtext"))
install.packages("psych")

library(magick)
library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)
library(igraph)
library(ggraph)
library(ggimage)
library(readr)
library(psych)
library(MetBrewer)

# add font
font_add_google(name = "Josefin Sans", family = "Josefin Sans")
font_t <- "Josefin Sans"

font_add_google(name = "Quicksand", family = "Quicksand")
font <- "Quicksand"

# turno on showtext
showtext_auto()
showtext_opts(dpi = 320)

# Set workind directory
setwd("C:/Users/ncber/OneDrive/Documentos/Practice in R/30daysChallenge/Day_3")

# load data
data <- readr::read_csv("nhp-species-search-results-3.csv")
#show_col_types = FALSE
#View(data)

# summary data
describe(data)

# wrangle and create df
df <- data%>%
  clean_names()%>%
  group_by(taxonomic_group, common_name)%>%
  count(nc_status)%>%
  ungroup()%>%
  # select(1,2,3)%>%
  # distint()%>%
  group_by(nc_status)

# create edge
edge_list1 <- df%>%
  select(taxonomic_group, nc_status)%>%
  unique%>%
  rename(from = taxonomic_group, to = nc_status)%>%
  mutate(color = to)

edge_list2 <- df%>%
  select(nc_status, common_name)%>%
  unique%>%
  rename(from = nc_status, to = common_name)%>%
  mutate(color=from)

edge_list = rbind(edge_list1, edge_list2)

# create vertices for color group
vertices <- data.frame(
  name = unique(c(as.character(edge_list$from), as.character(edge_list$to))),
  value = runif(57))

vertices$group = edge_list$from[match(vertices$name, edge_list$to, edge_list$color)]%>%
  replace_na("none")

vertices$group = edge_list$from[match(vertices$name, edge_list$to, edge_list$color)]%>%
  replace_na("none")

# create data frame for dendrogram
dendro <- graph_from_data_frame(edge_list, vertices = vertices)

# get image
butterfly <- magick::image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Butterfly_black.svg/350px-Butterfly_black.svg.png")%>%
  magick::image_trim()

butterfly_img <- magick::image_write(butterfly, path = "butterfly.img", format = "png")

image <- butterfly_img

# create plot
dendro%>%
  ggraph(layout = 'dendrogram', circular = TRUE)+
  geom_edge_diagonal2(color = "#BDBDBD")+
  geom_image(aes(x = 0, y = 0, image = image), size = 0.1)+
  geom_node_point(aes(filter = leaf, color = group))+
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, angle = -((-node_angle(x,y)+90)%%180)+90, label=name), hjust="outward", size=4)+
  annotate("text", x = -3, y=2.65, label = "Butterflies", family = font_t, size = 16, color = "#000000", hjust =0)+
  #annotate("richtext", x = -3, y=2.3, label="There are 56 butterfly species tracked by the<br>North Carolina Natural Heritage Program<br>with a state protection status of<br><span style='color: #66BB6A;'><b>Rare but Relatively Secure</span>,<br><span style='color: #42A5F5;'><b>Poorly Known in NC</b></span>,<br><span style='color: #AB47BC;'><b>Threat to Habitat</b></span>,<br>and <span style='color: #EF5350;'><b>Significantly<br>Rare</b></span>", family = font, size = 6, hjust=0, vjust="top", fill=NA, label.color=NA, lineheight=0.8, label.padding=unit(c(0, 0, 0, 0), "lines"),)+
  annotate("richtext", x = -3, y=2.3, label="There are 56 butterfly species tracked by the<br>North Carolina Natural Heritage Program<br>with a state protection status of<br><span style='color: #5b7314;'><b>Rare but Relatively Secure</span>,<br><span style='color: #454b87;'><b>Poorly Known in NC</b></span>,<br><span style='color: #87bcbd;'><b>Threat to Habitat</b></span>,<br>and <span style='color: #bd3106;'><b>Significantly<br>Rare</b></span>", family = font, size = 6, hjust=0, vjust="top", fill=NA, label.color=NA, lineheight=0.8, label.padding=unit(c(0, 0, 0, 0), "lines"),)+
    scale_x_continuous(limits = c(-3, 2))+
  scale_y_continuous(limits = c(-2,2.75))+
  #scale_color_manual(values = c("#EF5350", "#66BB6A", "#42A5F5", "#AB47BC"))+ 
  scale_color_manual(values = c("#bd3106", "#5b7314", "#454b87", "#87bcbd"))+
  #VanGogh1 = list(c("#2c2d54", "#434475", "#6b6ca3", "#969bc7", "#87bcbd", "#89ab7c", "#6f9954")
  #scale_color_manual(values=rev(met.brewer("VanGogh2", 4)))+
  coord_equal(clip = "off") +
  theme_void()+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 12, family = font, color = "#000000", hjust = 0),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF"))+
  labs(caption = "\n#30DayChallenge | Data: ncnhp.org | Design: Ryan Hart | Adapted: Neili Bermudez")

# save plot
ggsave(paste0("day_3_", format(Sys.time(), "%d%m%Y"), ".png"), dpi=320, width = 20, height = 15)

# angle of text labels
# https://stackoverflow.com/questions/43153004/how-to-read-a-text-label-in-ggraph-radial-graph

## Palette colors
#met.brewer(name="VanGogh1", n=7, type="discrete")
  
  