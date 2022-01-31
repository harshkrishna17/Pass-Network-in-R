# Libraries 

library(tidyverse)
library(ggsoccer)
library(gt)
library(bstfun)
library(ggshakeR)
library(useful)
library(viridis)

# Data Pulling 

setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents")
data <- read.csv("ELFinal.csv")

# Calculate xT

df <- data %>%
  rename("x" = "x",
         "y" = "y",
         "finalX" = "endX",
         "finalY" = "endY") %>%
  ggshakeR::calculate_threat(dataType = "opta")

df <- df %>%
  mutate(xT = xTEnd - xTStart) %>%
  select(xT)
df[is.na(df)] <- 0

data$xT <- df$xT

# Data Wrangling 

data1 <- data %>%
  mutate(x = x * 1.2) %>%
  mutate(y = y * 0.8) %>%
  mutate(endX = endX * 1.2) %>%
  mutate(endY = endY * 0.8) %>%
  filter(teamId == "Manchester United")

data1 <- data1[complete.cases(data1[ , "playerId"]), ]

data1 <- shift.column(data = data1, columns = "playerId", newNames = "receiver", len = 1, up = TRUE)

# Filter to before first substitution

min_events <- data1 %>% 
  filter(type == "SubstitutionOff") %>%
  arrange(minute)

min <- min(min_events$minute)

data1 <- data1 %>%
  filter(minute < min)

# Player Average Locations

nodes <- data1 %>% 
  group_by(playerId) %>% 
  summarise(x = mean(x, na.rm=T), y = mean(y, na.rm=T), events = n(), xT = sum(xT)) %>% 
  na.omit()

# Edges

edgelist <- data1 %>% 
  filter(type == "Pass" & outcome == "Successful") %>% 
  select(from = playerId, to = receiver) %>% 
  group_by(from, to) %>% 
  summarise(n = n()) %>% 
  na.omit()

edges <- left_join(edgelist, 
                   nodes %>% select(playerId, x, y),
                   by = c("from" = "playerId"))

edges <- left_join(edges, 
                   nodes %>% select(playerId, xend = x, yend = y),
                   by = c("to" = "playerId"))

edges <- edges %>% 
  group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
  summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])

# Minimum Number of Connections

edges <- edges %>% 
  filter(n >= 3)

# Creating Line-up Table

nodes <- nodes %>%
  mutate(Player = case_when(playerId == 1 ~ "De Gea",
                            playerId == 7 ~ "Cavani",
                            playerId == 11 ~ "Greenwood",
                            playerId == 18 ~ "Fernandes",
                            playerId == 10 ~ "Rashford",
                            playerId == 29 ~ "Wan-Bissaka",
                            playerId == 39 ~ "McTominay",
                            playerId == 3 ~ "Bailly",
                            playerId == 6 ~ "Pogba",
                            playerId == 2 ~ "Lindelof",
                            playerId == 23 ~ "Shaw"))

table <- nodes %>%
  select(Player, playerId) %>%
  arrange(playerId) %>%
  rename(No. = playerId) %>%
  gt() %>%
  tab_header(title = md("**Line-up**")) %>%
  tab_options(table.background.color = "#101010") %>%
  bstfun::as_ggplot()

# Plot 

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#101010", colour = "#454545") +
  theme_pitch() +
  geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, alpha = n), colour = "white", show.legend = FALSE, size = 2.5, arrow = arrow(length = unit(0.45, "cm"))) +
  geom_point(data = nodes, aes(x, y, fill = xT), size = 9, shape = 21, stroke = 1, colour = "white") +
  scale_fill_viridis_c(option = "mako") +
  geom_text(data = nodes, aes(x, y, label = playerId), colour = "white") +
  annotate(geom = "text", x = 5.5, y = 14, label = "Only 3+ Pass Connections.\nOpacity = Number of connections", size = 2.5, colour = "white") +
  labs(title = "Man Utd Pass Network",
       subtitle = "Vs. Villarreal CF | 2020/21 Europa League Final",
       caption = "Created by @placeholder2004") +
  theme(legend.position = c(0.88, 1.06),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#101010"),
        legend.title = element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        plot.background = element_rect(colour = "#101010", fill = "#101010"),
        panel.background = element_rect(colour = "#101010", fill = "#101010"),
        plot.title = element_text(colour = "white", hjust = 0.5, size = 25, face = "bold"),
        plot.caption = element_text(colour = "white", size = 10),
        plot.subtitle = element_text(colour = "white", hjust = 0.5, size = 14)) +
  coord_flip(ylim = c(100,-25),
             xlim = c(0,120)) +
  annotation_custom(ggplotGrob(table), xmin=0, xmax=120, ymin=125, ymax=-155) +
  direction_label(colour = "white", x_label = 107, y_label = 10)

# Save. You would need to crop the image manually afterwards. 

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("OptaPassNetwork.png", bg = "#101010", width = 2500, height = 2000, units = "px")