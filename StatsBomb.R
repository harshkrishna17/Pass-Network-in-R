# Libraries 

library(tidyverse)
library(StatsBombR)
library(ggsoccer)
library(scales)
library(ggrepel)
library(gt)
library(bstfun)
library(patchwork)
library(ggshakeR)
library(viridis)

# Data Pulling 

dataframe <- FreeCompetitions() %>%
  filter(competition_id == 2 & season_name == "2003/2004")
df <- FreeMatches(dataframe)
StatsBombData <- StatsBombFreeEvents(MatchesDF = df, Parallel = T)
data <- allclean(StatsBombData)

# Calculate xT

df <- data %>%
  rename("x" = "location.x",
         "y" = "location.y",
         "finalX" = "pass.end_location.x",
         "finalY" = "pass.end_location.y") %>%
  ggshakeR::calculate_threat(dataType = "statsbomb")

df <- df %>%
  mutate(xT = xTEnd - xTStart) %>%
  select(xT)
df[is.na(df)] <- 0

data$xT <- df$xT

# Data Wrangling 

data1 <- data %>%
  filter(match_id == 3749196) %>%
  filter(team.name == "Arsenal")

# Filter to before first substitution

min_events <- data1 %>% 
  group_by(player.id) %>% 
  summarise(period = min(period), timestamp = min(timestamp)) %>% 
  na.omit() %>% 
  arrange(period, timestamp)

if(nrow(min_events) > 11) {
  max_event <- min_events[12,]
  idx <- which(data1$period == max_event$period & data1$timestamp == max_event$timestamp) - 1
  data1 <- data1[1:idx,]
}

# Player Average Locations

nodes <- data1 %>% 
  filter(type.name %in% c("Pass", "Ball Receipt*", "Ball Recovery", "Shot", "Dispossessed", "Interception", "Clearance", "Dribble", "Shot", "Goal Keeper", "Miscontrol", "Error")) %>% 
  group_by(player.name) %>% 
  summarise(x = mean(location.x, na.rm=T), y = mean(location.y, na.rm=T), events = n(), xT = sum(xT)) %>% 
  na.omit()

# Edges

edgelist <- data1 %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
  select(from = player.name, to = pass.recipient.name) %>% 
  group_by(from, to) %>% 
  summarise(n = n()) %>% 
  na.omit()

edges <- left_join(edgelist, 
                   nodes %>% select(player.name, x, y),
                   by = c("from" = "player.name"))

edges <- left_join(edges, 
                   nodes %>% select(player.name, xend = x, yend = y),
                   by = c("to" = "player.name"))

edges <- edges %>% 
  group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
  summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])

# Rescaling edge sizes 

edges <- edges %>% 
  filter(n >= 3) %>%
  mutate(n = rescale(n, c(1, 8), c(3, 75)))

# Creating Line-up Table

nodes <- nodes %>%
  arrange(events)

nodes$id <- 1:nrow(nodes) 
nodes$player.name ## inspect names 

# Fix names

nodes$player.name[nodes$player.name == "Robert Pirès "] <- "Robert Pirès"
nodes$player.name <- sub(".* ", "", nodes$player.name)

table <- nodes %>%
  select(player.name, id) %>%
  rename(Player = player.name) %>%
  rename(Number = id) %>%
  spread(Player, Number) %>%
  gt() %>%
  tab_header(title = md("Line-up")) %>%
  bstfun::as_ggplot()

# Plot 

plot <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#101010", colour = "#454545") +
  theme_pitch() +
  geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n, alpha = n), colour = "white", show.legend = FALSE) +
  geom_point(data = nodes, aes(x, y, fill = xT), size = 10, shape = 21, stroke = 1, colour = "white") +
  scale_fill_viridis_c(option = "turbo") +
  geom_text(data = nodes, aes(x, y, label = id), colour = "white") +
  annotate(geom = "text", x = 110, y = 9.2, label = "Minimum 3 Pass Connections.\nWidth & Opacity of lines\ndenote number of passes.\nColour Scale = xT [Blue -> Red]", size = 2.6, colour = "white") +
  labs(title = "Arsenal Pass Network",
       subtitle = "Vs. Tottenham Hotspur | 2003/04 Premier League",
       caption = "Data from StatsBomb\nCreated by @placeholder2004") +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "#101010", fill = "#101010"),
        panel.background = element_rect(colour = "#101010", fill = "#101010"),
        plot.title = element_text(colour = "white", hjust = 0.5, size = 25, face = "bold"),
        plot.caption = element_text(colour = "white", size = 10),
        plot.subtitle = element_text(colour = "white", hjust = 0.5, size = 14)) +
  coord_flip() +
  direction_label(colour = "white")

# Add elements 

plot + table + plot_layout(nrow = 2)

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("StatsBombPassNetwork.png", bg = "white", width = 2500, height = 2400, units = "px")