# Libraries 

library(tidyverse)
library(StatsBombR)
library(ggsoccer)
library(gt)
library(bstfun)
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

# Minimum Number of Connections

edges <- edges %>% 
  filter(n >= 3)

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
  rename(No. = id) %>%
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
  scale_fill_viridis_c(option = "turbo") +
  geom_text(data = nodes, aes(x, y, label = id), colour = "white") +
  annotate(geom = "text", x = 5.5, y = 14, label = "Only 3+ Pass Connections.\nOpacity = Number of connections", size = 2.5, colour = "white") +
  labs(title = "Arsenal Pass Network",
       subtitle = "Vs. Tottenham Hotspur | 2003/04 Premier League",
       caption = "Data from StatsBomb\nCreated by @placeholder2004") +
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
  coord_flip(ylim = c(-25,100),
             xlim = c(0,120)) +
  annotation_custom(ggplotGrob(table), xmin=0, xmax=120, ymin=62, ymax=125) +
  direction_label(colour = "white", x_label = 107, y_label = 10)

# Save. You would need to crop the image manually afterwards.

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("StatsBombPassNetwork.png", bg = "#101010", width = 2500, height = 2000, units = "px")