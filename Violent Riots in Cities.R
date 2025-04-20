# Comparison: Violent Riots in cities vs non-cities
# Load required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(forcats)
library(stringr)
library(patchwork)

# Load data
events <- read_excel("events.xlsx")
cities <- read_excel("cities.xlsx")

# Merge datasets using CITY_ID
merged_data <- events %>%
  left_join(select(cities, CITY_ID, CAPITAL), by = "CITY_ID")

# Capital vs Non-Capital Riot Counts
riot_data <- merged_data %>%
  filter(PTYPE == 50)

riot_counts <- riot_data %>%
  count(CAPITAL, name = "Number_of_Riots")

# Plot 1
plot1 <- ggplot(riot_counts, aes(x = CAPITAL, y = Number_of_Riots, fill = CAPITAL)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Organized Violent Riots by Capital Status",
       x = "Capital City",
       y = "Number of Riots") +
  scale_fill_manual(values = c("Yes" = "#00088B", "No" = "#00BFFF")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 9)
)

# Chi-squared test
# Create a contingency table: capital vs. riot event
merged_data <- merged_data %>%
  mutate(riot_event = ifelse(PTYPE == 50, "Riot", "Other"))

table_data <- table(merged_data$CAPITAL, merged_data$riot_event)
chi_result <- chisq.test(table_data)

print(chi_result)

# Top Actors in Capital City Riots
capital_riots <- merged_data %>%
  filter(PTYPE == 50, CAPITAL == "Yes")

actor_clean <- capital_riots %>%
  select(ACTOR1, ACTOR2, ACTOR3) %>%
  pivot_longer(cols = everything(), names_to = "actor_type", values_to = "actor") %>%
  filter(!is.na(actor), actor != "99") %>%
  mutate(actor = str_to_title(actor),
         actor = case_when(
           str_detect(actor, "Student") ~ "Students",
           str_detect(actor, "Demonstrator") ~ "Demonstrators",
           str_detect(actor, "Protester") ~ "Protesters",
           str_detect(actor, "Police|Security Force") ~ "Security Forces",
           TRUE ~ actor
         )) %>%
  count(actor, sort = TRUE)

# Defining top actors in riots
top_actor_names <- c("Students", "Demonstrators", "Security Forces", "Protesters")

# Using custom colors for different actors
custom_colors <- c(
  "Students" = "#27408B",
  "Demonstrators" = "#1874CD",
  "Security Forces" = "#00BFFF",
  "Protesters" = "#0000FF"
)

# Filter and reorder only top selected actors
top_cleaned <- actor_clean %>%
  filter(actor %in% top_actor_names) %>%
  mutate(actor = fct_reorder(actor, n))

# Plot2
plot2 <- ggplot(top_cleaned, aes(x = actor, y = n, fill = actor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Top Actors in Capital City Riots",
    x = "Actor",
    y = "Number of Events"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 9)
  )

# Combining two plots
plot1 + plot2
