rm(list = ls())
library(tidyverse)

# 1. Läs in data
df_raw <- read.csv("post_faceoff_events_15s.csv", stringsAsFactors = FALSE)

# 2. Skapa Faceoff-ID (Den robusta versionen)
df_processed <- df_raw %>%
  mutate(
    is_new_faceoff = ifelse(eventname == "faceoff" & lag(eventname, default = "") != "faceoff", 1, 0),
    physical_faceoff_id = cumsum(is_new_faceoff)
  )

# 3. Registry - Identifiera roller och zoner
faceoff_registry <- df_processed %>%
  filter(eventname == "faceoff", xadjcoord > 0) %>%
  group_by(physical_faceoff_id) %>%
  summarise(
    gameid = first(gameid),
    compiledgametime = first(compiledgametime),
    start_x = first(xadjcoord),
    start_zon = case_when(
      start_x > 25  ~ "Zon-tekning (Anfall/Försvar)",
      start_x > 5   ~ "Neutral zon (vid blålinjen)",
      TRUE          ~ "Mitt-tekning (vid rödlinjen)"
    ),
    off_team = first(teamid),
    .groups = "drop"
  )

# 4. Analysera utfall
analysis_data <- df_processed %>%
  inner_join(faceoff_registry, by = "physical_faceoff_id") %>%
  mutate(my_role = ifelse(teamid == off_team, "Anfallande Lag", "Försvarande Lag")) %>%
  group_by(physical_faceoff_id, my_role, start_zon) %>%
  summarise(
    made_goal = any(eventname == "goal" & teamid == first(teamid)),
    made_shot = any(eventname == "shot" & teamid == first(teamid)),
    max_x = suppressWarnings(max(c(xadjcoord[teamid == first(teamid)], -100), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    utfall = case_when(
      made_goal  ~ "1. Gjort Mål",
      made_shot  ~ "2. Skott på mål",
      max_x > 25 ~ "3. Puck i Anfallszon",
      max_x > -5 ~ "4. Puck i Mittzon",
      TRUE       ~ "5. Fast i egen zon"
    )
  )

# --- AUDIT ---
all_ids <- unique(df_processed$physical_faceoff_id)
analyzed_ids <- unique(analysis_data$physical_faceoff_id)
missing_ids <- setdiff(all_ids, analyzed_ids)

cat("\n--- SLUTGILTIG VALIDERING ---\n")
cat("Totalt antal tekningar i analysen:", length(analyzed_ids), "\n")
cat("Antal exkluderade tekningar:", length(missing_ids), "\n")
cat("-----------------------------\n")

# 6. Slutgiltig rapport
final_report <- analysis_data %>%
  group_by(start_zon, my_role, utfall) %>%
  summarise(antal = n(), .groups = "drop") %>%
  group_by(start_zon, my_role) %>%
  mutate(procent = round(antal / sum(antal) * 100, 2)) %>%
  arrange(start_zon, my_role, utfall)

print(final_report, n = Inf)
