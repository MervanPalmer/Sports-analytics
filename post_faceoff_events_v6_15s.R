rm(list = ls())
library(tidyverse)

# 1. Läs in data
df_raw <- read.csv("post_faceoff_events_15s.csv", stringsAsFactors = FALSE)

# 2. Skapa ett unikt ID för VARJE FYSISK TEKNING (True_FO_Pair)
# Vi kopplar ihop rader som har samma match och där faceoff_time är identisk.
df_processed <- df_raw %>%
  arrange(gameid, compiledgametime) %>%
  mutate(
    # En ny fysisk tekning sker när eventname är faceoff OCH tiden ändras från föregående rad
    is_new_physical_fo = ifelse(eventname == "faceoff" & (compiledgametime != lag(compiledgametime, default = 0)), 1, 0),
    true_fo_pair_id = cumsum(is_new_physical_fo)
  ) %>%
  group_by(gameid) %>%
  fill(true_fo_pair_id, .direction = "downup") %>%
  ungroup()

# 3. Registry - Här definierar vi rollen för VARJE lag i VARJE par
# Vi antar inte ordningsföljd, utan kollar bara på lagets egna xadjcoord
faceoff_registry <- df_processed %>%
  filter(eventname == "faceoff") %>%
  mutate(
    my_zon = case_when(
      xadjcoord > 25   ~ "1. Offensiv Zon",
      xadjcoord > 5    ~ "2. Neutral Zon (Off)",
      xadjcoord > -5   ~ "3. Mitt-tekning",
      xadjcoord > -25  ~ "4. Neutral Zon (Def)",
      TRUE             ~ "5. Defensiv Zon"
    ),
    # Rollen bestäms av zonen, inte av radnummer
    my_role = case_when(
      my_zon %in% c("1. Offensiv Zon", "2. Neutral Zon (Off)") ~ "Anfallande Lag",
      my_zon %in% c("4. Neutral Zon (Def)", "5. Defensiv Zon") ~ "Försvarande Lag",
      # Vid mitt-tekning (Zon 3) använder vi outcome för att se vem som vann initiativet
      TRUE ~ ifelse(outcome == "successful", "Anfallande Lag", "Försvarande Lag")
    )
  ) %>%
  select(gameid, true_fo_pair_id, teamid, my_zon, my_role)

# 4. Analysera utfall - VERSION 8 (Slutgiltig geografisk logik)
analysis_data <- faceoff_registry %>%
  inner_join(df_processed, by = c("gameid", "true_fo_pair_id"), relationship = "many-to-many") %>%
  group_by(gameid, true_fo_pair_id, teamid.x, my_zon, my_role) %>%
  summarise(
    i_made_goal   = any(eventname == "goal" & outcome == "successful" & teamid.y == first(teamid.x)),
    opp_made_goal = any(eventname == "goal" & outcome == "successful" & teamid.y != first(teamid.x)),
    i_made_shot   = any(eventname == "shot" & outcome == "successful" & teamid.y == first(teamid.x)),
    opp_made_shot = any(eventname == "shot" & outcome == "successful" & teamid.y != first(teamid.x)),
    
    # Positioner
    my_max_x = suppressWarnings(max(c(-100, xadjcoord[teamid.y == first(teamid.x)]), na.rm = TRUE)),
    opp_max_x = suppressWarnings(max(c(-100, xadjcoord[teamid.y != first(teamid.x)]), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    total_utfall = case_when(
      # 1. Högprioriterade händelser
      i_made_goal   ~ "1. Gjort Mål",
      opp_made_goal ~ "2. Insläppt Mål",
      i_made_shot   ~ "3. Skott på mål (Egna)",
      opp_made_shot ~ "4. Skott på mål (Emot)",
      
      # 2. Motståndarens avancemang (Spegling för Zon 1)
      opp_max_x > 25 & my_zon == "1. Offensiv Zon" ~ "7b. Tappat hela vägen till Def-Zon",
      opp_max_x > 5  & my_zon == "1. Offensiv Zon" ~ "6a. Tappat till Mittzon",
      
      # 3. Offensiv zon-etablering
      my_max_x > 25 & my_zon == "1. Offensiv Zon" ~ "5a. Behållit tryck i Off-Zon",
      my_max_x > 25 & my_zon != "1. Offensiv Zon" ~ "5b. Ny etablering i Off-Zon",
      
      # 4. Neutral zon / Mittzon (Här har vi städat upp för Zon 2, 3 och 4)
      my_max_x > -25 & my_max_x <= 25 & my_zon %in% c("1. Offensiv Zon", "2. Neutral Zon (Off)", "3. Mitt-tekning") ~ "6b. Kontroll i Mittzon",
      my_max_x > -25 & my_zon %in% c("4. Neutral Zon (Def)", "5. Defensiv Zon") ~ "6c. Tagit sig ur egen zon",
      
      # 5. Defensiv zon
      my_max_x <= -25 & my_zon == "5. Defensiv Zon" ~ "7a. Fastlåst i Def-Zon",
      
      # 6. Fallback
      TRUE ~ "7c. Förlorad zon/Inget framsteg"
    )
  )

# 5. Slutgiltig rapport
final_report <- analysis_data %>%
  group_by(my_zon, my_role, total_utfall) %>%
  summarise(antal_tekningar = n(), .groups = "drop") %>%
  group_by(my_zon, my_role) %>%
  mutate(procent = round(antal_tekningar / sum(antal_tekningar) * 100, 2)) %>%
  arrange(my_zon, my_role, total_utfall)

print(final_report, n = Inf)
