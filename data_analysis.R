setwd("~/Behavioural Analytics/R.Studio_uni/DISSERTATION")
library(tidyverse)
library(dplyr)
library(stringr)


# read in the data 
markov.data <- read_csv("MARKOV_DATA.csv")
pa.data <- read_csv("procedural_adherence.csv")

markov$Behaviour <- iconv(markov$Behaviour, from = "latin1", to = "UTF-8")

# view data
view(markov.data)
view(pa.data)

# check if there are any NA values
sum(is.na(markov.data))
complete.cases(markov.data)

# checking what behaviour comes next 
markov.data <- markov.data %>%
  arrange(Video, Time) %>%
  group_by(Video) %>%
  mutate(next_behaviour = lead(Behaviour))


# collapse behaviours into broader categories

library(dplyr)
library(stringr)

markov_states <- markov.data %>%
  mutate(states = case_when(
    
    # Procedural Adherence (Use of Force)
    str_detect(Behaviour, "Procedural Adherence - Justification stated") |
      str_detect(Behaviour, "Procedural Adherence - Lawful") |
      str_detect(Behaviour, "Procedural Adherence - Necessary") |
      str_detect(Behaviour, "Procedural Adherence - De-escalation attempt") |
      str_detect(Behaviour, "Procedural Adherence - Proportionality") |
      str_detect(Behaviour, "Procedural Adherence - Stop force once compliance achieved") |
      str_detect(Behaviour, "Procedural Adherence - Medical check offered") |
      str_detect(Behaviour, "Procedural Adherence - Grounds explained") |
      str_detect(Behaviour, "Procedural Adherence - Object of search stated") |
      str_detect(Behaviour, "Procedural Adherence - Warrant card shown") |
      str_detect(Behaviour, "Procedural Adherence - Identity") |
      str_detect(Behaviour, "Procedural Adherence - Station") |
      str_detect(Behaviour, "Procedural Adherence - Entitlement to record of search") |
      str_detect(Behaviour, "Procedural Adherence - Legal power cited") |
      str_detect(Behaviour, "Procedural Adherence - Face being detained") |
      str_detect(Behaviour, "Procedural Adherence - Other relevant behaviour") ~ "Procedural_Adherence",
    
    # Procedural Justice
    str_detect(Behaviour, "Participation") |
    str_detect(Behaviour, "Fairness & Neutrality") |
    str_detect(Behaviour, "Dignity & Respect") |
      str_detect(Behaviour, "Trustworthy Motives") |
      str_detect(Behaviour, "PJT - Other relevant behaviour") ~ "Procedural_Justice",
    
    # Maladaptive
    str_detect(Behaviour, "Maladaptive") ~ "Maladaptive",
    
    # Citizen Behaviour - Cooperation
    str_detect(Behaviour, "Citizen Behaviour - Cooperative") |
    str_detect(Behaviour, "Citizen Behaviour - Hesitant") ~ "Citizen_Cooperation",
    
    # Citizen Behaviour - Resistance
    str_detect(Behaviour, "Citizen Behaviour - Resistant") |
    str_detect(Behaviour, "Citizen Behaviour - Citizen verbally threatens") |
    str_detect(Behaviour, "Citizen Behaviour - Citizen uses physical force") ~ "Citizen_Resistance",
    
    # Citizen Behaviour - Non-compliance
    str_detect(Behaviour, "Citizen Behaviour - Non-compliant") ~ "Citizen_Non-compliance",
    
    # Citizen Behaviour - Escalation
    str_detect(Behaviour, "Citizen Behaviour - Escalation") ~ "Citizen_Escalation",
    
    # Citizen Behaviour - De-escalation
    str_detect(Behaviour, "Citizen Behaviour - De-escalation") ~ "Citizen_De-escalation",
    
    # If nothing matches = NA
    TRUE ~ NA
  ))

View(markov_states)

#=================================================================

# Descriptive Stats

library(lubridate)
length_vids <- c("00:06:23", "00:08:06", "00:07:54", "00:05:27", "00:11:38", "00:24:30", "00:12:00", "00:16:02", "00:06:03")
duration <- hms(length_vids)   # minutes + seconds
mean_sec <- mean(as.numeric(duration))
mean_mins <- mean_sec / 60
mean_hms <- seconds_to_period(mean_mins * 60)

mean_mins

# avg no. of behaviours per vid

avg_behaviours <- markov.data %>%
  group_by(Video) %>%
  summarise(n_behaviours = n()) %>%
  summarise(avg_behaviours = mean(n_behaviours))

avg_behaviours

# freq of behaviour category across vids
behaviour_freq <- markov_states %>%
  group_by(states) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

behaviour_freq
#===================================================================
# RQ1 

## subset relevant states


officer_states <- c("Procedural_Justice", "Maladaptive", "Procedural_Adherence") # including procedural adherence

citizen_states <- c("Citizen_Cooperation", "Citizen_Resistance", "Citizen_Non-compliance", "Citizen_Escalation", "Citizen_De-escalation")


markov_subset <- markov_states %>%
  filter(states %in% c(officer_states, citizen_states)) %>%
  arrange(Video, Time)

View(markov_subset)

## get sequences per interaction

state_sequences <- markov_subset %>%
  group_by(Video) %>%
  summarise(state_seq = list(states)) %>%
  pull(state_seq)

## fit markov chain

mc_m1 <- markovchain::markovchainFit(data = state_sequences)
mc_m1$estimate

## create a matrix
tm_q1 <- mc_m1$estimate@transitionMatrix
tm_q1

## convert to igraph
markov_q1 <- graph_from_adjacency_matrix(tm_q1, mode = "directed", weighted = TRUE, diag = TRUE)


## plot transition diagram

plot(markov_q1,
     layout = layout_in_circle(markov_q1),
     vertex.size = 20,
     vertex.color = "plum1",
     vertex.label = V(markov_q1)$name <- c("Coop", "Desc", "Esc", "N-com", "Res", "Mal", "PA", "PJT"),
     vertex.label.cex = 1,
     vertex.label.color = "navy",
     edge.arrow.size = 0.3, 
     edge.label = round(E(markov_q1)$weight, 2), 
     edge.label.cex = 0.5)

#===========================================================
# RQ2

view(pa.data)

## stop and search adherence items
sas_items <- c("Grounds explained", 
               "Object of search stated",		
               "Warrant card shown (if plainclothes)",		
               "Identity (name / badge number provided)",		
               "Station (officer based at) shared",		
               "Entitlement to record of search mentioned",		
               "Legal power cited (e.g., Section 1 of PACE)",		
               "Face being detained is clearly communicated")

## use of force adherence items
uof_items <- c("Justification stated - e.g., officer verbally explaining why force is needed",		
"Lawful - legal power (if applicable)",		
"Necessary - verbal warning before force (unless immediate threat)",		
"De-escalation attempt - verbal commands, time given to comply, space given", 		
"Proportionality - force level matches citizen resistance", 		
"Stop force once compliance achieved",		
"Medical check offered / performed - checking citizen well-being")

view(uof_items)
view(sas_items)

## splitting procedural adherence into levels depending on the items followed
adherence_data <- pa.data %>%
  group_by(video_id, phase) %>%
  summarise(
    sas_present = sum(item %in% sas_items & present == 1), 
    sas_applicable = sum(item %in% sas_items & present != 999), 
    
    uof_present = sum(item %in% uof_items & present == 1), 
    uof_applicable = sum(item %in% uof_items & present != 999), 
    .groups = "drop") %>%
  
  mutate(SaS_adherence_level = case_when(
    sas_applicable == 0 ~ NA_character_, 
    sas_present == 0 ~ "No_Adherence", 
    sas_present == sas_applicable ~ "Full_Adherence", 
    TRUE ~ "Partial_Adherence"), 
  UoF_Adherence_Level = case_when(
    uof_applicable == 0 ~ NA_character_,
    uof_present == 0 ~ "No_Adherence", 
    uof_present == uof_applicable ~ "Full_Adherence", 
    TRUE ~ "Partial_Adherence"))

view(adherence_data)

adherence_data <- adherence_data %>%
  mutate(adherence_level = case_when(phase == "S&S_GOWISELY" ~ SaS_adherence_level, 
                                     phase == "Use_of_Force_PLAN" ~ UoF_Adherence_Level, 
                                     TRUE ~ NA_character_))
view(adherence_data)


## transition matrices 

tran_states <- table(markov_states$states, markov_states$next_behaviour)
markov_rq2 <- markovchainFit(data = markov_states$states)

markov_rq2$estimate

## comparing full vs. partial adherence

full_data_q2 <- markov_states %>%
  left_join(adherence_data %>%
              select(video_id, adherence_level), 
            by = c("Video" = "video_id"))
view(full_data_q2)


## full adherence
full_adh <- filter(full_data_q2, adherence_level == "Full_Adherence")
view(full_adh)
## partial adherence
partial_adh <- filter(full_data_q2, adherence_level == "Partial_Adherence")
view(partial_adh)

## markov chains 

library(igraph)

all_states <- c("Citizen_Cooperation", "Citizen_Escalation", "Maladaptive",
                "Procedural_Justice", "SaS_Procedural_Adherence",
                "Resistance", "Non_compliance")

## FULL PA with all states (not removing those that have 0)
full_markov <- markovchainFit(data = full_adh$states, 
                              possibleStates = all_states, method = "mle")$estimate

## export transition matrix
tm_full <- full_markov@transitionMatrix
tm_full
## convert to igraph
full_model <- graph_from_adjacency_matrix(tm_full, mode = "directed", weighted = TRUE, diag = TRUE)

# change the names
new_names_full <- c("Coop", "Esc", "Mal", "N-Com", "PJT", "Res", "Full_PA")
V(full_model)$name <- new_names_full

## plot full pa graph
plot.igraph(full_model,
     layout = layout_in_circle(full_model),
     vertex.size = 20,
     vertex.color = "orange",
     vertex.label.cex = 1,
     vertex.label.color = "navy",
     edge.arrow.size = 0.5, 
     edge.label = round(E(full_model)$weight, 2), 
     edge.label.cex = 0.65)

# Partial PA
partial_markov <- markovchainFit(data = partial_adh$states)$estimate

## export tm

tm_partial <- partial_markov@transitionMatrix
tm_partial
## making igraph 

partial_model <- graph_from_adjacency_matrix(tm_partial, mode = "directed", weighted = TRUE, diag = TRUE)
tm_partial
# plotting model  
plot(partial_model,
     layout = layout_in_circle(partial_model), 
     vertex.size = 16,
     vertex.label = V(partial_model)$name <- c("Coop", "Desc", "Esc", "N-Com", "Res", "Mal", "PJT", "UoF_PA", "SaS_PA"),
     vertex.color = "orange",
     vertex.label.cex = 1,
     vertex.label.color = "navy",
     edge.arrow.size = 0.4, 
     edge.label = round(E(full_model)$weight, 2), 
     edge.label.cex = 0.65) 



         
