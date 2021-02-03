library(tidyverse)

header <- tibble(`0` = 1, 
                 `0_1` = 0,
                 Header = c("Start_track", "Title_t", "Time_signature", "Time_signature"),
                 `0_2` = c(NA, "Output", "4", "4"),
                 `1` = c(NA, NA, 2, 2),
                 `96` = c(NA, NA, 36, 36),
                 ` ` = c(NA, NA, 8, 8))

footer <- tibble(`0` = c(1, 0),
                 `0_1` = c(max(midi_complete$time), 0),
                 Header = c("End_track", "End_of_file"),
                 `0_2` = c(NA, NA),
                 `1` = c(NA, NA),
                 `96` = c(NA, NA),
                 ` ` = c(NA, NA))

MIDI_unwrapped <- tibble(time = 0:max(midi_complete$time))

unique_char <- midi_complete$notes %>% 
  str_extract_all(boundary("character")) %>% 
  unlist() %>% 
  unique() %>% 
  na.omit()

for(column in unique_char){
  MIDI_unwrapped <- MIDI_unwrapped %>% 
                     mutate(!!column := str_extract(midi_complete$notes, !!column))
}

# TO DO: make the below function work better!  currently it breaks when an on/off is only 1 time unit apart

MIDI_consolidated <- 
  MIDI_unwrapped %>% 
    pivot_longer(!time, names_to = "attacks", values_to = "note") %>%
    select(-attacks) %>% 
    filter(!is.na(note)) %>% 
    arrange(note, time) %>% 
    mutate(behind_diff = time - lag(time, default = -700),
           ahead_diff = lead(time, default = -700) - time) %>% 
    filter(abs(ahead_diff) > 1 | abs(behind_diff) > 1) %>% 
    select(time, note) %>% 
    mutate(state = rep(c("on", "off"), dim(.)[1]/2),
           time = as.double(time),
           time = case_when(state == "off" ~ time + 1,
                            TRUE ~ time)) %>% 
    arrange(time)


MIDI_output <-
  MIDI_consolidated %>% 
  transmute(`0` = 1,
            `0_1` = time,
            Header = str_replace(state, "on", "Note_on_c"),
            Header = str_replace(Header, "off", "Note_off_c"),
            `0_2` = "0",
            `1` = asc(note),
             `96` = 100,
            ` ` = NA) %>% 
  add_row(header, .before = 1) %>% 
  add_row(footer)
  
MIDI_output %>% write_csv("output/output.csv")

# Old Function (replaced by pivot_longer)
# MIDI_test %>% 
#   mutate(A = replace_na(A, "NOPE")) %>% 
#   mutate(state = case_when(lag(A) == "NOPE" & A != "NOPE" & lead(A) != "NOPE" ~ "on",
#                            lag(A) != "NOPE" & A == "NOPE" & lead(A) == "NOPE" ~ "off",
#                            TRUE ~ "other")) %>% 
#   na_if("NOPE") %>% 
#   fill(A) %>% 
#   filter(state == "on" | state == "off")


