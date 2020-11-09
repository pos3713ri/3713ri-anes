library(tidyverse)
library(haven)
library(stringr)
library(rio)


#2016
df2016 <- rio::import("data/anes_timeseries_2016.zip", which = "anes_timeseries_2016_rawdata.txt")%>%
  select(weight = V160101,
         race = V161310x,
         social_classa = V161307, #Self-reported social class, 1-4 lower, working, middle, upper. -1, -8, -9 NA
         social_classb = V162133, #Self-reported social class, 1-4 lower, working, middle, upper. -1, -8, -9 NA
         general_vote = V162034, #Did R vote for president in the general?
         general_vote_choice = V162034a, #For whom did R vote for president (NA if they didn't vote)
         primary_vote = V161021, #Did R vote in the Primary?
         primary_vote_choice = V161021a, # for whom did R vote in the primary
         therm_dem = V161095, #1-100, 100 warmest. -99: NA
         therm_rep = V161096, #1-100, 100 warmest. -99: NA
         party_id_7cat = V161158x, # Does R think of themselves as a Dem, Rep, Ind or what?  This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         party_id_3cat = V161155#, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
  )%>%
  mutate(social_classa = recode(social_classa, .default = NA_character_,
                               "1" = "Lower Class",
                               "2" = "Working Class",
                               "3" = "Middle Class",
                               "4" = "Upper Class"))%>%
  mutate(social_classb = recode(social_classb, .default = NA_character_,
                                "1" = "Lower Class",
                                "2" = "Working Class",
                                "3" = "Middle Class",
                                "4" = "Upper Class"))%>%
  mutate(social_class = if_else(is.na(social_classa), social_classb, social_classa))%>% #2016 ANES used two different question setsfor social class, I'm just uniting them here.
  mutate(social_class = factor(social_class, 
                               levels = c("Lower Class",
                                          "Working Class",
                                          "Middle Class",
                                          "Upper Class")))%>%
  select(-social_classa,
         -social_classb)%>%
  mutate(race = as.factor(recode(race, .default = NA_character_,
                       "1" = "White",
                       "2" = "Black",
                       "3" = "Asian/Pacific Islander",
                       "4" = "Indigenous Americans",
                       "5" = "Hispanic",
                       "6" = "Other")))%>%
  mutate(party_id_7cat_num = as.numeric(party_id_7cat), # Recoding party_id as a factor, making sure to order it in a substantive way.
         party_id_7cat = recode(party_id_7cat, .default = NA_character_,
                        "1" = "Strong Democrat", 
                        "2" = "Weak Democrat", 
                        "3" = "Independent - Democrat", 
                        "4" = "Independent - Independent", 
                        "5" = "Independent - Republican", 
                        "6" = "Weak Republican", 
                        "7" = "Strong Republican"), 
         party_id_7cat = reorder(party_id_7cat, party_id_7cat_num))%>%
  mutate(party_id_3cat = recode(party_id_3cat, 
                        "0" = "No Preference", 
                        "1" = "Democrat", 
                        "2" = "Republican", 
                        "3" = "Independent",
                        "5" = "Other",
                        "-8" = NA_character_,
                        "-9" = NA_character_))%>%
  mutate(therm_dem = na_if(therm_dem, -99),
         therm_dem = na_if(therm_dem, -88),
         therm_dem = na_if(therm_dem, -89))%>%
  mutate(therm_rep = na_if(therm_rep, -99),
         therm_rep = na_if(therm_rep, -88),
         therm_rep = na_if(therm_rep, -89))%>%
  mutate(therm_inparty = case_when(party_id_3cat == "Democrat" ~ therm_dem,
                                   party_id_3cat == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(party_id_3cat == "Democrat" ~ therm_rep,
                                    party_id_3cat == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(general_vote_dummy = as.numeric(recode(general_vote, .default = NA_character_,
                                   "1" = "1",
                                   "2" = "0")))%>%
  select(-general_vote)%>%
  mutate(general_vote_choice = as.factor(recode(general_vote_choice, .default = NA_character_,
                                      "1" = "Hillary Clinton",
                                      "2" = "Donald Trump",
                                      "3" = "Gary Johnson",
                                      "4" = "Jill Stein",
                                      "5" = "Other",
                                      "7" = "Other",
                                      "9" = "Other")))%>%
  mutate(primary_vote_dummy = as.numeric(recode(primary_vote,
                                   "1" = "1",
                                   "2" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_)))%>%
  select(-primary_vote)%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing
                                      "-1" = NA_character_,
                                      "1" = "Hillary Clinton", #substantively meaningful about Hillary being "1" and Marco being "7"
                                      "2" = "Bernie Sanders",
                                      "3" = "Another Democrat",
                                      "4" = "Donald Trump",
                                      "5" = "Ted Cruz",
                                      "6" = "John Kasich",
                                      "7" = "Marco Rubio",
                                      "8" = "Another Republican",
                                      "9" = "A Third Party Candidate",
                                      "-8" = NA_character_, #"Don't Know",
                                      "-9" = NA_character_#"Refused"
                                      ))%>%
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dummy == 0, "Didn't Vote",
                                                 primary_vote_choice)))%>%
  select(social_class, #just re-selecting to put the cols in a nicer order
         race,
         general_vote_dummy,
         primary_vote_dummy,
         general_vote_choice,
         primary_vote_choice,
         party_id_7cat,
         party_id_3cat,
         therm_dem,
         therm_rep,
         therm_inparty,
         therm_outparty)%>%
  glimpse()%>%#
  write_rds("data/tidy-2016.rds")%>%
  write_csv("data/tidy-2016.csv")


###########################
###########################



####
####


