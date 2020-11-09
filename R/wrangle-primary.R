library(tidyverse)
library(haven)
library(stringr)
library(rio)

#2008

df2008 <- import("data/raw/anes_timeseries_2008.zip", which = "anes_timeseries_2008_rawdata.txt")%>%
  rename(weight = V081001,
         primary_vote_choice = V083077a,
         primary_vote = V083077,
         therm_dem = V083044a, #1-100, 100 warmest. -99: NA
         therm_rep = V083044b, #1-100, 100 warmest. -99: NA
         pid_3 = V083097, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
  )%>%
  select(weight,
         primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         primary_vote,
         therm_dem,
         therm_rep,
         pid_3
  )%>%
  mutate(pid_3 = recode(pid_3, 
                        "5" = "No Preference", 
                        "1" = "Democrat", 
                        "2" = "Republican", 
                        "3" = "Independent",
                        "4" = "Other",
                        "-8" = NA_character_,
                        "-9" = NA_character_))%>%
  mutate(therm_dem = na_if(therm_dem, -9),
         therm_dem = na_if(therm_dem, -8),
         therm_dem = na_if(therm_dem, -6))%>%
  mutate(therm_rep = na_if(therm_rep, -9),
         therm_rep = na_if(therm_rep, -8),
         therm_rep = na_if(therm_rep, -6))%>%
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                    pid_3 == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(primary_vote_dum = recode(primary_vote,
                                   "1" = "1",
                                   "5" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing substantive about it
                                      "-1" = NA_character_,
                                      "1" = "Joe Biden", 
                                      "2" = "Hillary Clinton",
                                      "3" = "Chris Dodd",
                                      "4" = "John Edwards",
                                      "5" = "Rudy Giuliani",
                                      "6" = "Mike Gravel",
                                      "7" = "Mike Huckabee",
                                      "8" = "Duncan Hunter",
                                      "9" = "Alan Keyes",
                                      "10" = "Dennis Kucinich",
                                      "11" = "John McCain",
                                      "12" = "Barack Obama",
                                      "13" = "Ron Paul",
                                      "14" = "Bill Richardson",
                                      "15" = "Mitt Romney",
                                      "16" = "Tom Tancredo",
                                      "17" = "Fred Thompson",
                                      "30" = "Someone Else",
                                      "-8" = NA_character_,
                                      "-9" = NA_character_
  ))%>%
  mutate(cand_party = recode(primary_vote_choice,
                             "Joe Biden" = "Democrat",
                             "Hillary Clinton" = "Democrat",
                             "Chris Dodd" = "Democrat",
                             "John Edwards" = "Democrat",
                             "Rudy Giuliani" = "Republican",
                             "Mike Gravel" = "Democrat",
                             "Mike Huckabee" = "Republican",
                             "Alan Keyes" = "Republican",
                             "Dennis Kucinich" = "Democrat",
                             "John McCain" = "Republican",
                             "Barack Obama" = "Democrat",
                             "Ron Paul" = "Republican",
                             "Bill Richardson" = "Democrat",
                             "Tom Tancredo" = "Republican",
                             "Fred Thompson" = "Republican",
                             "Someone Else" = "Other/Third Party"))%>%
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dum == 0, "Didn't Vote", primary_vote_choice)))%>%
  mutate(primary_vote_simple = case_when(pid_3 != cand_party ~ "Voted in Other Party Primary",
                                         pid_3 == "Democrat" & primary_vote_choice == "Barack Obama" ~ "Winner",
                                         pid_3 == "Republican" & primary_vote_choice == "John McCain" ~ "Winner",
                                         pid_3 != "Indpendent" & primary_vote_choice != "Didn't Vote" ~ "Loser",
                                         pid_3 != "Indpendent" & primary_vote_choice == "Didn't Vote" ~ "Didn't Vote",
                                         TRUE ~ NA_character_))%>%
  select(-primary_vote)%>%
  mutate(year = "2008")%>%
  glimpse()


#
##################
#################



df2012 <- import("data/raw/anes_timeseries_2012.zip", which = "anes_timeseries_2012_rawdata.txt")%>%
  rename(weight = weight_full,
         primary_vote = prevote_primv,
         primary_vote_choice = prevote_primvwho,
         therm_dem = ft_dem, #1-100, 100 warmest. -99: NA
         therm_rep = ft_rep, #1-100, 100 warmest. -99: NA
         pid_3 = pid_self #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
  )%>%
  select(weight,
         primary_vote,
         primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         therm_dem,
         therm_rep,                                                                                                       
         pid_3,
  )%>%
  mutate(pid_3 = recode(pid_3, 
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
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                    pid_3 == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(primary_vote_dum = recode(primary_vote,
                                   "1" = "1",
                                   "2" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing substantive about it
                                      "-1" = NA_character_,
                                      "1" = "Mitt Romney", 
                                      "2" = "Barack Obama",
                                      "3" = "Rick Santorum",
                                      "4" = "Newt Gingrich",
                                      "5" = "Ron Paul",
                                      "6" = "Rick Perry",
                                      "7" = "Michelle Bachmann",
                                      "8" = "Jon Huntsman",
                                      "9" = "Herman Cain",
                                      "95" = "Someone Else",
                                      "-8" = NA_character_,
                                      "-9" = NA_character_
  ))%>%
  mutate(cand_party = recode(primary_vote_choice, .default = "Republican", #supplying Republican as default to save time
                             "Barack Obama" = "Democrat",
                             "Someone Else" = "Other/Third Party"))%>%
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dum == 0, "Didn't Vote", primary_vote_choice)))%>%
  mutate(primary_vote_simple = case_when(pid_3 != cand_party ~ "Voted in Other Party Primary",
                                         pid_3 == "Democrat" & primary_vote_choice == "Barack Obama" ~ "Winner",
                                         pid_3 == "Republican" & primary_vote_choice == "Mitt Romney" ~ "Winner",
                                         pid_3 != "Indpendent" & primary_vote_choice != "Didn't Vote" ~ "Loser",
                                         pid_3 != "Indpendent" & primary_vote_choice == "Didn't Vote" ~ "Didn't Vote",
                                         TRUE ~ NA_character_))%>%
  mutate(year = 2012)%>%
  select(colnames(df2008))%>%
  glimpse()%>%#
  write_rds("data/tidy-2012.rds")%>%
  write_csv("data/tidy-2012.csv")


#2016
df2016 <- rio::import("data/raw/anes_timeseries_2016.zip", which = "anes_timeseries_2016_rawdata.txt")%>%
  rename(weight = V160101,
         primary_vote = V161021,
         primary_vote_choice = V161021a,
         therm_dem = V161095, #1-100, 100 warmest. -99: NA
         therm_rep = V161096, #1-100, 100 warmest. -99: NA
         pid_7 = V161158x, # Does R think of themeselves as a Dem, Rep, Ind or what? We are using this in addition to registration because we are interested in people's conceptions of party This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         pid_3 = V161155#, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
  )%>%
  select(weight,
         primary_vote,
         primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         therm_dem,
         therm_rep,
         pid_7,
         pid_3
  )%>%
  mutate(pid_7 = na_if(pid_7, -9),
         pid_7 = na_if(pid_7, -8))%>%
  mutate(pid_7_num = as.numeric(pid_7), # Recoding party_id as a factor, making sure to order it in a substantive way.
         pid_7 = recode(pid_7, 
                        "1" = "Strong Democrat", 
                        "2" = "Weak Democrat", 
                        "3" = "Independent - Democrat", 
                        "4" = "Independent - Independent", 
                        "5" = "Independent - Republican", 
                        "6" = "Weak Republican", 
                        "7" = "Strong Republican"
         ), # We are leaving off `"-9" = NA`, because we have already set na_if(pid_7, -9) above.
         pid_7 = reorder(pid_7, pid_7_num))%>%
  mutate(pid_3 = recode(pid_3, 
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
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                    pid_3 == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(primary_vote_dum = recode(primary_vote,
                                   "1" = "1",
                                   "2" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_))%>%
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
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dum == 0, "Didn't Vote", primary_vote_choice)))%>%
  mutate(cand_party = recode(primary_vote_choice, .default = NA_character_,
                             "Hillary Clinton" = "Democrat",
                             "Bernie Sanders" = "Democrat",
                             "Another Democrat" = "Democrat",
                             "Donald Trump" = "Republican",
                             "Ted Cruz" = "Republican",
                             "John Kasich" = "Republcian",
                             "Marco Rubio" = "Republican",
                             "Another Republican" = "Republican",
                             "A Third Party Candidate" = "Other/Third Party"
  ))%>%
  mutate(primary_vote_simple = case_when(pid_3 != cand_party ~ "Voted in Other Party Primary",
                                         pid_3 == "Democrat" & primary_vote_choice == "Hillary Clinton" ~ "Winner",
                                         pid_3 == "Republican" & primary_vote_choice == "Donald Trump" ~ "Winner",
                                         pid_3 != "Indpendent" & primary_vote_choice != "Didn't Vote" ~ "Loser",
                                         pid_3 != "Indpendent" & primary_vote_choice == "Didn't Vote" ~ "Didn't Vote",
                                         TRUE ~ NA_character_))%>%
  mutate(year = 2016)%>%
  select(colnames(df2008))%>% #selecting only columns that exist in the 2008 dataset
  glimpse()%>%#
  write_rds("data/tidy-2016.rds")%>%
  write_csv("data/tidy-2016.csv")


###########################
###########################



####
####

primaries <- rbind(df2016, df2008, df2012)%>%
  mutate(pid_3 = factor(pid_3, 
                        levels = c("1" = "Democrat", 
                                   "2" = "Independent", 
                                   "3" = "Republican")),
         primary_vote_choice = as.factor(primary_vote_choice),
         year = as.factor(year),
         primary_vote_simple = factor(primary_vote_simple,
                                      levels = c("1" = "Winner",
                                                 "2" = "Loser",
                                                 "3" = "Didn't Vote",
                                                 "4" = "Voted in Other Party Primary")),
         cand_party = factor(cand_party,
                             levels = c("1" = "Democrat",
                                        "2" = "Republican",
                                        "3" = "Other/Third Party")),
         primary_vote_dum = as.numeric(primary_vote_dum))%>%
  glimpse()%>%
  write_rds("data/tidy-primaries.rds")%>%
  write_csv("data/tidy-primaries.csv")
