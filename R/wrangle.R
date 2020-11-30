library(tidyverse)
library(haven)
library(stringr)
library(rio)


clean_ft <- function(ft) {
  ft_new <- case_when(ft >= 0 & ft <= 100 ~ ft,
                      ft < 0 ~ NA_integer_,
                      ft > 100 ~ NA_integer_)
  return(ft_new)
}

#2016
df <- rio::import("raw-data/anes_timeseries_2016.zip", which = "anes_timeseries_2016_rawdata.txt")%>%
  select(weight = V160101,
         race = V161310x,
         social_classa = V161307, #Self-reported social class, 1-4 lower, working, middle, upper. -1, -8, -9 NA
         social_classb = V162133, #Self-reported social class, 1-4 lower, working, middle, upper. -1, -8, -9 NA
         education = V161270,
         age = V161267,
         sex = V161342,
         income = V161361x,
         religion = V161265x,
         religious_service_freq = V161245,
         party_id_7cat = V161158x, # Does R think of themselves as a Dem, Rep, Ind or what?  This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         party_id_3cat = V161155,
         ft_hillary_clinton = V162078, #1-100, 100 warmest. -99: NA
         ft_donald_trump = V162079,#, #1-100, 100 warmest. -99: NA
         ft_libertarian_pres_cand = V162080,
         ft_green_pres_cand = V162081,
         ft_john_roberts = V162093,
         ft_pope_francis = V162094,
         ft_christian_fundamentalists = V162095,
         ft_feminists = V162096,
         ft_liberals = V162097,
         ft_labor_unions = V162098,
         ft_poor_people = V162099,
         ft_big_business = V162100,
         ft_conservatives = V162101,
         ft_us_supreme_court = V162102,
         ft_gay_men_and_lesbians = V162103,
         ft_congress = V162104,
         ft_rich_people = V162105,
         ft_muslims = V162106,
         ft_christians = V162107,
         ft_jews = V162108,
         ft_tea_party = V162109,
         ft_police = V162110,
         ft_transgender_people = V162111,
         ft_scientists = V162112,
         ft_black_lives_matter = V162113
         
         #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
         
         )%>% 
  mutate(age = na_if(age, -9)) %>%
  mutate(sex2 = sex,
         sex = recode(sex, 
                      "1" = "Male",
                      "2" = "Female",
                      "3" = "Other",
                      "-9" = NA_character_),
         sex = reorder(sex, sex2)) %>% 
  mutate(religion = recode(religion, 
                           "1" = "Mainline Protestant",
                           "2" = "Evangelical Protestant",
                           "3" = "Black Protestant",
                           "4" = "Roman Catholic",
                           "5" = "Undifferentiated Christian",
                           "6" = "Jewish",
                           "7" = "Other Religion",
                           "8" = "Not Religious",
                           "-2" = NA_character_)) %>%
  mutate(rsf2 = religious_service_freq,
         religious_service_freq= recode(religious_service_freq, 
                           "1" = "Every Week",
                           "2" = "Almost Every Week",
                           "3" = "Once or Twice a Month",
                           "4" = "A Few Times a Year",
                           "5" = "Never",
                           "-1" = NA_character_,
                           "-9" = NA_character_),
         religious_service_freq = reorder(religious_service_freq, -rsf2)) %>%
  mutate(income_category = case_when(income == -9 ~ NA_character_,
                                    income <= 14 ~ "Less than $50k",
                                    income > 14 & income <= 22 ~ "Between $50k and $100k",
                                    income > 22 & income <= 27~ "Between $100k and $250k",
                                    income >= 28 ~ "More than $250k"),
         income_category = reorder(income_category, income)) %>%
  mutate(college_degree = case_when(education == -9 ~ NA_character_,
                                    education <= 12 ~ "No College Degree",
                                    education > 12 ~ "College Degree"),
         college_degree = reorder(college_degree, education)) %>%
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
                        "3" = "Independent, leans Democrat", 
                        "4" = "Independent, leans neither", 
                        "5" = "Independent, leans Republican", 
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
  mutate(across(starts_with("ft_"), clean_ft)) %>%
  select(social_class, #just re-selecting to put the cols in a nicer order
         race,
         age,
         sex, 
         college_degree,
         income_category,
         religion,
         religious_service_freq,
         party_id_3cat,
         party_id_7cat,
         starts_with("ft_"))%>%
  glimpse()%>%#
  write_rds("data/anes-2016.rds")%>%
  write_csv("data/anes-2016.csv") %>%
  glimpse()


###########################
###########################



####
####


