#install.packages("lubriadate") 
#install.packages("janitor")

#install.packages("mapdata")
#install.packages("ggplot")
#install.packages("maps")

library(janitor)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(maps)
library(mapdata)
library(ggmap)


ipums_california_2018.csv <- read_csv("Data_for_classes/ipums_california_2018.csv")

# import data
ipums_california_2018.csv <- read_csv("Data_for_classes/ipums_california_2018.csv")

fips_names_california <- read_csv("Data_for_classes/fips_names_california.csv") #county names

annual_aqi_by_county_2018 <- read_csv("Data_for_classes/annual_aqi_by_county_2018.csv") #EPA

#clean EPA
#FIX VARIABLE NAMES
annual_aqi_by_county_2018 <- clean_names(annual_aqi_by_county_2018)

#MAKE NAMES LOWERCASE
annual_aqi_by_county_2018 <- mutate(annual_aqi_by_county_2018, state = tolower(state), county=tolower(county))

# FILTER CALIFORNIA and DROP COUNTYFIP 0
epa_california_2018 <- filter(annual_aqi_by_county_2018, state=="california")

#View(epa_california_2018)

#clean rest
ipums_clean <- clean_names(ipums_california_2018) # makes lowercase

ipums_clean <- select(ipums_clean, "hhwt", "statefip", "countyfip", "sex", "race", "inctot", "poverty") #(only keeping secifided data)

ipums_clean <- filter(ipums_clean, poverty != 0)#drop incomplete info

#MAKE POVERTY DUMMY VARIABLE
ipums_clean <- mutate(ipums_clean, pov100=ifelse(poverty<100,1,0))

#GENERATE DUMMY FOR EVERYONE
ipums_clean <- mutate(ipums_clean, pop=1)

#ADD A DUPLICATE DUMMY VARIABLE FOR POVERTY
ipums_clean <- mutate(ipums_clean, povrate100_2=pov100)

#avg/grouping

#GROUP by country
ipums_group <- group_by(ipums_clean, countyfip)

#SUMMARISE THE GROUPS
ipums_county_sum <- summarise(ipums_group, pov100=sum(pov100), pop=sum(pop), pov100_2=mean(povrate100_2))

#View(ipums_county_sum)


#UNGROUP
ungroup(ipums_group)

#CALCULATE POVERTY RATE
ipums_county_sum <- mutate(ipums_county_sum, povrate100=pov100/pop)


#JOIN THE NAMES
ipums_names <- full_join(ipums_county_sum, fips_names_california, by = "countyfip")

#View(ipums_names)

#fix variable name
epa_california_2018 <- rename(epa_california_2018, county_name=county)

#work

final_ipums_epa <- full_join(epa_california_2018, ipums_names, by = "county_name")

View(final_ipums_epa)

#DROP UNNEEDED VARIABLES
final_ipums_epa <- select(final_ipums_epa, -"state", -"state_name")

final_ipums_epa <- mutate(final_ipums_epa, bad_days=unhealthy_for_sensitive_groups_days+unhealthy_days+very_unhealthy_days+hazardous_days)

#graph
#ggplot(final_ipums_epa, aes(y=bad_days, x=povrate100)) + 
#  geom_point() +
#  labs(y = "Pollution days", x = "Poverty rate (100% cutoff)") +
#  theme(panel.background = element_rect(fill="grey"))


#save data

#write_csv(final_ipums_epa, "~/Google Drive/big-enviro/data/pollution_poverty_california_2018.csv")

#      MAPS

map_us_counties <- map_data("county")
map_california_counties <- filter(map_us_counties, region == "california")

#RENAME VARIABLE
map_california_counties <- rename(map_california_counties, state=region, county_name=subregion)

#JOIN W IPUMS / EPA DATA
final_map <- full_join(final_ipums_epa, map_california_counties, by = "county_name")


ggplot(final_map, aes(y=lat, x=long, group=group, fill=povrate100)) + 
  geom_polygon(color="black") + 
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill="white"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )
        scale_fill_gradient(name = "Poverty Rate", low="blue", high="red", breaks = seq(0.08, 0.20, 0.04))



