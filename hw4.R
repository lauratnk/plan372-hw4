#load libraries
library(tidycensus)
library(tidyverse)
library(sf)
library(dplyr)

#load data
data = read.csv("airport_pairs.csv")

####Q1####
#filter to RDU arrival/departures with more than 10,000 passengers 
rdu_data = subset(data, origin=="RDU" | dest=="RDU")
rdu_data = filter(rdu_data, passengers > 10000)

#sorted passenger no. highest to lowest
rdu_data = rdu_data %>% arrange(desc(passengers))

#the most popular non-stop destination from RDU is ATL
rdu_data_origin = filter(rdu_data, origin=="RDU")

####Q2####
#load ACS 5-year census data
acs_vars = load_variables(2020, "acs5")
#look at available variables
view(acs_vars)

#create table of CBSAs with relevant variables
travel_vol_data = get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables=c(
    "total_pop"="B01001_001",
    "income_over_75k"="B06010_011"
  ),
  year = 2020,
  survey = "acs5",
  output="wide"
)


#clean up table
##delete unnecessary columns
travel_vol_data = subset(travel_vol_data, select = -total_popM)
travel_vol_data = subset(travel_vol_data, select = -income_over_75kM)
##change GEOID to integer
travel_vol_data$GEOID = as.integer(travel_vol_data$GEOID)


#perform joins
##join to origin
airport_to_airpot_vol = left_join(rdu_data, travel_vol_data, by = c("origin_cbsa" = "GEOID"))
##join to destination
airport_to_airpot_vol = left_join(airport_to_airpot_vol, travel_vol_data, by = c("dest_cbsa" = "GEOID"))


#clean data up again
airport_to_airpot_vol = subset(airport_to_airpot_vol, select = -NAME.y)
airport_to_airpot_vol = subset(airport_to_airpot_vol, select = -NAME.x)
colnames(airport_to_airpot_vol)[11] = "origin_pop"
colnames(airport_to_airpot_vol)[12] = "origin_inc_75k"
colnames(airport_to_airpot_vol)[13] = "dest_pop"
colnames(airport_to_airpot_vol)[14] = "dest_inc_75k"


#make new dataset to visualize CBSA to CBSA volumes
CBSA_to_CBSA_vol = group_by(airport_to_airpot_vol, origin_cbsa, origin_cbsa_name, dest_cbsa, 
                            dest_cbsa_name,origin_pop,dest_pop,origin_inc_75k,
                            dest_inc_75k) %>%
  summarise(total_pass = sum(passengers), mean_miles=mean(distancemiles))


raleigh_origin_CBSA_vol = filter(CBSA_to_CBSA_vol, origin_cbsa==39580)
raleigh_dest_CBSA_vol = filter(CBSA_to_CBSA_vol, dest_cbsa==39580)


#create scatter plots
##origin population and total passengers
ggplot(raleigh_dest_CBSA_vol, aes(y=total_pass, x=origin_pop)) +
  geom_point() +
  geom_smooth()
##destination population and total passengers
ggplot(raleigh_origin_CBSA_vol, aes(x=dest_pop, y=total_pass)) +
  geom_point() +
  geom_smooth()
##flight distance and total passengers
ggplot(CBSA_to_CBSA_vol, aes(x=mean_miles, y=total_pass)) +
  geom_point() +
  geom_smooth()


##extra credit: origin income and total passengers
ggplot(raleigh_dest_CBSA_vol, aes(x=origin_inc_75k, y=total_pass)) +
  geom_point()+
  geom_smooth()


##extra credit: dest income and total passengers
ggplot(raleigh_origin_CBSA_vol, aes(x=dest_inc_75k, y=total_pass)) +
  geom_point()+
  geom_smooth()

####Q3#####
##create new table by joining data table to travel vol table

Q3_by_airports = left_join(data, travel_vol_data, by = c("origin_cbsa" = "GEOID"))
Q3_by_airports = left_join(Q3_by_airports, travel_vol_data, by = c("dest_cbsa" = "GEOID"))

##clean up
Q3_by_airports = subset(Q3_by_airports, select = -NAME.y)
Q3_by_airports = subset(Q3_by_airports, select = -NAME.x)
colnames(Q3_by_airports)[11] = "origin_pop"
colnames(Q3_by_airports)[12] = "origin_inc_75k"
colnames(Q3_by_airports)[13] = "dest_pop"
colnames(Q3_by_airports)[14] = "dest_inc_75k"

Q3_by_CBSAs = group_by(Q3_by_airports, origin_cbsa_name, dest_cbsa_name,origin_pop,dest_pop,origin_inc_75k,
                       dest_inc_75k) %>%
  summarise(total_pass = sum(passengers), mean_miles=mean(distancemiles))

#regression
passenger_inc_model = lm(total_pass~origin_pop+dest_pop+mean_miles+origin_inc_75k+dest_inc_75k, Q3_by_CBSAs)
summary(passenger_inc_model)


####Q4####
# -2.928e+04 + 3.435e-02*(origin_pop) + 3.402e-02*(dest_pop) + -2.204e+01*(mean_miles) + -6.166e-02*(origin_inc_75k) + -5.981e-02*(dest_inc_75k)

#RDU to PDX
#actual = 90
-2.928e+04 + 3.435e-02*(1362997) + 3.402e-02*(2472774) + -2.204e+01*(2363) + -6.166e-02*(225101) + -5.981e-02*(403828)

#PDX to RDU
#actual = n/a
-2.928e+04 + 3.435e-02*(2472774) + 3.402e-02*(1362997) + -2.204e+01*(2363) + -6.166e-02*(403828) + -5.981e-02*(225101)

#RDU to ELP
#actual = 30
-2.928e+04 + 3.435e-02*(1362997) + 3.402e-02*(841602) + -2.204e+01*(1606) + -6.166e-02*(225101) + -5.981e-02*(51889)

#ELP to RDU
#actual = n/a
-2.928e+04 + 3.435e-02*(841602) + 3.402e-02*(1362997) + -2.204e+01*(1606) + -6.166e-02*(51889) + -5.981e-02*(225101)

#RDU to TLH
#actual = n/a
-2.928e+04 + 3.435e-02*(1362997) + 3.402e-02*(384783) + -2.204e+01*(496) + -6.166e-02*(225101) + -5.981e-02*(34698)

#TLH to RDU
#actual = n/a
-2.928e+04 + 3.435e-02*(384783) + 3.402e-02*(1362997) + -2.204e+01*(496) + -6.166e-02*(34698) + -5.981e-02*(225101)

#RDU to SAN
#actual = 950
-2.928e+04 + 3.435e-02*(1362997) + 3.402e-02*(3323970) + -2.204e+01*(2193) + -6.166e-02*(225101) + -5.981e-02*(546618)

#SAN to RDU
#actual = 650
-2.928e+04 + 3.435e-02*(3323970) + 3.402e-02*(1362997) + -2.204e+01*(2193) + -6.166e-02*(546618) + -5.981e-02*(225101)



