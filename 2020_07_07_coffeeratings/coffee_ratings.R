library(tidytuesdayR)
library(tidyverse)

tt <- tidytuesdayR::tt_load('2020-07-07')

# All entries for country_of_origin == 'United States' are wrong
# Fix based on info in 'region' column
# Also shorten country names where possible to reduce text on plots
coffee_ratings <- tt$coffee_ratings %>%
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States" & 
                                     region == "antioquia", 
                                   "Columbia")) %>%
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States" & 
                                     region == "berastagi", 
                                   "Indonesia")) %>%
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States" & 
                                     region == "chikmagalur", 
                                   "India")) %>%
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States" & 
                                     region == "kwanza norte province, angola	", 
                                   "Angola")) %>%
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States", 
                                   "Guatemala")) %>%
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "Tanzania, United Republic Of",
                                   "Tanzania")) %>% 
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "Cote d?Ivoire",
                                   "Cote d'Ivoire"))  %>% 
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States (Hawaii)",
                                   "USA (Hawaii)")) %>% 
  mutate(country_of_origin=replace(country_of_origin, 
                                   country_of_origin == "United States (Puerto Rico)",
                                   "USA (Puerto Rico)")) %>%
  filter(total_cup_points > 0) #removes 1 row

# Total points for each Arabica variety
# remove NAs, 'Other' varieties as they can't be directly compared
var <- coffee_ratings %>%
  filter(!is.na(variety) & variety != 'Other' & species == 'Arabica') %>%
  select(variety, country_of_origin, total_cup_points) %>%
  group_by(variety) %>%
  arrange(desc(total_cup_points), .by_group=TRUE)

# Mean total points for each variety, by country of origin
var_country <- coffee_ratings %>%
  filter(!is.na(variety) & variety != 'Other') %>%
  group_by(variety, country_of_origin) %>%
  summarize(count=n(), mean_score = mean(total_cup_points)) %>%
  arrange(desc(mean_score), .by_group=TRUE)

# Plot for Bourbon variety
ab <- var %>%
  filter(variety == 'Bourbon')

#ab_country <- var_country %>%
#  filter(variety == 'Bourbon')


p <- ggplot(data = ab) +
  geom_point(aes(y=country_of_origin, 
                 x=total_cup_points, 
                 color=country_of_origin)) +
  geom_boxplot(aes(y=country_of_origin, 
                   x=total_cup_points, 
                   fill = country_of_origin),
               alpha=0.5) +
  theme_minimal(base_size = 14) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        panel.background = element_rect(colour = "grey", size=1, fill=NA)) +
  xlim(65, 95) +
  labs(x='Rating out of 100',
       title = 'Arabica Coffee Ratings (var. Bourbon) by country of origin')

show(p)

## COLUMNS, CATEGORIZED ##

# COFFEE METADATA
# "species"               
# "variety" 

# GEOGRAPHIC ORIGIN
# "country_of_origin" NOTE: "United States" entries for this column are WRONG; see region
# "region" 
# "farm_name"
# "altitude"              
# "unit_of_measurement"  
# "altitude_low_meters"   
# "altitude_high_meters" 
# "altitude_mean_meters"  A few of these are very wrong!

# OWNERSHIP AND SUPPLY CHAIN
# "owner"  
# "owner_1"  
# "mill"                 
# "ico_number"           
# "company"     
# "producer"  
# "in_country_partner" 

# HARVESTING AND PROCESSING
# "harvest_year"          
# "grading_date"         
# "processing_method"            
# "expiration" 

# CERTIFICATION         
# "certification_body"    
# "certification_address" 
# "certification_contact"

# TESTING INFORMATION 
# "lot_number"           
# "number_of_bags"        
# "bag_weight" 

# TESTING - OBSERVATIONS
# "category_one_defects"  
# "quakers" =  Beans that didn't roast properly           
# "category_two_defects" 
# "moisture" 
# "color" = Green, Bluish-Green, Blue-Green, None, NA

# TESTING- RATINGS
# "aroma"                
# "flavor"                
# "aftertaste"            
# "acidity"              
# "body"                  
# "balance"               
# "uniformity"           
# "clean_cup"             
# "sweetness"             
# "cupper_points"
# "total_cup_points" = Sum of all ratings


  

           





 







