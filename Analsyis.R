library(tidyverse)
library(dplyr)
library(maps)

#load data in
prison_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#find percent of population in jail in a column
prison_df <- mutate(prison_df, percent_jail_of_pop = total_jail_pop/total_pop)

#filter by year 2002
prison_2002 <- prison_df[prison_df$year == "2002", "percent_jail_of_pop"]

#What is the lowest year in the data set?
min_year <- min(prison_df$year)

#What is the highest year in the data set?
max_year <- max(prison_df$year)

#what county has the highest percent in jail compared to its entire population in 2002?
max_ratio_2002 <- max(prison_2002, na.rm = TRUE)
max_jail_percent_2002 <- prison_df[prison_df$percent_jail_of_pop == max_ratio_2002 & prison_df$year == "2002", "county_name"]
max_jail_percent_2002 <- na.omit(max_jail_percent_2002)

#what county has the lowest percent in jail compared to its entire population in 2002?
min_ratio_2002 <- min(prison_2002, na.rm = TRUE)
min_jail_percent_2002 <- prison_df[prison_df$percent_jail_of_pop == min_ratio_2002 & prison_df$year == "2002", "county_name"]
min_jail_percent_2002 <- na.omit(min_jail_percent_2002)

#what is the percentage of counties where African american inmates are 
#overly represented than the average amount in jail in 2002?
amount_over_avg_b <- prison_df %>%
  filter(year == 2002) %>%
  summarize(amount_over_b = sum((black_jail_pop/total_jail_pop) >= percent_jail_of_pop, na.rm = TRUE))
amount_over_avg_b <- 2558
total_2002 <- 3139
percent_over_avg_b <- amount_over_avg_b/total_2002

#what is the difference between the change in population in jail in Los Angeles County
#between 2002 and 2003?

total_2002 <- round(prison_df[prison_df$year == 2002 & prison_df$county_name == "Los Angeles County", "total_jail_pop"], 1)
total_2003 <- round(prison_df[prison_df$year == 2003 & prison_df$county_name == "Los Angeles County", "total_jail_pop"], 1)
diff_2002_2003 <- total_2003 - total_2002
#there were 1,888 more people in jail in 2003 then 2002.

#what is the ratio between the change in population in jail in Los Angeles County
#between 2002 and 2003?

ratio_2002_2003 <- round(total_2002/total_2003, 3)


#what is the difference between the change in population in jail in New York County
#between 2010 and 2011?

total_2010 <- round(prison_df[prison_df$year == 2010 & prison_df$county_name == "New York County", "total_jail_pop"], 1)
total_2011 <- round(prison_df[prison_df$year == 2011 & prison_df$county_name == "New York County", "total_jail_pop"], 1)
diff_2010_2011 <- total_2011 - total_2010
#there were 244 less people in jail in 2011 then 2010.

#visualization1: how has average incarceration rate varied from 2001 to 2010 for different races in the top 3 biggest counties?
#white LA
prison_df <- mutate(prison_df, perc_white_jail = white_jail_pop/total_jail_pop)

prison_2001_w_la <- prison_df[prison_df$year == "2001" & prison_df$county_name == "Los Angeles County", "perc_white_jail"]
prison_2002_w_la <- prison_df[prison_df$year == "2002" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2003_w_la <- prison_df[prison_df$year == "2003" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2004_w_la <- prison_df[prison_df$year == "2004" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2005_w_la <- prison_df[prison_df$year == "2005" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2006_w_la <- prison_df[prison_df$year == "2006" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2007_w_la <- prison_df[prison_df$year == "2007" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2008_w_la <- prison_df[prison_df$year == "2008" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2009_w_la <- prison_df[prison_df$year == "2009" & prison_df$county_name == "Los Angeles County","perc_white_jail"]
prison_2010_w_la <- prison_df[prison_df$year == "2010" & prison_df$county_name == "Los Angeles County","perc_white_jail"]

w_la_2001_2010 <- c(prison_2001_w_la, prison_2002_w_la, prison_2003_w_la, prison_2004_w_la, prison_2005_w_la,
                    prison_2006_w_la, prison_2007_w_la, prison_2008_w_la, prison_2009_w_la, prison_2010_w_la)
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
w_la_2001_2010_df <- data.frame(years, w_la_2001_2010)

#visualization1
#african_americans LA
prison_df <- mutate(prison_df, percent_afr_pop = black_jail_pop/total_jail_pop)

prison_2001_aa_la <- prison_df[prison_df$year == "2001" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2002_aa_la <- prison_df[prison_df$year == "2002" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2003_aa_la <- prison_df[prison_df$year == "2003" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2004_aa_la <- prison_df[prison_df$year == "2004" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2005_aa_la <- prison_df[prison_df$year == "2005" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2006_aa_la <- prison_df[prison_df$year == "2006" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2007_aa_la <- prison_df[prison_df$year == "2007" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2008_aa_la <- prison_df[prison_df$year == "2008" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2009_aa_la <- prison_df[prison_df$year == "2009" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]
prison_2010_aa_la <- prison_df[prison_df$year == "2010" & prison_df$county_name == "Los Angeles County", "percent_afr_pop"]

aa_la_2001_2010 <- c(prison_2001_aa_la, prison_2002_aa_la, prison_2003_aa_la, prison_2004_aa_la, prison_2005_aa_la,
                     prison_2006_aa_la, prison_2007_aa_la, prison_2008_aa_la, prison_2009_aa_la, prison_2010_aa_la)
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)

aa_la_2001_2010_df <- data.frame(years, aa_la_2001_2010)

#visualization1
#white NY
prison_2001_w_ny <- prison_df[prison_df$year == "2001" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2002_w_ny <- prison_df[prison_df$year == "2002" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2003_w_ny <- prison_df[prison_df$year == "2003" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2004_w_ny <- prison_df[prison_df$year == "2004" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2005_w_ny <- prison_df[prison_df$year == "2005" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2006_w_ny <- prison_df[prison_df$year == "2006" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2007_w_ny <- prison_df[prison_df$year == "2007" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2008_w_ny <- prison_df[prison_df$year == "2008" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2009_w_ny <- prison_df[prison_df$year == "2009" & prison_df$county_name == "New York County","perc_white_jail"]
prison_2010_w_ny <- prison_df[prison_df$year == "2010" & prison_df$county_name == "New York County","perc_white_jail"]

w_ny_2001_2010 <- c(prison_2001_w_la, prison_2002_w_la, prison_2003_w_la, prison_2004_w_la, prison_2005_w_la,
                    prison_2006_w_la, prison_2007_w_la, prison_2008_w_la, prison_2009_w_la, prison_2010_w_la)
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
w_ny_2001_2010_df <- data.frame(years, w_ny_2001_2010)

#visualization1
#African american NY
prison_2001_aa_ny <- prison_df[prison_df$year == "2001" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2002_aa_ny <- prison_df[prison_df$year == "2002" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2003_aa_ny <- prison_df[prison_df$year == "2003" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2004_aa_ny <- prison_df[prison_df$year == "2004" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2005_aa_ny <- prison_df[prison_df$year == "2005" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2006_aa_ny <- prison_df[prison_df$year == "2006" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2007_aa_ny <- prison_df[prison_df$year == "2007" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2008_aa_ny <- prison_df[prison_df$year == "2008" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2009_aa_ny <- prison_df[prison_df$year == "2009" & prison_df$county_name == "New York County", "percent_afr_pop"]
prison_2010_aa_ny <- prison_df[prison_df$year == "2010" & prison_df$county_name == "New York County", "percent_afr_pop"]

aa_ny_2001_2010 <- c(prison_2001_aa_ny, prison_2002_aa_ny, prison_2003_aa_ny, prison_2004_aa_ny, prison_2005_aa_ny,
                     prison_2006_aa_ny, prison_2007_aa_ny, prison_2008_aa_ny, prison_2009_aa_ny, prison_2010_aa_ny)
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)

aa_ny_2001_2010_df <- data.frame(years, aa_ny_2001_2010)

#visualization1
#white Cook
prison_2001_w_m <- prison_df[prison_df$year == "2001" & prison_df$county_name == "Maricopa County", "perc_white_jail"]
prison_2002_w_m <- prison_df[prison_df$year == "2002" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2003_w_m <- prison_df[prison_df$year == "2003" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2004_w_m <- prison_df[prison_df$year == "2004" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2005_w_m <- prison_df[prison_df$year == "2005" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2006_w_m <- prison_df[prison_df$year == "2006" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2007_w_m <- prison_df[prison_df$year == "2007" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2008_w_m <- prison_df[prison_df$year == "2008" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2009_w_m <- prison_df[prison_df$year == "2009" & prison_df$county_name == "Maricopa County","perc_white_jail"]
prison_2010_w_m <- prison_df[prison_df$year == "2010" & prison_df$county_name == "Maricopa County","perc_white_jail"]

w_m_2001_2010 <- c(prison_2001_w_m, prison_2002_w_m, prison_2003_w_m, prison_2004_w_m, prison_2005_w_m,
                   prison_2006_w_m, prison_2007_w_m, prison_2008_w_m, prison_2009_w_m, prison_2010_w_m)
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
w_m_2001_2010_df <- data.frame(years, w_m_2001_2010)

#visualization1
#African american Cook
prison_2001_aa_m <- prison_df[prison_df$year == "2001" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2002_aa_m <- prison_df[prison_df$year == "2002" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2003_aa_m <- prison_df[prison_df$year == "2003" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2004_aa_m <- prison_df[prison_df$year == "2004" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2005_aa_m <- prison_df[prison_df$year == "2005" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2006_aa_m <- prison_df[prison_df$year == "2006" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2007_aa_m <- prison_df[prison_df$year == "2007" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2008_aa_m <- prison_df[prison_df$year == "2008" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2009_aa_m <- prison_df[prison_df$year == "2009" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]
prison_2010_aa_m <- prison_df[prison_df$year == "2010" & prison_df$county_name == "Maricopa County", "percent_afr_pop"]

aa_m_2001_2010 <- c(prison_2001_aa_m, prison_2002_aa_m, prison_2003_aa_m, prison_2004_aa_m, prison_2005_aa_m,
                    prison_2006_aa_m, prison_2007_aa_m, prison_2008_aa_m, prison_2009_aa_m, prison_2010_aa_m)
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)

aa_m_2001_2010_df <- data.frame(years, aa_m_2001_2010)

#visualization1 ggplot

plot1 <- ggplot(data = w_la_2001_2010_df, aes(x = years, y = w_la_2001_2010)) +
  geom_point(data = w_la_2001_2010_df, aes(x = years, y = w_la_2001_2010, color = "White Los Angeles County")) +
  geom_point(data = aa_la_2001_2010_df, aes(x = years, y = aa_la_2001_2010, color = "African American Los Angeles County")) +
  geom_point(data = w_ny_2001_2010_df, aes(x = years, y = w_ny_2001_2010, color = "White New York County")) +
  geom_point(data = aa_ny_2001_2010_df, aes(x = years, y = aa_ny_2001_2010, color = "African American New York County")) +
  geom_point(data = w_m_2001_2010_df, aes(x = years, y = w_m_2001_2010, color = "White Maricopa County")) +
  geom_point(data = aa_m_2001_2010_df, aes(x = years, y = aa_m_2001_2010, color = "African American Maricopa County")) +
  ggtitle("Percent of Prison Population by Race") +
  labs(y = "Average Percent of Population in Jail Across all Counties",
       x = "Year",
       color = "Legend")

#print(plot1)


#visualization2: What is the relationship between men and women in jail in Los
#Angeles County from 2000 to 2017?

#women
prison_2000_w <- prison_df[prison_df$year == "2000" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2001_w <- prison_df[prison_df$year == "2001" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2002_w <- prison_df[prison_df$year == "2002" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2003_w <- prison_df[prison_df$year == "2003" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2004_w <- prison_df[prison_df$year == "2004" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2005_w <- prison_df[prison_df$year == "2005" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2006_w <- prison_df[prison_df$year == "2006" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2007_w <- prison_df[prison_df$year == "2007" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2008_w <- prison_df[prison_df$year == "2008" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2009_w <- prison_df[prison_df$year == "2009" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2010_w <- prison_df[prison_df$year == "2010" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2011_w <- prison_df[prison_df$year == "2011" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2012_w <- prison_df[prison_df$year == "2012" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2013_w <- prison_df[prison_df$year == "2013" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2014_w <- prison_df[prison_df$year == "2014" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2015_w <- prison_df[prison_df$year == "2015" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2016_w <- prison_df[prison_df$year == "2016" & prison_df$county_name == "Los Angeles County","female_jail_pop"]
prison_2017_w <- prison_df[prison_df$year == "2017" & prison_df$county_name == "Los Angeles County","female_jail_pop"]

#men
prison_2000_m <- prison_df[prison_df$year == "2000" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2001_m <- prison_df[prison_df$year == "2001" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2002_m <- prison_df[prison_df$year == "2002" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2003_m <- prison_df[prison_df$year == "2003" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2004_m <- prison_df[prison_df$year == "2004" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2005_m <- prison_df[prison_df$year == "2005" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2006_m <- prison_df[prison_df$year == "2006" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2007_m <- prison_df[prison_df$year == "2007" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2008_m <- prison_df[prison_df$year == "2008" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2009_m <- prison_df[prison_df$year == "2009" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2010_m <- prison_df[prison_df$year == "2010" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2011_m <- prison_df[prison_df$year == "2011" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2012_m <- prison_df[prison_df$year == "2012" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2013_m <- prison_df[prison_df$year == "2013" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2014_m <- prison_df[prison_df$year == "2014" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2015_m <- prison_df[prison_df$year == "2015" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2016_m <- prison_df[prison_df$year == "2016" & prison_df$county_name == "Los Angeles County","male_jail_pop"]
prison_2017_m <- prison_df[prison_df$year == "2017" & prison_df$county_name == "Los Angeles County","male_jail_pop"]

#join together
m_2000_2017 <- c(prison_2000_m,  prison_2001_m, prison_2002_m, prison_2003_m, prison_2004_m, 
                 prison_2005_m, prison_2006_m, prison_2007_m, prison_2008_m,
                 prison_2009_m, prison_2010_m, prison_2011_m, prison_2012_m,
                 prison_2013_m, prison_2014_m, prison_2015_m, prison_2016_m,
                 prison_2017_m
)

w_2000_2017 <- c(prison_2000_w, prison_2001_w, prison_2002_w, prison_2003_w, prison_2004_w, 
                 prison_2005_w, prison_2006_w, prison_2007_w, prison_2008_w,
                 prison_2009_w, prison_2010_w, prison_2011_w, prison_2012_w,
                 prison_2013_w, prison_2014_w, prison_2015_w, prison_2016_w,
                 prison_2017_w
)


m_w_2000_2017 <- data.frame(m_2000_2017, w_2000_2017)

#ggplot

plot_2 <-ggplot(data = m_w_2000_2017, aes(x= m_2000_2017, w_2000_2017)) +
  geom_point() +
  labs(y = "Total Amount of Women in Jail in Los Angeles County",
       x = "Total Amount of Men in Jail in Los Angeles County") +
  ggtitle("Relationship Between Men and Women in Jail in Los Angeles County") 


#print(plot_2)

#map

#load map data
MainStates <- map_data("state")


MainStates <- as.data.frame(MainStates)

#use abbreviations to match with prison_df
MainStates[MainStates == "alabama"] <- "AL"
MainStates[MainStates == "arizona"] <- "AZ"
MainStates[MainStates == "arkansas"] <- "AR"
MainStates[MainStates == "california"] <- "CA"
MainStates[MainStates == "colorado"] <- "CO"
MainStates[MainStates == "connecticut"] <- "CT"
MainStates[MainStates == "district of columbia"] <- "DC"
MainStates[MainStates == "florida"] <- "FL"
MainStates[MainStates == "georgia"] <- "GA"
MainStates[MainStates == "idaho"] <- "ID"
MainStates[MainStates == "illinois"] <- "IL"
MainStates[MainStates == "delaware"] <- "DE"
MainStates[MainStates == "indiana"] <- "IN"
MainStates[MainStates == "iowa"] <- "IA"
MainStates[MainStates == "kansas"] <- "KS"
MainStates[MainStates == "kentucky"] <- "KY"
MainStates[MainStates == "louisiana"] <- "LA"
MainStates[MainStates == "maine"] <- "ME"
MainStates[MainStates == "maryland"] <- "MD"
MainStates[MainStates == "massachusetts"] <- "MA"
MainStates[MainStates == "michigan"] <- "MI"
MainStates[MainStates == "minnesota"] <- "MN"
MainStates[MainStates == "mississippi"] <- "MS"
MainStates[MainStates == "missouri"] <- "MO"
MainStates[MainStates == "montana"] <- "MT"
MainStates[MainStates == "nebraska"] <- "NE"
MainStates[MainStates == "nevada"] <- "NV"
MainStates[MainStates == "new hampshire"] <- "NH"
MainStates[MainStates == "new jersey"] <- "NJ"
MainStates[MainStates == "new mexico"] <- "NM"
MainStates[MainStates == "new york"] <- "NY"
MainStates[MainStates == "north carolina"] <- "NC"
MainStates[MainStates == "north dakota"] <- "ND"
MainStates[MainStates == "ohio"] <- "OH"
MainStates[MainStates == "oklahoma"] <- "OK"
MainStates[MainStates == "oregon"] <- "OR"
MainStates[MainStates == "pennsylvania"] <- "PA"
MainStates[MainStates == "rhode island"] <- "RI"
MainStates[MainStates == "south carolina"] <- "SC"
MainStates[MainStates == "south dakota"] <- "SD"
MainStates[MainStates == "tennessee"] <- "TN"
MainStates[MainStates == "texas"] <- "TX"
MainStates[MainStates == "utah"] <- "UT"
MainStates[MainStates == "vermont"] <- "VT"
MainStates[MainStates == "virginia"] <- "VA"
MainStates[MainStates == "washington"] <- "WA"
MainStates[MainStates == "west virginia"] <- "WV"
MainStates[MainStates == "wisconsin"] <- "WI"
MainStates[MainStates == "wyoming"] <- "WY"


#make dataframe with total amount of juveniles in each state
juvenile_pop <- prison_df %>%
  group_by(state) %>%
  summarise(
    total_juv = sum(female_juvenile_jail_pop + male_juvenile_jail_pop, na.rm = TRUE)
  ) 

juvenile_pop <- as.data.frame(juvenile_pop)

#removing CT, DE, HI, RI, VT because at 0
MainStates <- MainStates %>%
  filter(region != "CT", region != "DE", region != "HI", region != "RI",
         region != "VT", region != "AK")

#Bias:Alaksa is not inclued in the configuration
#will have to be removed and this bias will be note
juvenile_pop <- juvenile_pop %>%
  filter(state != "AK") %>%
  select(state, total_juv) 


names(juvenile_pop)[names(juvenile_pop) == "state"] <- "region"


#combine total juvenile population with states df
MergedStates <- inner_join(MainStates, juvenile_pop, by = "region")

plot3 <- ggplot() +
  geom_polygon(data = MergedStates, aes(x = long, y = lat, group = group, fill = total_juv),
               color = "white", size = 0.2) +
  scale_fill_continuous(name = "Total Juvenile Population",
                        high = "#B0E0E6", low = "#00008B") +
  ggtitle("Total Juvenile Jail Population by State") 


print(plot3)