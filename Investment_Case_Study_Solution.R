###########################################################################
#################### INVESTMENT CASE STUDY ################################
###########################################################################

# set working directory where you have the case study files
# setwd()

library(tidyr)
library(dplyr)
library(stringr)

######################################### Check Point 1 ##################################################

# Import companies and rounds2 dataframes

companies <- read.delim("companies.txt", sep="\t", stringsAsFactors = F, na.strings = "")
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F,na.string = "")

# Change companies$permalink to upper case
companies <- mutate(companies, permalink = toupper(permalink))

# change rounds2$company_permalink to upper case permalink
rounds2 <- mutate(rounds2, permalink = toupper(company_permalink), company_permalink=NULL)

# How many unique companies are present in rounds2?
# count(distinct(rounds2, permalink))
n_distinct(rounds2$permalink, na.rm = TRUE)

# How many unique companies are present in the companies file?
# count(distinct(companies, permalink))
n_distinct(companies$permalink, na.rm = TRUE)

# In the companies data frame, which column can be used as the unique key for each company? 
# permalink column is the unique key for each company

# Are there any companies in the rounds2 file which are not present in companies?
nrow(anti_join(rounds2, companies, by = "permalink"))
# No

# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
master_frame <- inner_join(companies, rounds2, by = "permalink")

# How many observations are present in master_frame?
nrow(master_frame)


######################################### Data Cleanup for master_frame ##################################

# Remove the observations for raised_amount_usd from dataframe?
# No, as the total missing values are greater than 10% of total observations

# Missing values for raised_amount_usd to be replaced by mean(raised_amount_usd)?
# raised_amount_usd has positive skew so ideally should not be replaced by mean

# We have decided to keep the NA values "as is" and ignored them during calculation na.rm = TRUE

# Missing values for country_code
# Assign Country_Code "MISSING" to missing (NA) values

master_frame$country_code[is.na(master_frame$country_code)] <- "MISSING"

######################################### Check Point 2 ##################################################

# Aggregate and calculate average of raised_amount_usd, grouped by funding type
funding_type_agg <- aggregate(x = master_frame$raised_amount_usd, by=list(y = master_frame$funding_round_type), FUN=mean, na.rm = TRUE, na.action = NULL)

# Rename columns
funding_type_agg <- mutate(funding_type_agg, funding_type = y, average_raised_amount_usd = x, y = NULL, x = NULL)

# 1. Average funding amount of venture type?
filter(funding_type_agg, funding_type == "venture")
# USD 11,748,949

# 2. Average funding amount of angel type?
filter(funding_type_agg, funding_type == "angel")
# USD 958,694.5

# 3. Average funding amount of seed type?
filter(funding_type_agg, funding_type == "seed")
# USD 719,818

# 4. Average funding amount of private equity type?
filter(funding_type_agg, funding_type == "private_equity")
# USD 73,308,593

# 5. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
filter(funding_type_agg, between(average_raised_amount_usd, 5000000, 15000000))
# venture


######################################### Check Point 3 ##################################################

# For the chosen investment type, data frame top9 with the top nine countries (based on the total investment amount each country has received)
top9 <- master_frame %>%
          filter(funding_round_type == "venture") %>%
          select(country_code, raised_amount_usd) %>%
          group_by(country_code) %>%
          summarize(investment = sum(raised_amount_usd, na.rm = TRUE)) %>%
          arrange(desc(investment)) %>%
          head(9)


# 1. Top English-speaking country	
# USA - United States

# 2. Second English-speaking country	
# GBR - United Kingdom

# 3. Third English-speaking country	
# IND - India


######################################### Check Point 4 ##################################################

# Read mapping File
mapping <- read.csv("mapping.csv", check.names = FALSE, stringsAsFactors = FALSE, na.strings = "")

# Update bad values in mapping dataframe
# e.g. category_list "A0lytics" -> "Analytics"
# Change all "0" to na 
# One numeric value in the category_list "Enterprise 2.0" changed to "Enterprise 2.na"
# Change ".na" to "0"
# use gather function to convert to long format with just 3 columns
# filter for value == 1 and remove records containiing NA value for primary_sector
# Remove unwanted columns and change primary_sector to upper case

mapping <- mutate(mapping, primary_sector = str_replace_all(category_list, "0", "na")) %>%
              mutate(primary_sector = str_replace_all(primary_sector,"\\.na", ".0")) %>%
              gather(main_sector, sector_val, 2:10) %>%
              filter(sector_val == 1 & !is.na(primary_sector)) %>%
              mutate(primary_sector = toupper(primary_sector),sector_val = NULL, category_list = NULL)

# Add a new column primary sector with upper case values of category_list
# Separate the primary subsector and write back to primary sector column
# Left join with mapping dataframe using primary_sector column
# Remove observations with NA as main_sector

master_frame_merged <- mutate(master_frame, primary_sector = toupper(category_list)) %>%
                         separate(primary_sector, into=c("primary_sector"),sep="\\|", extra = "drop") %>%
                         left_join(mapping,by = "primary_sector") %>%
                         filter(!is.na(main_sector))    


######################################### Check Point 5 ##################################################

# Data frames from each of the three top english speaking countries, should contain:
# Observations of funding type FT falling within the 5-15 million USD range
# All the columns of the master_frame along with the primary sector and the main sector
# The total number (or count) of investments for each main sector in a separate column
# The total amount invested in each main sector in a separate column

# Data frame D1 for USA
D1 <- filter(master_frame_merged, country_code == "USA" & funding_round_type == "venture" & between(raised_amount_usd, 5000000,15000000)) %>%
        group_by(main_sector) %>%
        mutate(sum_of_investment = sum(raised_amount_usd), count_of_investment = n()) %>%
        arrange(desc(count_of_investment))

# Data frame D2 for GBR
D2 <- filter(master_frame_merged, country_code == "GBR" & funding_round_type == "venture" & between(raised_amount_usd, 5000000,15000000)) %>%
        group_by(main_sector) %>%
        mutate(sum_of_investment = sum(raised_amount_usd), count_of_investment = n()) %>%
        arrange(desc(count_of_investment))

# Data frame D3 for IND
D3 <- filter(master_frame_merged, country_code == "IND" & funding_round_type == "venture" & between(raised_amount_usd, 5000000,15000000)) %>%
        group_by(main_sector) %>%
        mutate(sum_of_investment = sum(raised_amount_usd), count_of_investment = n()) %>%
        arrange(desc(count_of_investment))

#####################################################################################################################
##########################################    Table 5.1
#####################################################################################################################

# 1. Total number of investments (count)
# USA - 
nrow(D1)
# GBR - 
nrow(D2)
# IND - 
nrow(D3)

# 2. Total amount of investment (USD)
# USA - 
sum(D1$raised_amount_usd)
# GBR - 
sum(D2$raised_amount_usd)
# IND - 
sum(D3$raised_amount_usd)

# 3. Top sector (based on count of investments)
# USA - 
select(D1, main_sector) %>%
  head(1)

# GBR - 
select(D2, main_sector) %>%
  head(1)

# IND - 
select(D3, main_sector) %>%
  head(1)

# 4. Second-best sector (based on count of investments)
# USA - 
select(D1, main_sector) %>%
  distinct(main_sector) %>%
  head(2)[,2]

# GBR - 
select(D2, main_sector) %>%
  distinct(main_sector) %>%
  head(2)[,2]

# IND - 
select(D3, main_sector) %>%
  distinct(main_sector) %>%
  head(2)[,2]

# 5. Third-best sector (based on count of investments)
# USA - 
select(D1, main_sector) %>%
  distinct(main_sector) %>%
  head(3)[,3]

# GBR - 
select(D2, main_sector) %>%
  distinct(main_sector) %>%
  head(3)[,3]

# IND - 
select(D3, main_sector) %>%
  distinct(main_sector) %>%
  head(3)[,3]

# 6. Number of investments in the top sector (refer to point 3)
# USA - 
select(D1, main_sector, count_of_investment) %>%
  head(1)[2,2]

# GBR - 
select(D2, main_sector, count_of_investment) %>%
  head(1)[2,2]

# IND - 
select(D3, main_sector, count_of_investment) %>%
  head(1)[2,2]

# 7. Number of investments in the second-best sector (refer to point 4)
# USA - 
select(D1, main_sector, count_of_investment) %>%
  distinct(count_of_investment) %>%
  head(2)[2,2]

# GBR - 
select(D2, main_sector, count_of_investment) %>%
  distinct(count_of_investment) %>%
  head(2)[2,2]

# IND - 
select(D3, main_sector, count_of_investment) %>%
  distinct(count_of_investment) %>%
  head(2)[2,2]

# 8. Number of investments in the third-best sector (refer to point 5)
# USA - 
select(D1, main_sector, count_of_investment) %>%
  distinct(count_of_investment) %>%
  head(3)[2,3]

# GBR - 
select(D2, main_sector, count_of_investment) %>%
  distinct(count_of_investment) %>%
  head(3)[2,3]

# IND - 
select(D3, main_sector, count_of_investment) %>%
  distinct(count_of_investment) %>%
  head(3)[2,3]

# 9. For the top sector count-wise (point 3), which company received the highest investment?
# USA - 
filter(D1,main_sector == "Others") %>%
  group_by(permalink, name) %>%
  summarize(investment_sum = sum(raised_amount_usd)) %>%
  arrange(desc(investment_sum)) %>%
  head(1)

# GBR - 
filter(D2,main_sector == "Others") %>%
  group_by(permalink, name) %>%
  summarize(investment_sum = sum(raised_amount_usd)) %>%
  arrange(desc(investment_sum)) %>%
  head(1)

# IND - 
filter(D3,main_sector == "Others") %>%
  group_by(permalink, name) %>%
  summarize(investment_sum = sum(raised_amount_usd)) %>%
  arrange(desc(investment_sum)) %>%
  head(1)


# 10. For the second-best sector count-wise (point 4), which company received the highest investment?
# USA - 
filter(D1,main_sector == "Social, Finance, Analytics, Advertising") %>%
  group_by(permalink, name) %>%
  summarize(investment_sum = sum(raised_amount_usd)) %>%
  arrange(desc(investment_sum)) %>%
  head(1)  

# GBR - 
filter(D2,main_sector == "Social, Finance, Analytics, Advertising") %>%
  group_by(permalink, name) %>%
  summarize(investment_sum = sum(raised_amount_usd)) %>%
  arrange(desc(investment_sum)) %>%
  head(1)  

# IND - 
filter(D3,main_sector == "Social, Finance, Analytics, Advertising") %>%
  group_by(permalink, name) %>%
  summarize(investment_sum = sum(raised_amount_usd)) %>%
  arrange(desc(investment_sum)) %>%
  head(1)  
