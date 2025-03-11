library(dplyr)
library(data.table)
library(countrycode)
library(haven)
library(purrr)
library(clipr)
library(ggplot2)
library(tidyverse)
library(stringr)


# https://dataweb.usitc.gov/trade/search/ForeignExp/HTS
# United States International Trade Commission
# data from bourbon sales 


# Download Date	February 22, 2025, 10:26 AM
# Step 1: Trade Flow and Classification System	
# Trade Flow	Domestic Exports
# Classification System	HTS Items
# Step 2: Data and Years	
# Data To Report	FAS Value, First Unit of Quantity, Second Unit of Quantity
# Data Format	1
# Select Type	Full Years
# Years	2024
# Timeframe Aggregation	Annual
# Step 3: Countries	
# Select Type	Use All Countries
# Country List	
# Country Aggregation	Aggregate Countries
# Step 4: Commodities	
# Select Type	Select Individual Commodities
# Commodity List	220830, 2208306020, 2208306040, 2208309020, 2208309025, 2208309030, 2208309040
# Commodity Aggregation Level	10
# Commodity Aggregation	Break Out Commodities
# Description Display	YES
# Step 5: Programs	
# Select Type	Use All Programs
# Import Program Aggregation	Aggregate CSC
# Step 6: Rate Provision Codes	
# Select Type	Use All Provision Codes
# Provision Code Aggregation	Aggregate RPCODE
# Step 7: Districts	
# Select Type	Use All Districts
# District List	
# District Aggregation	Break Out District
# Step 8: Report Layout	
# Column Order	DISTRICT, HTS10 & DESCRIPTION
# Column Sort Order	Districts


dt <- fread("states_export.csv")

head(dt)
# dist         id                                                                         desc        qdesc total
# <char>      <i64>                                                                       <char>       <char> <num>
#   1: Anchorage, AK 2208306020              WHISKIES, BOURBON, IN CONTAINERS EACH HOLDING NOT OVER 4 LITERS proof liters   416
# 2: Anchorage, AK 2208306040                  WHISKIES, BOURBON, IN CONTAINERS EACH HOLDING OVER 4 LITERS                  0
# 3: Anchorage, AK 2208309025    RYE WHISKIES EXCEPT BOURBON, IN CONTAINERS EACH HOLDING NOT OVER 4 LITERS proof liters   292
# 4: Anchorage, AK 2208309030 WHISKIES EXCEPT BOURBON, IN CONTAINERS EACH HOLDING NOT OVER 4 LITERS, NESOI                  0
# 5: Anchorage, AK 2208309040            WHISKIES EXCEPT BOURBON, IN CONTAINERS EACH HOLDING OVER 4 LITERS                  0
# 6: Baltimore, MD 2208306020              WHISKIES, BOURBON, IN CONTAINERS EACH HOLDING NOT OVER 4 LITERS                  0
> 
  

  
    
dt=dt %>% filter(total!=0)
dt[grep('EXCEPT BOURBON',desc),borbon:=1]
dt[is.na(borbon),borbon:=0]
dt[,.(stringr::str_remove_all(".*,",dist))]
dt[,.N,.(dist)]
dt[, dist:=str_extract(dist, "(?<=, ).*")]
dt=dt %>% filter(!is.na(dist)) %>%  select(abbr  =dist,total,borbon)
dt=dt %>%  group_by(abbr,borbon) %>% summarise(total=sum(total))
setDT(dt)
dt <- dcast(dt, abbr ~ borbon, value.var = "total", fill = 0)
setnames(dt, c("0", "1"), c("non_borbon", "borbon"))
dt=dt %>% left_join(usmap::citypop) %>% select(borbon,borbon,non_borbon,state,lon,lat) %>% filter(!is.na(lon  ))
borbon_total=sum(dt$borbon,rm.na=TRUE)
nonborbon_total=sum(dt$non_borbon,rm.na=TRUE)

dt[,share_borbon:=100*borbon/borbon_total]
dt[,share_nonborbon:=100*non_borbon/nonborbon_total]
dt[,share_total:=100*(borbon+non_borbon )/sum(borbon_total+nonborbon_total)]

## it's only for ports export
plot_usmap(data = dt, values = "share_borbon", regions = "states") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Bourbon %") +
  theme_minimal() +
  labs(title = "Bourbon Production in the U.S.", subtitle = "Kentucky dominates bourbon production")


plot_usmap(data = dt, values = "share_total", regions = "states") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Bourbon %") +
  theme_minimal() +
  labs(title = "Bourbon Production in the U.S.", subtitle = "Kentucky dominates bourbon production")

plot_usmap(data = dt, values = "non_borbon", regions = "states") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Bourbon %") +
  theme_minimal() +
  labs(title = "Bourbon Production in the U.S.", subtitle = "Kentucky dominates bourbon production")


# the data is only on ports and export


##########




# whiskey export share in 2023


# https://usatrade.census.gov/
# State Exports by HS Commodities
# value of export
# find main state produces whiskey around us


df=fread("st_exp3.csv") %>% 
  mutate(
    spirit=case_when(
      grepl('220820',comm)~"brandy",
      grepl('220830',comm)~"whiskie",
      grepl('220840',comm)~"rum",
      grepl('220850',comm)~"gin",
      grepl('220860',comm)~"vodka",
      grepl('220870|220890',comm)~"liq",
      grepl('2208 Ethyl Alcohol',comm)~"total",
      ),
    states=case_when(grepl('colombia',cntry)~'District of Columbia',TRUE~state),
    value = str_remove_all(value, ",") %>% as.numeric()
  ) %>% select(cntry,state,spirit,time,value) 

df$cntry
setDT(df)

df[, whiskey := as.numeric(comm == "220830 Whiskies")]
df <- df %>% 
  filter(whiskey == 1, time == 2024) %>% 
  select(value, state) %>% 
  mutate(value = str_remove_all(value, ",") %>% as.numeric())

total <- as.numeric(df[state == "All States", ]$value)
df <- df %>% mutate(share = 100 * value / total)
df[state == "Dist of Columbia", state := "District of Columbia"]

map_data <- df %>% 
  left_join(usmap::statepop %>% select(state = full, fips, abbr), by = "state") %>% 
  filter(!is.na(fips), state != "Alaska")


## production of whiskey
map_data <- map_data %>% mutate(
  category = factor(case_when(
    share > 50 ~ "Above 50%",
    share > 20 ~ "Above 20%",
    share > 3  ~ "Above 3%",
    TRUE       ~ "Under 3%"
  ), levels = c("Under 3%", "Above 3%", "Above 20%", "Above 50%"))
)

plot_usmap(data = map_data, values = "category", regions = "states",exclude = "AK", color = "white") +
  scale_fill_manual(
    values = c(
      "Under 3%"  = "#d4b9db",   # Medium pink
      "Above 3%"  = "#c994c8",   # Medium dark pink
      "Above 20%" = "#ce1256",   # Dark pink
      "Above 50%" = "#91003f"    # Very dark pink
    ),
    name = "Whiskey Share (%)",
    na.value = "lightgray"
  ) +
  theme(legend.position = "right")





#  from: https://dataweb.usitc.gov/
# change in Bourbon, vs other whiskey, vs other alcohold



df


dr=fread('export.csv') %>%  filter(year>2008) %>% mutate(alc=case_when(
  grepl("WHISKIES, BOURBON",desc)~"bourbon",
  #grepl("RYE WHISKIES EXCEPT BOURBON",desc)~"r",
  grepl("EXCEPT BOURBON",desc)~"other_whiskey",
  TRUE~"other_alcohol"
)) %>% group_by(year,alc) %>% summarise(total=sum(total)) %>% pivot_wider(names_from = alc,values_from = total)
dr
drt %>% fwrite('drt.csv')










# Load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read and transform data
dr[,.(.N),desc]

dr[grepl("TEQUILA",desc),.N,desc]
dr[grepl("VODKA",desc),.N,desc]
dr[grepl("LIQUEURS|KIRSCHWASSER",desc),.N,desc]
dr[grepl("KIRSCHWASSER",desc),.N,desc]
dr[grepl("GIN",desc),.N,desc]
dr[grepl("WHISKIES",desc),.N,desc]


fread('export.csv') %>%
  filter(year > 2014) %>%
  mutate(alc = case_when(
    grepl("WHISKIES, BOURBON", desc) ~ "Bourbon",
    grepl("TEQUILA", desc) ~ "teqila",
    grepl("VODKA", desc) ~ "vodka",
    grepl("LIQUEURS|KIRSCHWASSER|BRANDY", desc) ~ "liquer",
    grepl("GIN", desc) ~ "gin",
    grepl("RUM", desc) ~ "rum",
    
    grepl("EXCEPT BOURBON", desc) ~ "Other Whiskey",
    TRUE ~ "other"
  )) %>%
  group_by(year, alc) %>%
  summarise(total = sum(total)/1000000, .groups = "drop") %>%
  pivot_wider(names_from = alc, values_from = total) 

# Convert to long format for plotting
dr_long <- dr %>%
  pivot_longer(cols = -year, names_to = "Product", values_to = "ExportValue")

# Create academic chart
ggplot(dr_long, aes(x = year, y = ExportValue, color = Product)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    #title = "Domestic Exports: Bourbon vs. Other Whiskey vs. Other Alcohol (2009–2024)",
    x = "Year",
    y = "Export Value (FAS Value)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.direction = "vertical",
    
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

# Display the chart


# Alchol Sales, all countries, by fas value, for every month from -01/17 to 12-19
#
st[,.N,contry] %>% select(contry) %>% print(150) %>% clipr::write_clip() 
## what is the change in export by region (eu, canada, mexico,china) around 2018, by alcohol


st <- fread('All_distiled_alcohol_export.csv')

st=st %>% mutate(alc = case_when(
  grepl("WHISKIES, BOURBON", desc) ~ "bourbon",
  grepl("TEQUILA", desc) ~ "teqila",
  grepl("VODKA", desc) ~ "vodka",
  grepl("LIQUEURS|KIRSCHWASSER|BRANDY", desc) ~ "liquer",
  grepl("GIN", desc) ~ "gin",
  grepl("RUM", desc) ~ "rum",
  
  grepl("EXCEPT BOURBON", desc) ~ "whiskey_other",
  TRUE ~ "other"
)) %>%  select(contry,year,month,alc,value)

eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia (Czech Republic)", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain"
)
million=1000000
stu=st %>% mutate(region = case_when(
  contry %in% eu_countries ~ "EU",
  contry == "China" ~ "ch",
  contry == "Canada" ~ "ca",
  contry == "Mexico" ~ "me",
  TRUE ~ "other"
)) %>% select(region,year,month,alc,value) %>% mutate(value=value %>% as.numeric(na.rm=TRUE))%>%  group_by(alc,region,year,month) %>%
  summarise(value=sum(value)/million) %>%  filter(!is.na(year)) %>% 
  mutate(date =paste(year, month, "01", sep = "-") %>% as.Date())))

stu_eu <- stu %>%
  filter(region == "EU") %>%
  arrange(alc, date) %>%
  mutate(
    alc_gr=case_when(
      alc=='bourbon' ~ 'bourbon',
      alc=='whiskey_other' ~ 'whiskey_other',
      TRUE ~'other_alc'
                     )
  ) %>% group_by(alc_gr,region,date)  %>% summarise(value=sum(value))
  
  
ggplot(stu_eu %>%  filter(date>as.Date('01-01-2017')), aes(x = date, y = log(value), color = alc_gr)) +
  geom_line() +
  labs(title = "Monthly Percentage Change in Export Values by Alcohol Type in the EU",
       x = "Date",
       y = "Percentage Change (%)") +
  theme_minimal()



# 
# 
# 
# 
# 
# GATS Formula Calculations
# 
# The Global Agricultural Trade System (GATS) provides a variety of pre-defined calculation formulas to facilitate trend analysis.  These include:
#   
#   Growth Calculations
# 
# 1.       Period on Period Growth (P/P Growth %)
# 
# 2.       Period Growth
# 
# 3.       Compound Period Growth
# 
# 4.       Average Period Growth
# 
# 
# Growth Calculations
# 
# Growth Calculations can be utilized to analyze the percentage change in the value, quantity, or unit value of commodity trade over a period of time.  The period of time used in the calculation is determined by the time series selected, the date range, and the market year.
# 
# GATS provides three time series options:
#   
#   1.       Annual Series – The time period is one market year.
# 
# 2.       Monthly Series – The time period is one month.
# 
# 3.       Quarterly Series – The time period is three month increments within the market year.
# 
# The market year is determined by the market year start month and market year end month selected by the user and is a minimum of one month and a maximum of twelve months.  It should be noted that when using a Quarterly Series, the first quarter is defined as the first three month increment and the second quarter is the next three month increment.  If the market year is not evenly divisible by three, the remaining months are treated as the final quarter.
# 
# Back to the top
# 
# 1.  Period on Period Growth:
#   
#   Period on Period Growth (abbreviated P/P Growth %) calculates the percentage change in commodity value, quantity, and/or unit value across the most recent two periods selected in a query.
# 
# Period on Period Growth is calculated by subtracting the starting period value from the ending period value to determine the change, dividing this result by the starting period value and multiplying by 100 to give the percentage change:
#   
#   
#   
#   Example
# 
# 
# 
# Time Series: Annual
# 
# Market Year Start Month: October          Market Year End Month: September
# 
# Start Year: 2000                                                 End Year: 2003
# 
# 
# 
# Sample Data:
#   
#   Market Year (Oct-Sept)
# 
# 2000
# 
# 2001
# 
# 2002
# 
# 2003
# 
# P/P Growth %
# 
# Statistical Value
# 
# 800
# 
# 900
# 
# 1000
# 
# 1100
# 
# 10
# 
# 
# 
# 
# 
# Back to the top
# 
# 
# 
# 2.    Period Growth
# 
# Period Growth calculates the percentage change in commodity value, quantity, and/or unit value across the entire range of periods selected in a query.
# 
# Period Growth is calculated by subtracting the first statistical period value in the query from the ending period value to determine the change, dividing this result by the first period statistical value and multiplying by 100 to give the percentage change:
#   
#   
#   
#   
#   
#   Example
# 
# 
# 
# Time Series: Annual
# 
# Market Year Start Month: October          Market Year End Month: September
# 
# Start Year: 2000                                                 End Year: 2003
# 
# 
# 
# Sample Data:
#   
#   Market Year (Oct-Sept)
# 
# 2000
# 
# 2001
# 
# 2002
# 
# 2003
# 
# Period Growth
# 
# Statistical Value
# 
# 800
# 
# 900
# 
# 1000
# 
# 1100
# 
# 38
# 
# 
# 
# 
# 
# Back to the top
# 
# 
# 
# 3.    Compound Period Growth
# 
# Compound Period Growth calculates the period growth rate in commodity value, quantity, and/or unit value across the entire range of periods selected in a query if that growth rate were evenly distributed over the number of periods selected.
# 
# Compound Period Growth is calculated by taking the last statistical value in the selected range and dividing by the first statistical value in the range, raising that value to the reciprocal of the number of periods selected minus 1, subtracting 1 from that number and multiplying the result by 100:
#   
#   
#   
#   
#   
#   Example
# 
# 
# 
# Time Series: Annual
# 
# Market Year Start Month: October          Market Year End Month: September
# 
# Start Year: 2000                                                 End Year: 2003
# 
# 
# 
# Sample Data:
#   
#   Market Year (Oct-Sept)
# 
# 2000
# 
# 2001
# 
# 2002
# 
# 2003
# 
# Compound Period Growth
# 
# Statistical Value
# 
# 800
# 
# 900
# 
# 1000
# 
# 1100
# 
# 11
# 
# 
# 
# 
# 
# Back to the top
# 
# 
# 
# 4.    Average Period Growth
# 
# Average Period Growth calculates the period growth rate in commodity value, quantity, and/or unit value averaged across the entire range of periods selected in a query.
# 
# Average Period Growth is calculated by taking the natural log of the last statistical value in the selected range and subtracting the natural log of the first statistical value in the range, then dividing by the number of periods selected minus 1, and multiplying the result by 100:
#   
#   
#   
#   
#   
#   
#   
#   
#   Example
# 
# 
# 
# Time Series: Annual
# 
# Market Year Start Month: October          Market Year End Month: September
# 
# Start Year: 2000                                                 End Year: 2003
# 
# 
# 
# Sample Data:
#   
#   Market Year (Oct-Sept)
# 
# 2000
# 
# 2001
# 
# 2002
# 
# 2003
# 
# Average Period Growth
# 
# Statistical Value
# 
# 800
# 
# 900
# 
# 1000
# 
# 1100
# 
# 11
# 
# 
# 
# 
# 
# Back to the top
# 






dr_long <- dr %>%
  pivot_longer(
    cols = c(bourbon, other_whiskey, other_alcohol),
    names_to = "alcohol_type",
    values_to = "exports"
  ) %>%
  mutate(
    post_tariff = ifelse(year >= 2018, 1, 0),
    log_exports = log(exports + 1)  # Avoid log(0)
  )
