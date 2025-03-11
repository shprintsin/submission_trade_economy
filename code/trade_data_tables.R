# ====================================================
# Academic Article Report on U.S. Alcohol Market & 2018 Tariffs
# ====================================================

library(dplyr)
library(data.table)
library(countrycode)
library(usmap)
library(tidyr)

# -------------------------------
# Section 1: Worldwide Data Analysis
# -------------------------------

# Load worldwide trade data (aggregated for 2018, without state detail)
df_world <- fread('usatrade_census_worldwide_allstates.csv') %>% 
  mutate(value = as.numeric(gsub(',', '', value)),
         spirit = case_when(
           grepl('220830', com) ~ "whiskie",
           grepl('220860', com) ~ "vodka",
           grepl('220870|220890|220820', com) ~ "other",
           grepl('2208 Ethyl Alcohol', com) ~ "total",
           TRUE ~ "other"
         )) %>% 
  filter(spirit == 'total') %>% 
  select(country, value) %>% 
  data.table()

# Helper functions for percentages and formatting
prc <- function(x, y, d = 1){ round(100 * x / y, d) }
prnct <- function(x){ paste0("(", round(x), "%)") }
dollar <- function(x){ paste0(x, "Million $  ") }
share <- function(x){ paste0(round(x, 1), "%") }
million <- 10^6

# Compute total export value (in millions) from the "World Total" row
EXPORT_TOTAL <- df_world[country == 'World Total']$value /million

# Exclude the world total and extract the top 15 countries by market share
df_world_res <- df_world %>% 
  filter(country != 'World Total') %>% 
  mutate(value=round(value/million,1),share = prc(value, EXPORT_TOTAL)) %>% 
  arrange(-share) %>% 
  head(10)

# Extract values for key markets: United Kingdom, Canada, and the European Union
ALC_SHARE_UK <- df_world_res[country == "United Kingdom"]$share
ALC_SHARE_CA <- df_world_res[country == "Canada"]$share
VAL_UK <- df_world_res[country == "United Kingdom"]$value
VAL_CA <- df_world_res[country == "Canada"]$value
VAL_EU <- df_world_res[country == "European Union"]$value
VAL_MAJOR <- VAL_CA + VAL_EU + VAL_UK
SHARE_MAJOR <- prc(VAL_MAJOR, EXPORT_TOTAL)

# -------------------------------
# Section 2: U.S. State-Level Data Analysis (2017-2024)
# -------------------------------

# Load U.S. state-level data
df <- fread('usatrade_census_worldwide.csv')

# Data preparation: assign country codes, clean values, and classify spirit types.
dt <- df %>%
  mutate(
    foreign = case_when(
      country == 'Canada' ~ "CA",
      country == 'Mexico' ~ "ME",
      country == 'United Kingdom' ~ "UK",
      country == 'European Union' ~ "EU",
      country == 'World Total' ~ "all",
      TRUE ~ "Other"
    ),
    value = as.numeric(gsub(',', '', value)),
    spirit = case_when(
      grepl('220830', com) ~ "whiskie",
      grepl('220860', com) ~ "vodka",
      grepl('220870|220890|220820', com) ~ "other",
      grepl('2208 Ethyl Alcohol', com) ~ "total",
      TRUE ~ "other"
    ),
    state = ifelse(grepl('colombia', state, ignore.case = TRUE), 
                   'District of Columbia', state)
  ) %>%
  group_by(state, foreign, time, spirit) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  left_join(usmap::statepop, by = c("state" = "full")) %>%
  select(abbr, foreign, time, spirit, value) %>%
  filter(!is.na(abbr))

# Filter data for the yearly analysis (2017-2024)
dty <- dt %>% filter(time %in% as.character(2017:2024))
dtm <- dt %>% filter(!time %in% as.character(2017:2024))

# Calculate the total export value for U.S. whiskie (for EU) in 2018, in millions
EXPORT_TOTAL_W <- dty %>% 
  filter(foreign == 'all', time == '2018', spirit == 'whiskie') %>% 
  pull(value) %>% sum() / million

# -------------------------------
# Section 3: Analysis of the EU Market in 2018
# -------------------------------

# Focus on 2018 data for the European Union market
dty_2018 <- dty %>% 
  filter(time == "2018") %>% 
  mutate(total = value / million) %>% 
  filter(foreign == 'EU')

# Calculate state-level market shares for non-total spirit types (e.g., whiskie)
state_spirit_share <- dty_2018 %>% 
  filter(spirit != 'total') %>% 
  group_by(abbr, spirit) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(spirit) %>% 
  mutate(prc = 100 * value / sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(abbr, spirit, prc)

# Prepare a table specifically for whiskie production market shares by state
whisky <- dty_2018 %>% 
  filter(spirit == 'whiskie') %>%  
  mutate(total = round(value / million),
         prc = round(100 * value / sum(value), 2)) %>% 
  select(abbr, prc, total) %>% 
  arrange(desc(prc)) %>% 
  data.table()

# Total distilled alcohol exports (in millions) to the EU from 2018
TOTAL_ALC_EXPORT_eu <- dty_2018 %>% 
  filter(spirit == 'total') %>% 
  pull(total) %>% sum()

# -------------------------------
# Section 4: Tariff Loss Analysis (2018-2019)
# -------------------------------

# Calculate monthly loss differences between 2018 and 2019 (in millions)
stmv <- dtm %>% 
  filter(!is.na(abbr), foreign != 'ME') %>% 
  select(time, foreign, value, spirit) %>% 
  mutate(time = as.Date(paste0(time, "-01"), format = "%Y-%m-%d")) %>% 
  arrange(time) %>% 
  group_by(spirit, foreign, time) %>% 
  summarise(value = sum(value) / million, .groups = 'drop') %>% 
  mutate(year = as.numeric(format(time, "%Y"))) %>% 
  group_by(year, foreign, spirit) %>% 
  summarise(avg = mean(value), .groups = 'drop') %>% 
  filter(year %in% c(2018, 2019)) %>%  
  group_by(foreign, spirit) %>% 
  arrange(year) %>% 
  mutate(diff = avg - lag(avg)) %>% 
  filter(!is.na(diff))

loss_diff_table <- stmv %>% 
  select(foreign, spirit, diff) %>% 
  pivot_wider(names_from = foreign, values_from = diff)

# Calculate state-level losses for total production in the EU market (2018 vs. 2019)
state_spirit <- dty %>% 
  filter(foreign == 'EU', time %in% c("2018", "2019"))  %>% 
  group_by(abbr, time, spirit) %>% 
  summarise(value = sum(value) / million, .groups = 'drop') %>% 
  filter(spirit == 'total') %>% 
  arrange(time)

total_losses <- state_spirit %>% 
  group_by(abbr, time) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  group_by(abbr)  %>% 
  mutate(diff = value - lag(value)) %>% 
  ungroup() %>% 
  mutate(loss_precnt = diff / lag(value),
         burden = diff / sum(diff, na.rm = TRUE)) %>% 
  filter(!is.na(loss_precnt)) %>% 
  select(abbr, diff, loss_precnt, burden) %>% 
  arrange(-burden) %>% data.table()

# Extract loss metrics for key states: Tennessee (TN), Texas (TX), and Kentucky (KY)
TN_LOSS_PRCNTG <- round(-100 * total_losses[abbr == 'TN']$loss_precnt)
TN_BURDEN      <- round(total_losses[abbr == 'TN']$burden * 100)
TN_LOSS        <- -total_losses[abbr == 'TN']$diff

TX_LOSS_PRCNTG <- round(-100 * total_losses[abbr == 'TX']$loss_precnt)
TX_BURDEN      <- round(total_losses[abbr == 'TX']$burden * 100)
TX_LOSS        <- round(-total_losses[abbr == 'TX']$diff, 2)

KY_LOSS_PRCNTG <- round(-100 * total_losses[abbr == 'KY']$loss_precnt)
KY_BURDEN      <- round(total_losses[abbr == 'KY']$burden * 100)
KY_LOSS        <- round(-total_losses[abbr == 'KY']$diff, 2)

TOTAL_LOSS <- round(-sum(total_losses$diff), 2)
MAJOR_LOSSES <- TX_LOSS + KY_LOSS + TN_LOSS
MAJOR_LOSSES_BURDEN <- round(100 * MAJOR_LOSSES / TOTAL_LOSS)


# -------------------------------
# Section 5: Final Report (Academic Article Style)
# -------------------------------

report_text <- paste0(
  "In 2018, the U.S. distilled alcoholic beverage market experienced significant trade disruptions due to ",
  "the imposition of tariffs. Worldwide data indicate that U.S. distilled alcohol exports totaled ",
  dollar(EXPORT_TOTAL), " with major export markets including the European Union, Canada, and the United Kingdom. ",
  "Specifically, the combined exports to the EU, Canada, and the UK amounted to ", dollar(VAL_MAJOR), 
  ", representing ", share(SHARE_MAJOR), " of the total U.S. exports.\n\n",
  
  "Focusing on the European Union market in 2018, the analysis of state-level data reveals that U.S. whiskie exports ",
  "were a significant component of the total distilled alcohol shipments to the EU. The total whiskie production, calculated at ",
  dollar(sum(whisky$total)), "million dollars, accounted for ", share(prc(sum(whisky$total), TOTAL_ALC_EXPORT_eu)), 
  " of the EU market. Notably, key production states included Tennessee, Kentucky, and Texas. Tennessee alone produced ",
  VAL_W_TN <- if(exists("whisky") && "TN" %in% whisky$abbr) whisky[abbr == 'TN']$total else NA,
  " million dollars of whiskie, with a market share of ", if(!is.na(whisky[abbr=='TN']$prc)) prnct(whisky[abbr=='TN']$prc) else "NA", 
  ". Kentucky and Texas contributed ", 
  if("KY" %in% whisky$abbr) prnct(whisky[abbr=='KY']$prc) else "NA", " and ", 
  if("TX" %in% whisky$abbr) prnct(whisky[abbr=='TX']$prc) else "NA",
  " respectively. Together, these states accounted for ", dollar(VAL_W_ALL <- if(exists("whisky")) sum(whisky[abbr %in% c('TN','KY','TX')]$total) else NA),
  "million dollars or ", share(SHARE_W_ALL <- if(exists("whisky")) sum(whisky[abbr %in% c('TN','KY','TX')]$prc) else NA),
  " of the whiskie market in the EU.\n\n",
  
  "An analysis of tariff-induced losses between 2018 and 2019 further highlights the uneven impact across states. ",
  "Tennessee experienced a loss of approximately ", TN_LOSS, " million dollars, corresponding to a loss percentage of ", 
  prnct(TN_LOSS_PRCNTG), " and a burden of ", TN_BURDEN, "% of the total industry losses. Texas and Kentucky ",
  "also incurred losses of ", TX_LOSS, " and ", KY_LOSS, " million dollars respectively, with loss percentages of ",
  prnct(TX_LOSS_PRCNTG), " and ", prnct(KY_LOSS_PRCNTG), ". Collectively, Tennessee, Texas, and Kentucky accounted for a combined loss of ",
  dollar(MAJOR_LOSSES), "million dollars, representing ", share(MAJOR_LOSSES_BURDEN), " of the total losses in the industry.\n\n",
  
  "These findings underscore the fact that the European Union remains the largest market for U.S. distilled alcoholic beverages, ",
  "with a substantial portion of the market concentrated in key states. The impact of the 2018 tariffs was unevenly distributed, ",
  "with Tennessee bearing the highest burden, followed by Texas and Kentucky. Such insights are critical for understanding both the ",
  "economic and political implications of trade policies on the U.S. alcohol industry."
)

# Calculate HHI for each spirit type
hhi_results <- dty %>%
  filter(time == "2018") %>%
  group_by(abbr, spirit) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() %>%
  group_by(spirit) %>%
  mutate(market_share = value / sum(value)) %>%
  summarise(HHI = sum(market_share^2) * 10000, .groups = 'drop')

hhi_results %>% setDT()
HHI_VODKA=hhi_results[spirit=='vodka']
HHI_TOTAL=hhi_results[spirit=='total']
HHI_OTHER=hhi_results[spirit=='other']
HHI_WHISKIE=hhi_results[spirit=='whiskie']
p(
  "In terms of market concentration, the distilled spirits industry exhibits varying levels of concentration across different product categories. 
  The Herfindahl-Hirschman Index (HHI), a standard measure of market concentration, reveals that the whiskey industry is the most concentrated, 
  with an HHI of", HHI_WHISKIE, ", followed by vodka at", HHI_VODKA, ". 

  The overall distilled spirits market has an HHI of", HHI_TOTAL, ", indicating moderate concentration. 
  Meanwhile, the category labeled 'other' spirits, including brandy and liqueurs, has the lowest concentration, with an HHI of", HHI_OTHER, ", 
  suggesting a more competitive market. 

  These findings suggest that whiskey and vodka production are dominated by a few states, while other spirits are produced across a wider 
  geographic distribution."
)




# ====================================================
# Table 1: State-Level Whiskie Market Shares in the EU (2018)
# ====================================================
# dty_2018 is assumed to be already created from the cleaned data:
#   dty_2018 <- dty %>% filter(time == "2018") %>% mutate(total = value/10^6) %>% filter(foreign == 'EU')

dty_2018 %>% 
  filter(spirit == 'whiskie') %>%  
  left_join(usmap::statepop %>% select(abbr,full), by = "abbr") %>% 
  
  mutate(total_million = round(value / million, 2),
         market_share = round(100 * value / sum(value), 2)) %>% 
  select(State = full, `Production (Million $)` = total_million, `Market Share (%)` = market_share) %>% 
  arrange(desc(`Market Share (%)`)) %>% head(5)


# Render Table 1 using kableExtra

whisky_df <- dty_2018 %>% 
  filter(spirit == "whiskie") %>%  
  left_join(usmap::statepop %>% select(abbr, full), by = "abbr") %>% 
  mutate(total_million = round(value / million, 2),
         MarketShare = round(100 * value / sum(value), 2)) %>% 
  select(State = full, Production = total_million, MarketShare) %>% 
  arrange(desc(MarketShare))

# Extract top 5 states
top5 <- whisky_df %>% head(5)

# Aggregate the remaining states into an "Other" row
others <- whisky_df %>% slice((6):n())
total_production_all <- sum(whisky_df$Production)
others_summary <- others %>% 
  summarise(
    State = "Other",
    Production = round(sum(Production), 2),
    MarketShare = round(100 * sum(Production) / total_production_all, 2)
  )

final_table <- dty_2018 %>% 
  filter(spirit == "whiskie") %>%  
  left_join(usmap::statepop %>% select(abbr, full), by = "abbr") %>% 
  mutate(total_million = round(value / million, 2),
         MarketShare = round(100 * value / sum(value), 2)) %>% 
  select(State = full, Production = total_million, MarketShare) %>% 
  arrange(desc(MarketShare)) %>% 
  { 
    # Save the entire table as . (dot)
    top5 <- head(., 5)
    others <- slice(., 6:n())
    total_production_all <- sum(.$Production)
    others_summary <- others %>% 
      summarise(State = "Other", 
                Production = round(sum(Production), 2),
                MarketShare = round(100 * sum(Production) / total_production_all, 2))
    total_summary <- summarise(., 
                               State = "Total", 
                               Production = round(sum(Production), 2),
                               MarketShare = 100)
    bind_rows(top5, others_summary, total_summary)
  }



# Create a "Total" row summarizing overall production and 100% market share
total_summary <- whisky_df %>% 
  summarise(
    State = "Total",
    Production = round(sum(Production), 2),
    MarketShare = 100
  )



# ====================================================
# Table 2: Tariff Losses by State in the EU (2018 vs. 2019)
# ====================================================
# Calculate state-level losses for 'total' production between 2018 and 2019.
# (Assuming dty contains data for years 2018 and 2019 for foreign == 'EU')
state_spirit <- dty %>% 
  filter(foreign == 'EU', time %in% c("2018", "2019"))  %>% 
  group_by(abbr, time, spirit) %>% 
  summarise(value = sum(value, na.rm = TRUE) / 10^6, .groups = 'drop') %>% 
  filter(spirit == 'total') %>% 
  arrange(time)




res= state_spirit %>%
  filter(time %in% c("2018", "2019"), spirit == "total") %>% 
  group_by(abbr, time) %>% 
  summarise(total = sum(value, na.rm = TRUE) , .groups = "drop") %>% 
  group_by(abbr) %>% 
  mutate(diff = total - lag(total)) %>% 
  ungroup() %>% 
  mutate(Loss_Percent = round(100 * diff / lag(total), 1),
         Burden_Percent = round(100 * diff / sum(diff, na.rm = TRUE), 1)) %>% 
  arrange(desc(Burden_Percent))


total_loss <- sum(res$diff,na.rm = TRUE)
top_states <- head(res%>%filter(!is.na(Loss_Percent)), 5) 
others <- slice(res, 6:n())

others=res %>% filter(!abbr %in% top_states$abbr ) %>% group_by(time)%>% summarise(total=sum(total)) %>% data.table()
others_diff=others[time==2019]$total-others[time==2018]$total
others_burden=-prc(others_diff,-total_loss)
other_loss_per=prc(others_diff,others[time==2018]$total)
other_line=data.table(State="Other",diff=others_diff,loss_per=other_loss_per,burden=others_burden)
table_2=top_states %>% left_join(usmap::statepop %>% select(abbr, full), by = "abbr") %>%
  mutate(State = case_when(
    abbr %in% c("other", "Total") ~ abbr,
    TRUE ~ full
  )) %>% select(State, `Loss (Million $)` = diff, `Loss (%)` = Loss_Percent, `Burden (%)` = Burden_Percent) %>%  
  data.table() %>%rbind(other_line,use.names=FALSE) 


# Render Table 2 using kableExtra

# ====================================================
# Narrative Text Describing the Tables
# ====================================================


table_3=df_world_res %>%
  mutate(Value = round(value / 10^6, 1), Share = share) %>%
  select(Country = country, Value, Share) %>%
  arrange(-Value)

table_3=df_world_res 

table4a=  dty_2018 %>%
  group_by(spirit) %>%
  summarise(Value = sum(value) / 10^6, .groups = 'drop') %>%
  mutate(Share = 100 * Value / sum(Value)) %>%
  arrange(-Value)

table4b=dty %>%
  filter(time == "2018") %>%
  group_by(abbr, spirit) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() %>%
  group_by(spirit) %>%
  mutate(market_share = value / sum(value)) %>%
  summarise(HHI = sum(market_share^2) * 10000, .groups = 'drop')

table4=table4a%>% left_join(table4b) %>% arrange(-HHI)

# pre and post
table5=dty %>%
  filter(foreign %in% c('EU', 'CA', 'UK'), spirit == 'total') %>%
  group_by(time, foreign) %>%
  summarise(Value = sum(value) / 10^6, .groups = 'drop') %>%
  pivot_wider(names_from = foreign, values_from = Value) %>%
  rename(Year = time, `EU` = EU, `Canada` = CA, `UK` = UK)

# state export value first 10
table6=dt %>%
  filter(time == "2018", foreign == 'all', spirit == 'total') %>%
  group_by(abbr) %>%
  summarise(Value = sum(value) / million, .groups = 'drop') %>%
  mutate(Share = 100 * Value / sum(Value)) %>%
  left_join(usmap::statepop, by = "abbr") %>%
  select(State = full, Value, Share) %>% arrange(-Value) %>% 
  head(9)
table6

####################################################


md=state_spirit %>% group_by(abbr) %>% reframe(share=100*value/sum(state_spirit$value)) %>% arrange(share)
## production of whiskey
map_data=md %>% mutate(
  category = factor(case_when(
    share > 50 ~ "Above 50%",
    share > 20 ~ "Above 20%",
    share > 15 ~ "Above 15%",
    share > 10 ~ "Above 10%",
    share > 3  ~ "Above 3%",
    TRUE       ~ "Under 3%"
  ))
) %>% left_join(usmap::statepop %>% select(state = full, fips, abbr), by = "abbr")%>% 
  filter(!is.na(fips), state != "Alaska")


plot_usmap(data = map_data, values = "category", regions = "states",exclude = "AK", color = "white") +
  scale_fill_manual(
    values = c(
      "Under 3%"  = "#d4b9db",   # Medium pink
      "Above 3%"  = "#c994c8",   # Medium dark pink
      "Above 20%" = "#ce1256",   # Dark pink
      "Above 10%" = "#ce1256",   # Dark pink
      "Above 50%" = "#91003f"    # Very dark pink
    ),
    name = "Whiskey Share (%)",
    na.value = "lightgray"
  ) +
  theme(legend.position = "right")



library(usmap)
library(ggplot2)
library(dplyr)

# Compute whiskey share
md = state_spirit %>% 
  group_by(abbr) %>% 
  reframe(share = 100 * value / sum(state_spirit$value)) %>% 
  arrange(share)

# Join with state data
map_data = md %>%
  left_join(usmap::statepop %>% select(state = full, fips, abbr), by = "abbr") %>% 
  filter(!is.na(fips), state != "Alaska")

# Define thresholds and corresponding colors
thresholds <- c(3, 10, 15, 20, 50)  # Range of values
colors <- c("lightgray", "#c994c8", "#ce1256",  "#91003f")  # Your color scale

# Plot using a custom color gradient
plot_usmap(data = map_data, values = "share", regions = "states", exclude = "AK", color = "white") +
  scale_fill_gradientn(
    colors = colors,
    values = scales::rescale(thresholds),  # Rescale values to fit gradient mapping
    name = "Whiskey Share (%)",
    na.value = "gray"
  ) +
  theme(legend.position = "right")




kableExtra::kableExtra_latex_packages()


library(dplyr)
library(kableExtra)

# Function to round all numeric columns in a dataframe to 2 decimal places
round_df <- function(df, digits = 2) {
  df %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

# Define table captions and labels, rounding values before rendering
tables <- list(
  list(data = round_df(final_table), caption = "State-Level Whiskie Market Shares in the EU (2018)", label = "table1"),
  list(data = round_df(table_2), caption = "Tariff Losses by State in the EU (2018 vs. 2019)", label = "table2"),
  list(data = round_df(table_3), caption = "Top 10 Export Destinations for U.S. Alcohol (2018)", label = "table3"),
  list(data = round_df(table4), caption = "Market Share and HHI by Alcohol Type (2018)", label = "table4"),
  list(data = round_df(table5), caption = "U.S. Alcohol Exports by Major Market (2017-2024)", label = "table5"),
  list(data = round_df(table6), caption = "Top 10 U.S. States by Alcohol Export Value (2018)", label = "table6")
)

# Generate LaTeX output for each table
latex_tables <- lapply(tables, function(tbl) {
  kable(tbl$data, caption = tbl$caption, label = tbl$label, format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position"), full_width = FALSE) %>%
    footnote(general = "Source: Our analysis based on USA trade census data.")
})

# Combine all LaTeX tables into one string
latex_output <- paste(latex_tables, collapse = "\n\n")

# Save LaTeX tables to a file
writeLines(latex_output, "tables.tex")

# Print output to console
cat(latex_output)

# Detailed log of all variables and analysis results
# This will create a comprehensive log file with all calculated values

# Create a function to log variables
log_file <- "us_alcohol_market_analysis_log.txt"
p <- function(fmt, ..., log_file = "us_alcohol_market_analysis_log.txt") {  cat(sprintf(fmt, ...),'\n', file = log_file, append = TRUE)}
co <- function(x, log_file = "us_alcohol_market_analysis_log.txt") {
  p('\n')
  capture.output(print(x, nrow = nrow(x)),file=log_file,append = TRUE,split = TRUE)
  
}
x=table_2
co(table_2)
# Start a new log file
p("==========================================")
p("DETAILED ANALYSIS LOG - U.S. ALCOHOL MARKET & 2018 TARIFFS")
p("==========================================")
# Log date and time
p(paste("Log created on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# Section 1: Worldwide Data Analysis
p("SECTION 1: WORLDWIDE DATA ANALYSIS")
p("------------------------------------------")

# Log total export value
p("EXPORT_TOTAL: $%s", format(EXPORT_TOTAL, big.mark = ","))

# Log top 10 countries by market share
p("Top 10 Countries by Market Share:")
print_df <- df_world_res %>% 
  select(Country = country, `Value (Million $)` = value, `Share (%)` = share) %>%
  arrange(desc(`Share (%)`))
co(print_df)
# Log key market values
p("Key Market Values:")
p("ALC_SHARE_UK: %.1f%%", ALC_SHARE_UK)
p("ALC_SHARE_CA: %.1f%%", ALC_SHARE_CA)
p("VAL_UK: $%.1f million", VAL_UK)
p("VAL_CA: $%.1f million", VAL_CA)
p("VAL_EU: $%.1f million", VAL_EU)
p("VAL_MAJOR (CA+EU+UK): $%.1f million", VAL_MAJOR)
p("SHARE_MAJOR: %.1f%%", SHARE_MAJOR)

# Section 2: U.S. State-Level Data Analysis
p("SECTION 2: U.S. STATE-LEVEL DATA ANALYSIS (2017-2024)")
p("------------------------------------------")

# Log total export value for U.S. whiskie in 2018
p("EXPORT_TOTAL_W (U.S. Whiskie 2018): $%.1f million", EXPORT_TOTAL_W)

# Section 3: Analysis of the EU Market in 2018
p("SECTION 3: ANALYSIS OF THE EU MARKET IN 2018")
p("------------------------------------------")

# Log total distilled alcohol exports to the EU from 2018
p("TOTAL_ALC_EXPORT_eu: $%.1f million", TOTAL_ALC_EXPORT_eu)

# Log whisky state market shares
p("Whisky State Market Shares:")
print_df <- whisky %>% 
  select(State = abbr, `Total (Million $)` = total, `Market Share (%)` = prc) %>%
  arrange(desc(`Market Share (%)`))
co(print_df)
# Log the final whisky table
p("Final Whisky Table (Top 5 + Other + Total):")
co(data.table(final_table))
# Section 4: Tariff Loss Analysis
p("\nSECTION 4: TARIFF LOSS ANALYSIS (2018-2019)")
p("------------------------------------------")

# Log loss difference table
p("Loss Difference Table (2018 vs 2019):")
co(data.table(loss_diff_table))
# Log total losses
p("Total Losses:")

options(scipen = 999)

print_df=total_losses %>% 
  arrange(-burden) %>% mutate(diff=round(diff,2),burden=100*round(burden,3),loss_precnt=100*round(loss_precnt,3)) %>%
  select(State = abbr, `Loss (Million $)` = diff, `Loss (%)` = loss_precnt, `Burden (%)` = burden) %>% data.table() 
co(print_df)

# Log key state losses
p("Key State Losses:")
p("TN_LOSS_PRCNTG: %d%%", TN_LOSS_PRCNTG)
p("TN_BURDEN: %d%%", TN_BURDEN)
p("TN_LOSS: $%.2f million", TN_LOSS)

p("TX_LOSS_PRCNTG: %d%%", TX_LOSS_PRCNTG)
p("TX_BURDEN: %d%%", TX_BURDEN)
p("TX_LOSS: $%.2f million", TX_LOSS)

p("KY_LOSS_PRCNTG: %d%%", KY_LOSS_PRCNTG)
p("KY_BURDEN: %d%%", KY_BURDEN)
p("KY_LOSS: $%.2f million", KY_LOSS)

p("TOTAL_LOSS: $%.2f million", TOTAL_LOSS)
p("MAJOR_LOSSES (TN+TX+KY): $%.2f million", MAJOR_LOSSES)
p("MAJOR_LOSSES_BURDEN: %d%%", MAJOR_LOSSES_BURDEN)

# Section 5: HHI Analysis 
p("SECTION 5: MARKET CONCENTRATION (HHI) ANALYSIS")
p("------------------------------------------")

# Log HHI results
p("HHI Results by Spirit Type:")
co(hhi_results)

# Extract specific HHI values if available
# p("HHI_WHISKIE: %.0f", HHI_WHISKIE)
# p("HHI_VODKA: %.0f", HHI_VODKA)
# p("HHI_TOTAL: %.0f", HHI_TOTAL)
# p("HHI_OTHER: %.0f", HHI_OTHER)
# 

# Log additional tables
p("ADDITIONAL TABLES")
p("------------------------------------------")
p("Table 3 (Top Export Destinations):")
co(table_3)
p("Table 4 (Spirit Types and HHI):")
co(data.table(table4))
p("Table 5 (Pre and Post Tariff by Country):")
co(data.table(table5))
p("Table 6 (Top State Export Values):")
co(data.table(table6))


# End of log
p("==========================================")
p("END OF LOG")
p("==========================================")
# Return the log file name

p("==========================================")
p("END OF LOG")
p("==========================================")
# Return the log file name

# Print confirmation message
p(paste("Detailed log file has been created:", log_file, ""))
p("The log contains all calculated variables and tables from the analysis.")

# Additionally, create a comprehensive environment dump
# If you want to print all variables to console as well
p("Printing all variables to console:")
# Get all variables in the environment
all_vars <- ls()
for (var_name in all_vars) {
  var_value <- get(var_name)
  # Only print simple variables (not functions or complex objects)
  if (!is.function(var_value) && !is.environment(var_value)) {
    if (is.numeric(var_value) && length(var_value) == 1) {
      p("%s: %s", var_name, format(var_value, scientific = FALSE))
    } else if (is.character(var_value) && length(var_value) == 1) {
      p("%s: %s", var_name, var_value)
    } else if (is.vector(var_value) && length(var_value) < 10) {
      p("%s: ", var_name)
      p(paste(var_value, collapse = ", "), "")
    } else {
      p("%s: [%s with %d elements]", var_name, class(var_value)[1], length(var_value))
    }
  }
}


