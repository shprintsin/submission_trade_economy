# Revised and structured R code for clarity, efficiency, and to answer key questions clearly.


library(ggplot2)
library(data.table)
library(countrycode)
library(usmap)
library(tidyr)

# Load data
df <- fread('usatrade_census_worldwide.csv')
# df_world = filtered for 2018 only(no month) no states, gives info over the all countries. 
df_world= fread('usatrade_census_worldwide_allstates.csv')%>% mutate(value = as.numeric(gsub(',', '', value)),
              spirit = case_when(
                grepl('220830', com) ~ "whiskie",
                grepl('220860', com) ~ "vodka",
                # Liquer,brandy,whiskey
                grepl('220870|220890|220820', com) ~ "other",
                grepl('2208 Ethyl Alcohol', com) ~ "total",
                TRUE ~ "other"
              ))  %>% filter(spirit=='total') %>% select(country,value) %>% data.table()
EXPORT_TOTAL=df_world[country=='World Total']$value/million
df_world_res=df_world %>% filter(country!='World Total') %>% mutate(share=prc(value,EXPORT_TOTAL),value=value) %>% arrange(-share) %>% head(15)

ALC_SHARE_UK=df_world_res[country=="United Kingdom"]$share
ALC_SHARE_CA=df_world_res[country=="Canada"]$share

VAL_UK=df_world_res[country=="United Kingdom"]$value
VAL_CA=df_world_res[country=="Canada"]$value
VAL_EU=df_world_res[country=="European Union"]$value
VAL_MAJOR=VAL_CA+VAL_EU+VAL_UK
SHARE_MAJOR=VAL_MAJOR/EXPORT_TOTAL
df_world %>% mutate()


df[,.N,conunty]
# Data preparation
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
      # Liquer,brandy,whiskey
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



EXPORT_TOTAL_W=dt %>% filter(foreign=='all'&time=='2018',spirit=='whiskie') %>% pull(value) %>% sum()/million

##################
# Filter yearly data (2017-2024)
dty <- dt %>%  filter(time %in% as.character(2017:2024))
dtm <- dt %>%  filter(!time %in% as.character(2017:2024))

# What is the share of each state of whiskey production?
prc=function(x,y,d=1){
  res=round(100*x/y,d)
  return(res)
}


prnct=function(x){return(paste0('(',round(x),'%)'))}
dollar=function(x){return(paste0(x,'million$',' '))}
share=function(x){return(paste0(' ',round(x,1),'%',' '))}

dty_2018=dty %>% filter(time == "2018") %>% mutate(total=value/million) %>% filter(foreign=='EU')
whysky=dty_2018 %>% filter(spirit=='whiskie') %>%  mutate(total=round(value/million),prc=100*value/sum(value) %>% round(2)) %>% select(abbr,prc,total) %>% arrange(-prc) %>% data.table()
vodka=dty_2018 %>% filter(spirit=='vodka') %>%  mutate(total=round(value/million),prc=100*value/sum(value) %>% round(2)) %>% select(abbr,prc,total) %>% arrange(-prc) %>% data.table()
other=dty_2018 %>% filter(spirit=='other') %>%  mutate(total=round(value/million),prc=100*value/sum(value) %>% round(2)) %>% select(abbr,prc,total) %>% arrange(-prc) %>% data.table()
total=dty_2018 %>% filter(spirit=='total') %>%  mutate(total=round(value/million),prc=100*value/sum(value) %>% round(2)) %>% select(abbr,prc,total) %>% arrange(-prc) %>% data.table()



TOTAL_ALC_EXPORT_eu=dty_2018 %>% filter(spirit=='total') %>% pull(total) %>% sum()


EXPORT_EU_SHARE=prc(TOTAL_ALC_EXPORT_eu,EXPORT_TOTAL)
SHARE_W= prc(VAL_W,TOTAL_ALC_EXPORT_eu)
SHARE_V_KT=vodka[2]$prc
SHARE_V_TX=vodka[3]$prc
SHARE_O=prc(VAL_O,TOTAL_ALC_EXPORT_eu)
SHARE_O_KT=prc(VAL_O_KT,VAL_O)
SHARE_T_TN=total[1]$prc
SHARE_T_KT=total[2]$prc
SHARE_T_ALL=SHARE_T_KT+SHARE_T_TN
SHARE_W_TN=whysky[1]$prc
SHARE_W_KT=whysky[2]$prc
SHARE_W_TX=whysky[3]$prc
SHARE_V_TN=vodka[1]$prc
SHARE_W_ALL=SHARE_W_KT+SHARE_W_TN+SHARE_W_TX
SHARE_V_ALL=SHARE_V_KT+SHARE_V_TN+SHARE_V_TX

VAL_W=sum(whysky$total)
VAL_W_TN=whysky[1]$total
VAL_W_KT=whysky[2]$total
VAL_W_TX=whysky[3]$total
VAL_V_TN=vodka[1]$total
VAL_V_KT=vodka[2]$total
VAL_V_TX=vodka[3]$total
VAL_W_ALL=VAL_W_KT+VAL_W_KT+VAL_W_TX
VAL_V_ALL=VAL_V_KT+VAL_V_KT+VAL_V_TX
VAL_O=sum(other$total)
VAL_O_KT=other[abbr=='KY']$total
VAL_T_TN=total[1]$total
VAL_T_KT=total[2]$total
VAL_T_ALL=VAL_T_KT+VAL_T_KT
cat(
'the EU is the largest market for distiles Alcoholic bevarges with',dollar(TOTAL_ALC_EXPORT_eu),'which accounts for',share(EXPORT_EU_SHARE),
    'the other partners are canada and the UK with',share(ALC_SHARE_CA),',',share(ALC_SHARE_UK),'each. the total account for those countries is market of ',dollar(VAL_MAJOR),
'',

  "Whisky sales value is almost", dollar(VAL_W),
  "and accounts for", share(SHARE_W), "of total distilled alcohol sales to the EU. ",
  "Most of the whiskey industry is concentrated in Tennessee, accounting for",
  VAL_W_TN, prnct(SHARE_W_TN), "of the total industry. ",
  "The next players in that market are Texas and Kentucky with whiskey production of",
  VAL_W_KT, prnct(SHARE_W_KT), "and", VAL_W_TX, prnct(SHARE_W_TX),
  "respectively, which together account for a total of", dollar(VAL_W_ALL),
  "or", share(SHARE_W_ALL), "of the industry."
,'',
  'the second market is the vodka market, where tenasee producess, almost all in values of',dollar(VAL_V_TN),'which is almost',share(SHARE_V_TN),'of the entire vodka industry'
,'',
  'all other alchold industry,as LIquer, Gin, Brandy etc, account for',dollar(VAL_O),prnct(SHARE_O),
  'Where Kentucky is the dominant producer of about',share(SHARE_O_KT), 'of all '
,'',
  'in total, Tennucy and Kentucky responsible for',share(SHARE_T_ALL),'of entire distiles alcohold market to the EU. with production values of',
  dollar(VAL_T_TN),'and',dollar(VAL_T_KT),'each'
)

# What is the share of each state of total production?  


      
    
# 2018-2019 losses - in million dollars
  
million=10^6    
dtm %>% filter(!is.na(abbr))
stmv=dtm %>% filter(!is.na(abbr),foreign!='ME') %>% select(time,foreign,value,spirit)  %>% 
  mutate(time=time %>% paste0('-01') %>% as.Date(format = "%b-%y-%d")) %>% arrange(time) %>% 
    group_by(spirit,foreign,time) %>% summarise(value=sum(value)/million) %>% 
  mutate(year=year(time)) %>% group_by(year,foreign,spirit) %>% summarise(avg=mean(value)) %>% 
  filter(year %in% c(2018,2019)) %>%  group_by(foreign,spirit) %>% arrange(year) %>% mutate(diff=avg-lag(avg)) %>% filter(!is.na(diff))

stmv %>% select(foreign,spirit,diff) %>% pivot_wider(names_from = foreign,values_from = diff)
stmv %>%  filter(spirit=='total')%>% group_by(foreign) %>% mutate(avg-lag(avg))
  pivot_wider(names_from = 'year',values_from = avg)
#export (in millions)

state_spirit=dty %>% filter(foreign=='EU') %>% mutate(time=as.numeric(time)) %>% filter(time %in% c('2018','2019'))  %>% group_by(abbr,time,spirit) %>% summarise(value=sum(value)/million) %>% arrange(-time)
total_losses=state_spirit%>% filter(spirit=='total') %>% select(-spirit) %>% group_by(abbr,time) %>% summarise(value=sum(value)) %>% ungroup() %>% group_by(abbr)  %>% mutate(diff=value-lag(value)) %>% ungroup() %>% mutate(diffp=diff/lag(value),diffr=diff/sum(diff,na.rm=TRUE)) %>% filter(!is.na(diffp)) %>% select(abbr,diff,loss_precnt=diffp,burden=diffr) %>%  arrange(-burden)
total_losses %>% setDT()
total_losses[abbr=='TN']
# Calculate metrics for each state
TN_LOSS_PRCNTG <- round(-100 * total_losses[abbr == 'TN']$loss_precnt)
TN_BURDEN      <- round(total_losses[abbr == 'TN']$burden * 100)
TN_LOSS        <- -total_losses[abbr == 'TN']$diff

TX_LOSS_PRCNTG <- round(-100 * total_losses[abbr == 'TX']$loss_precnt)
TX_BURDEN      <- round(total_losses[abbr == 'TX']$burden * 100)
TX_LOSS        <- round(-total_losses[abbr == 'TX']$diff,2)

KY_LOSS_PRCNTG <- round(-100 * total_losses[abbr == 'KY']$loss_precnt)
KY_BURDEN      <- round(total_losses[abbr == 'KY']$burden * 100)
KY_LOSS        <- round(-total_losses[abbr == 'KY']$diff,2)

TOTAL_LOSS          <- round(-sum(total_losses$diff),2)
MAJOR_LOSSES        <- TX_LOSS + KY_LOSS + TN_LOSS
MAJOR_LOSSES_BURDEN <- round(100 * MAJOR_LOSSES / TOTAL_LOSS)

# Rewrite the paragraph with corrected values and formatting
cat("Most of the burden fell on Tennessee, which carried almost ", TN_BURDEN,
    "% of total losses during that year. ",
    "Between 2018-2019, Tennessee lost almost $", TN_LOSS,
    ", which represents a loss of ", TN_LOSS_PRCNTG, "%. ",
    "The second state affected was Texas, which also experienced losses, but they were much smaller: $", TX_LOSS,
    " (a loss of ", TX_LOSS_PRCNTG, "% relative to 2018). Texas's burden accounts for only ", TX_BURDEN,
    "% of total tariffs loss, compared to a total loss of $", TOTAL_LOSS, ". ",
    "Surprisingly, the industry in Kentucky saw a relatively small impact with only $", KY_LOSS,
    " in losses, which is ", KY_LOSS_PRCNTG, "% and accounts for ", KY_BURDEN,
    "% of the total burden. ",
    "Together, Tennessee, Texas, and Kentucky lost $", MAJOR_LOSSES,
    ", which accounts for ", MAJOR_LOSSES_BURDEN, "% of the total losses in the industry.")




state_spirit %>% filter(abbr=='TX')
state_spirit%>% filter(spirit=='whiskie') %>% select(-spirit) %>% group_by(abbr,time) %>% summarise(value=sum(value)) %>% ungroup() %>% group_by(abbr)  %>% mutate(diff=value-lag(value)) %>% ungroup() %>% mutate(diffp=diff/lag(value),diffr=diff/sum(diff,na.rm=TRUE)) %>% filter(!is.na(diffp)) %>% select(abbr,diff,loss_precnt=diffp,burden=diffr) %>%  arrange(-burden)
library(kableExtra)
kableExtra::kable(state_spirit)

state_spirit_diff %>% filter(spirit=='whiskie')  %>% mutate(prcnt_loss=diff/sum(diff)) %>% select(abbr,spirit,prcnt_loss) %>% arrange(-prcnt_loss)
%>% pivot_wider(names_from='spirit',values_from='prcnt_loss')
losses %>%   
# 'who was hit the mosr

  
  ggplot(
  stmv %>% filter(
    time > as.Date("01-08-2017", format = "%d-%m-%Y"),
    time < as.Date("01-04-2019", format = "%d-%m-%Y")
    ) 
  
  ,
       aes(x = time, y = value, color = foreign)) +
  geom_line() +
  
  # geom_point() +
  labs(title = "Change in Value Over Time",
       x = "Time",
       y = "Value",
       color = "Region") +
  theme_minimal()


    
  mutate(market_share = value / sum(value, na.rm = TRUE) * 100) %>% ungroup() %>% mutate(state_group = if_else(market_share < 10, 'other', abbr))
%>% group_by(state_group, spirit) %>% summarise(total_market_share = sum(market_share),
                                                total_value = sum(value), .groups = 'drop') %>% select(state_group, spirit, total_market_share) %>% filter(spirit!='total')

dty %>%
  filter(time == "2018",spirit=='whiskie') %>%
  group_by(abbr, spirit) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  group_by(spirit) %>%
  mutate(market_share = value / sum(value, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(state_group = if_else(market_share < 10, 'other', abbr)) %>%
  group_by(state_group, spirit) %>%
  summarise(total_market_share = sum(market_share),
            total_value = sum(value),
            .groups = 'drop') %>%
  select(state_group, spirit, total_market_share) %>% 
  filter(spirit!='total')

# Answering key questions
res = market_share_table %>%  pivot_wider(names_from = spirit, values_from = total_market_share, values_fill = 0)

market_share_table_long %>% filter(state_group=='KY') %>% select(total_market_share) %>% sum()
market_share_table_long <- market_share_table %>% filter(spirit!='total')

ggplot(market_share_table_long, aes(x = reorder(state_group, -total_market_share), y = total_market_share, fill = spirit)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Alcohol Market Share by State and Spirit (2024)",
       x = "State", y = "Market Share (%)") +
  theme_minimal()
















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
cat("\n\n")
cat("The following tables summarize critical quantitative findings for the U.S. alcohol market in the context of 2018 tariffs.\n\n")

cat("Table 1 presents state-level whiskie production data for the European Union market in 2018. For each state, the table displays the production value in millions of dollars and the corresponding market share as a percentage. This table highlights the states that dominate whiskie production and provides insight into the geographic concentration of the U.S. whiskie industry.\n\n")

cat("Table 2 summarizes the tariff-induced losses between 2018 and 2019 in the EU market. The table reports the absolute loss in production value (in millions of dollars), the loss percentage relative to the previous year, and the burden percentage, which indicates each state's contribution to the overall industry losses. The analysis focuses on key states, specifically Tennessee, Texas, and Kentucky, which are critical to understanding the economic impact of tariffs on the U.S. alcohol sector.\n")
