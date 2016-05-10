library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(xml2, warn.conflicts=FALSE, quietly=TRUE)
library(rvest, warn.conflicts=FALSE, quietly=TRUE)
library(maps, warn.conflicts=FALSE, quietly=TRUE)
library(mapproj, warn.conflicts=FALSE, quietly=TRUE)
library(RColorBrewer, warn.conflicts=FALSE, quietly=TRUE)
library(rpart, warn.conflicts=FALSE, quietly=TRUE)
library(randomForest, warn.conflicts=FALSE, quietly=TRUE)
library(party, warn.conflicts=FALSE, quietly=TRUE)

# load packages to create a fancy version of the decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# --------------------------------------------
# Functions
# --------------------------------------------
# purpose: scrapes data from web page given html and css selector
# inputs: html (web page link), selector (css selector)
# output: column vector of scraped dom nodes
scrapeWageData <-function(html, selector) {
  html_cast <- html_nodes(html, selector)
  cast <- html_text(html_cast)
  temp <- as.data.frame(cast)
  names(temp)[1] <- "col"
  temp$col <- as.character(temp$col)
  temp$col <- gsub ("\\s","",temp$col) # remove whitespace
  temp$col <- gsub ("\\$","",temp$col) # remove dollar ($) sign
  temp$col <- gsub ("\\,","",temp$col) # remove commas
  return (temp$col)
}

# purpose: calculate mode for given numeric or character vector
# inputs: column vector
# output: column vector statistical mode
getMode <- function(v) {
  val <- unique(v)
  val[which.max(tabulate(match(v, val)))]
}

# --------------------------------------------
# Scraping
# --------------------------------------------
setwd("D:/Github/Data_MIT_Living_Wage")

# data accurate as of Jan-2016
# load supplementary data
supp_data <- tbl_df(read.csv("suppdata.csv", stringsAsFactors=FALSE))

# data accurate as of Dec-2014
# scrape data directly from MIT's living wage calculator website: http://livingwage.mit.edu/
selector <- "tbody td"
html <- read_html("http://livingwage.mit.edu/states/01")
wages <- scrapeWageData(html, selector)
wages <- as.data.frame(wages)
names(wages)[1] <- "alabama"
# html <- read_html("http://livingwage.mit.edu/states/02")
# wages$alaska <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/04")
wages$arizona <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/05")
wages$arkansas <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/06")
wages$california <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/08")
wages$colorado <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/09")
wages$connecticut <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/10")
wages$delaware <- scrapeWageData(html, selector)
# html <- read_html("http://livingwage.mit.edu/states/11")
# wages$disColumbia <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/12")
wages$florida <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/13")
wages$georgia <- scrapeWageData(html, selector)
# html <- read_html("http://livingwage.mit.edu/states/15")
# wages$hawaii <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/16")
wages$idaho <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/17")
wages$illinois <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/18")
wages$indiana <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/19")
wages$iowa <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/20")
wages$kansas <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/21")
wages$kentucky <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/22")
wages$louisiana <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/23")
wages$maine <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/24")
wages$maryland <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/25")
wages$massachusetts <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/26")
wages$michigan <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/27")
wages$minnesota <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/28")
wages$mississippi <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/29")
wages$missouri <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/30")
wages$montana <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/31")
wages$nebraska <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/32")
wages$nevada <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/33")
wages$newhampshire <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/34")
wages$newjersey <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/35")
wages$newmexico <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/36")
wages$newyork <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/37")
wages$northcarolina <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/38")
wages$northdakota <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/39")
wages$ohio <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/40")
wages$oklahoma <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/41")
wages$oregon <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/42")
wages$pennsylvania <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/44")
wages$rhodeisland <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/45")
wages$southcarolina <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/46")
wages$southdakota <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/47")
wages$tennessee <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/48")
wages$texas <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/49")
wages$utah <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/50")
wages$vermont <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/51")
wages$virginia <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/53")
wages$washington <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/54")
wages$westvirginia <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/55")
wages$wisconsin <- scrapeWageData(html, selector)
html <- read_html("http://livingwage.mit.edu/states/56")
wages$wyoming <- scrapeWageData(html, selector)

# --------------------------------------------
# Wrangling
# --------------------------------------------
# remove rows for annual salaries table in states
wages <- wages[1:156,]

# split wages into dataset for hourly wages and typical expenses for each state
hourly_wages <- wages[1:39,]
expenses <- wages[40:156,]

# gather state names into new column
hourly_wages <- gather(hourly_wages, "region", "wages", 1:48)
expenses <- gather(expenses, "region", "expenses", 1:48)

# split wage columns into 3 separate columns
hourly_wages$type <- rep(c("LivingWage", "PovertyWage", "MinWage"), each = 13)
hourly_wages$family <- rep(c("Placeholder", "1 Adult", "1 Adult 1 Child", "1 Adult 2 Children", "1 Adult 3 Children", "2 Adults (One Working)", "2 Adults (One Working) 1 Child", "2 Adults (One Working) 2 Children", "2 Adults (One Working) 3 Children", "2 Adults", "2 Adults 1 Child", "2 Adults 2 Children", "2 Adults 3 Children"), each = 1)
hourly_wages <- spread(hourly_wages, type, wages)
expenses$type <- rep(c("Food", "ChildCare", "Medical", "Housing", "Transportation", "Other", "ReqdIncomeAfterTaxes", "AnnualTaxes", "ReqdIncomeBeforeTaxes"), each = 13)
expenses$family <- rep(c("Placeholder", "1 Adult", "1 Adult 1 Child", "1 Adult 2 Children", "1 Adult 3 Children", "2 Adults (One Working)", "2 Adults (One Working) 1 Child", "2 Adults (One Working) 2 Children", "2 Adults (One Working) 3 Children", "2 Adults", "2 Adults 1 Child", "2 Adults 2 Children", "2 Adults 3 Children"), each = 1)
expenses <- spread(expenses, type, expenses)

# remove placeholder rows (last remnant from html scraping)
hourly_wages <- hourly_wages %>%
  filter(family != "Placeholder")
expenses <- expenses %>%
  filter(family != "Placeholder")

# correct state spellings to match on 'region' names for hourly_wages
hourly_wages$region[hourly_wages$region == "newhampshire"] <- "new hampshire"
hourly_wages$region[hourly_wages$region == "newjersey"] <- "new jersey"
hourly_wages$region[hourly_wages$region == "newmexico"] <- "new mexico"
hourly_wages$region[hourly_wages$region == "newyork"] <- "new york"
hourly_wages$region[hourly_wages$region == "northcarolina"] <- "north carolina"
hourly_wages$region[hourly_wages$region == "northdakota"] <- "north dakota"
hourly_wages$region[hourly_wages$region == "rhodeisland"] <- "rhode island"
hourly_wages$region[hourly_wages$region == "southcarolina"] <- "south carolina"
hourly_wages$region[hourly_wages$region == "southdakota"] <- "south dakota"
hourly_wages$region[hourly_wages$region == "westvirginia"] <- "west virginia"
unique(hourly_wages$region)

# correct state spellings to match on 'region' names for expenses
# expenses$region[expenses$region == "disColumbia"] <- "district of columbia"
expenses$region[expenses$region == "newhampshire"] <- "new hampshire"
expenses$region[expenses$region == "newjersey"] <- "new jersey"
expenses$region[expenses$region == "newmexico"] <- "new mexico"
expenses$region[expenses$region == "newyork"] <- "new york"
expenses$region[expenses$region == "northcarolina"] <- "north carolina"
expenses$region[expenses$region == "northdakota"] <- "north dakota"
expenses$region[expenses$region == "rhodeisland"] <- "rhode island"
expenses$region[expenses$region == "southcarolina"] <- "south carolina"
expenses$region[expenses$region == "southdakota"] <- "south dakota"
expenses$region[expenses$region == "westvirginia"] <- "west virginia"
unique(expenses$region)

# fix column types for hourly_wages
hourly_wages$region <- as.factor(hourly_wages$region)
hourly_wages$family <- as.factor(hourly_wages$family)
hourly_wages$LivingWage <- as.numeric(hourly_wages$LivingWage)
hourly_wages$MinWage <- as.numeric(hourly_wages$MinWage)
hourly_wages$PovertyWage <- as.numeric(hourly_wages$PovertyWage)

# fix column types for expenses
expenses$region <-  as.factor(expenses$region)
expenses$family <- as.factor(expenses$family)
expenses$AnnualTaxes <- as.numeric(expenses$AnnualTaxes)
expenses$ChildCare <- as.numeric(expenses$ChildCare)
expenses$Food <- as.numeric(expenses$Food)
expenses$Housing <- as.numeric(expenses$Housing)
expenses$Medical <- as.numeric(expenses$Medical)
expenses$Other <- as.numeric(expenses$Other)
expenses$ReqdIncomeAfterTaxes <- as.numeric(expenses$ReqdIncomeAfterTaxes)
expenses$ReqdIncomeBeforeTaxes <- as.numeric(expenses$ReqdIncomeBeforeTaxes)
expenses$Transportation <- as.numeric(expenses$Transportation)

# prep states overlay data
states <- data.frame(state.center, state.abb)
states <- subset(states, !state.abb %in% c("HI", "AK"))
usa_map <- map_data("state")

# --------------------------------------------
# Analysis
# --------------------------------------------
# summaries
str(hourly_wages)
str(expenses)
summary(hourly_wages)
summary(expenses)

# including divisions for west, midwest, northeast and south
hourly_wages$division <- ""
hourly_wages$division[hourly_wages$region %in% c("washington", "oregon", "california", "nevada", "utah", "idaho", "montana", "wyoming", "colorado", "new mexico", "arizona")] <- "west"
hourly_wages$division[hourly_wages$region %in% c("north dakota", "south dakota", "minnesota", "kansas", "nebraska", "iowa", "missouri", "wisconsin", "illinois", "indiana", "ohio", "michigan")] <- "midwest"
hourly_wages$division[hourly_wages$region %in% c("texas", "oklahoma", "arkansas", "louisiana", "mississippi", "tennessee", "alabama", "south carolina", "north carolina", "virginia", "west virginia", "kentucky", "florida", "georgia", "delaware", "maryland")] <- "south"
hourly_wages$division[hourly_wages$region %in% c("new jersey", "connecticut", "rhode island", "new york", "pennsylvania", "vermont", "new hampshire", "massachusetts", "maine")] <- "northeast"
hourly_wages$division <- as.factor(hourly_wages$division)

# Plot - Living wages by state for 1 Adult
# set appropriate selector for plot
selector <- "1 Adult"
hourly_wages_sel <- hourly_wages %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(LivingWage, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Living Wages', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Living Wages by State for 1 Adult')

# Plot - Living wages by state for 1 Adult 1 Child
# set appropriate selector for plot
selector <- "1 Adult 1 Child"
hourly_wages_sel <- hourly_wages %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(LivingWage, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Living Wages', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Living Wages by State for 1 Adult 1 Child')

# Plot - Living wages by state for 2 Adults 2 Children
# set appropriate selector for plot
selector <- "2 Adults 2 Children"
hourly_wages_sel <- hourly_wages %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(LivingWage, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Living Wages', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Living Wages by State for 2 Adults 2 Children')

# Plot - Expenses by state for 1 Adult
# set appropriate selector for plot
selector <- "1 Adult"
expenses_sel <- expenses %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(expenses_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(ReqdIncomeBeforeTaxes/1000, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Income Reqd (in thousands)', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Income Reqd Before Taxes by State for 1 Adult')

# Plot - Expenses by state for 1 Adult 1 Child
# set appropriate selector for plot
selector <- "1 Adult 1 Child"
expenses_sel <- expenses %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(expenses_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(ReqdIncomeBeforeTaxes/1000, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Income Reqd (in thousands)', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Income Reqd Before Taxes by State for 1 Adult 1 Child')

# Plot - Expenses by state for 2 Adults 2 Children
# set appropriate selector for plot
selector <- "2 Adults 2 Children"
expenses_sel <- expenses %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(expenses_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(ReqdIncomeBeforeTaxes/1000, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Income Reqd (in thousands)', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Income Reqd Before Taxes by State for 2 Adults 2 Children')

# Plot - Expenses by state for 1 Adult 1 Child for Childcare Costs
# set appropriate selector for plot
selector <- "1 Adult 1 Child"
expenses_sel <- expenses %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(expenses_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for living wages by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(ChildCare/1000, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Child Care (in thousands)', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Child Care Expenses by State for 1 Adult 1 Child')


# Plot - Living wages by state for 1 Adult
# set appropriate selector for plot
selector <- "1 Adult"
hourly_wages_sel <- hourly_wages %>%
  filter(family == selector)

# reorg dataframe to for key-value pair of wage type - wages by state
hourly_wages_sel <- gather(hourly_wages_sel, type, wages, 3:5)

# plot for living wages by state
ggplot(data = hourly_wages_sel, aes(x = type, y = wages)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  coord_flip() + 
  xlab("Wage Type") +
  ylab ("Wage (in USD)") + 
  ggtitle('Distribution of Wage by State for 1 Adult')


# Plot - Wages by division for 1 Adult, 1 Adult 1 Child, 2 Adults 2 Children
# set appropriate selector for plot
hourly_wages_sel <- hourly_wages %>%
  filter(family %in% c("1 Adult", "1 Adult 1 Child", "2 Adults 2 Children")) %>%
  select(-region)

# create summaries for mean and median across divisions
by_div <- group_by(hourly_wages_sel, division, family)
wages_by_division <- summarise(by_div, 
          meanLW = mean(LivingWage),
          meanMW = mean(MinWage), 
          meanPW = mean(PovertyWage), 
          medLW = median(LivingWage),
          medMW = median(MinWage), 
          medPW = median(PovertyWage)
          )

# plot for living wages by region
ggplot(data = wages_by_division, aes(x = division)) + 
  geom_bar(stat = "identity", aes(y = medLW, fill = family), position = "dodge") + 
  coord_flip() + 
  xlab("Region") +
  ylab ("Median Living Wage") + 
  ggtitle('Median Living Wage by Region')

# plot for minimum wages by region
ggplot(data = wages_by_division, aes(x = division)) + 
  geom_bar(stat = "identity", aes(y = medMW, fill = family), position = "dodge") + 
  coord_flip() + 
  xlab("Region") +
  ylab ("Median Minimum Wage") + 
  ggtitle('Median Minimum Wage by Region')

# Plot - Living wages by state for all family types
# set appropriate selector for plot
hourly_wages_sel <- hourly_wages

# reorg dataframe to for key-value pair of wage type - wages by state
hourly_wages_sel <- gather(hourly_wages_sel, type, wages, 3:5)

# plot for living wages by state
ggplot(data = hourly_wages_sel, aes(x = family, y = wages)) + 
  geom_boxplot(aes(color = type)) + 
  xlab("Family Type") +
  ylab("Wage (in USD)") + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.85,0.85)) +
  ggtitle('Distribution of Wage by State for All Family Types')

# Plot - Living wages state distrubtion by family type
ggplot(data = hourly_wages, aes(x = LivingWage)) + 
  geom_histogram(binwidth = 2) + 
  facet_wrap(~family) + 
  xlab("Count") +
  ylab ("Living Wage (in USD)") + 
  ggtitle('Distribution of Living Wage for States by Family Type')  


# Plot - Percentage of Wage Workers in Labour Unions by State
# create map overlay data
map_data <- merge(supp_data, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for union participation by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(union, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Union % of Labour', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Pct. Wage Workers in Labour Unions by State')

# plot for party of governor by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = gov)) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_manual('Party', values = c("#30b4f4", "#ff6262", "white")) +
  theme_bw() + 
  ggtitle('Political Party of Governor by State')

# plot for party of legislature by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = leg)) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_manual('Party', values = c("#30b4f4", "#ff6262", "#a18bb6")) +
  theme_bw() + 
  ggtitle('Political Party of Legislature by State')

# plot for right to work laws by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = rtw)) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_manual(values = c("white", "#a18bb6")) +
  guides(fill = FALSE) + 
  theme_bw() + 
  ggtitle('States with Right to Work Laws')


# Plot - Living wages as percentage of minimum wage by state
# create new column for living wage / minimum wage multiple
hourly_wages_sel <- hourly_wages %>%
  mutate(pctWage = LivingWage / MinWage) %>%
  select(-LivingWage, -MinWage, -PovertyWage, -region)

# create summaries for mean and median across divisions
by_div <- group_by(hourly_wages_sel, division, family)
wages_by_division <- summarise(by_div, 
                               meanPW = mean(pctWage))

# plot for living wages by region
ggplot(data = wages_by_division, aes(x = division)) + 
  geom_bar(stat = "identity", aes(y = meanPW, fill = family), position = "dodge") + 
  scale_fill_discrete(name = "Family Type") +
  xlab("Region") +
  ylab ("Mean Multiple of Living Wage / Min. Wage") + 
  ggtitle('Mean Wage Multiple by Region for All Families')


# Plot - Wage multiple by state
# create new column for living wage / minimum wage multiple
tmp <- hourly_wages %>%
  mutate(pctWage = LivingWage / MinWage) %>%
  select(-LivingWage, -MinWage, -PovertyWage, -division)

# set appropriate selector for plot
selector <- "1 Adult"
hourly_wages_sel <- tmp %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for wage multiple by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(pctWage, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Wage Multiple', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Wage Multiple by State for 1 Adult')

# set appropriate selector for plot
selector <- "1 Adult 2 Children"
hourly_wages_sel <- tmp %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for wage multiple by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(pctWage, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Wage Multiple', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Wage Multiple by State for 1 Adult 2 Children')


# Plot - Expenses covered by wages for 1 Adult
# combine the two dataframes
hourly_wages_sel <- inner_join (hourly_wages, expenses)

# create new column for expenses multiple calculation
tmp <- hourly_wages_sel %>%
  mutate(hoursNeeded = round((((Housing + Food + ChildCare + Medical)/ 52) / MinWage), 0)) %>%
  mutate(jobsNeeded = round(((Housing + Food + ChildCare + Medical)/ (52*40)) / MinWage, 1)) %>%
  select(-PovertyWage, -AnnualTaxes, -Other, -ReqdIncomeAfterTaxes, -Transportation)

# set appropriate selector for plot
selector <- "1 Adult 1 Child"
hourly_wages_sel <- tmp %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for expenses coverage by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(hoursNeeded, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Hours', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Hours Needed to Cover Base Expenses by State for 1 Adult 1 Child')

# set appropriate selector for plot
selector <- "1 Adult 2 Children"
hourly_wages_sel <- tmp %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for expenses coverage by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(hoursNeeded, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Hours', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Hours Needed to Cover Base Expenses by State for 1 Adult 2 Children')

# set appropriate selector for plot
selector <- "1 Adult 2 Children"
hourly_wages_sel <- tmp %>%
  filter(family == selector)

# create map overlay data
map_data <- merge(hourly_wages_sel, usa_map, by = 'region')
map_data <- arrange(map_data, order)

# plot for expenses coverage by state
ggplot(data = map_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut_interval(jobsNeeded, 5))) + 
  geom_path(colour = 'gray', linetype = 2) + 
  coord_map() + 
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 4) + 
  scale_fill_brewer('Hours', palette  = 'YlOrRd') + 
  theme_bw() + 
  ggtitle('Jobs Needed to Cover Base Expenses by State for 1 Adult 2 Children')


# --------------------------------------------
# Predictions
# --------------------------------------------
# predicting which states can expect minimum wage hikes in the future
fullset <- inner_join (hourly_wages, expenses)
fullset <- inner_join (fullset, supp_data, by = "region")
fullset <- fullset %>%
  select(-PovertyWage, -ReqdIncomeAfterTaxes, -Transportation, -Other)
  
# create wage hike column and set all states that have increased their minimum wage since 2014 (year of dataset collection) to 1
# this does not include states that raise min. wage regularly to correct for inflation (e.g. montana, missouri, nevada etc.)
fullset$wageHike <- 0
fullset$wageHike[fullset$region %in% 
                   c("arkansas",
                     "connecticut",
                     "delaware",
                     "maryland",
                     "massachusetts",
                     "minnesota",
                     "nebraska",
                     "rhode island",
                     "south dakota",
                     "vermont",
                     "west virginia")] <- 1

# create test and train datasets for prediction models
fullset$trainYN <- 0
fullset$trainYN[fullset$wageHike == 1] <- 1
fullset$trainYN[fullset$region %in% 
                   c("alabama",
                     "illinois",
                     "indiana",
                     "texas",
                     "flordia",
                     "north carolina",
                     "nevada",
                     "tennessee",
                     "utah",
                     "new mexico",
                     "kentucky",
                     "virginia",
                     "mississippi")] <- 1
train <- fullset %>% 
  filter(trainYN == 1)
test <- fullset %>% 
  filter(trainYN == 0)

# prediction #1 - using decision trees
# build decision tree
my_tree <- rpart(wageHike ~ MinWage + Housing + family + Food + Medical + ChildCare + union + leg + gov + rtw, data = train, method = "class")

# visualize the decision tree using plot() and text()
plot(my_tree)
text(my_tree)

# plot fancy tree
fancyRpartPlot(my_tree, f)

# make prediction using the test set
prediction_tree <- predict(my_tree, test, type = "class")

# create data frame with two columns state and wageHike
solution_tree <- data.frame(state = test$region, wageHike = prediction_tree)

# create summary by state
by_state <- group_by(solution_tree, state)
solution_tree <- summarise(by_state,  
                           decTree = getMode(wageHike))


# prediction #2 - using random forrest
# clean up factor issues for random forrest function
train$gov <- factor(train$gov)
train$leg <- factor(train$leg)
train$rtw <- factor(train$rtw)
test$gov <- factor(test$gov)
test$leg <- factor(test$leg)
test$rtw <- factor(test$rtw)

# get forrest prediction and plot accuracy and gini coeff. charts for features
forrest <- randomForest(as.factor(wageHike) ~  MinWage + Housing + family + Food + Medical + ChildCare + union + leg + gov + rtw, data=train, importance=TRUE, ntree=2000)
varImpPlot(forrest)

# make prediction using the complete dataset
prediction_forrest <- predict(forrest, test)

# create data frame with two columns per kaggle rules: PassengerId & Survived
solution_forrest <- data.frame(state = test$region, wageHike = prediction_forrest)

# create summary by state
by_state <- group_by(solution_forrest, state)
solution_forrest <- summarise(by_state,  
                           ranForrest = getMode(wageHike))


# prediction #3 - using party package (forrest of inference trees)
set.seed (400)
party <- cforest(as.factor(wageHike) ~  MinWage + Housing + family + Food + Medical + ChildCare + union + leg + gov + rtw, data = train, controls = cforest_unbiased(ntree=2000, mtry=3))

# make prediction using the complete dataset
prediction_inf_forrest <- predict(party, test, OOB=TRUE, type="response")

# create data frame with two columns per kaggle rules: PassengerId & Survived
solution_inf_forrest <- data.frame(state = test$region, wageHike = prediction_inf_forrest)

# create summary by state
by_state <- group_by(solution_inf_forrest, state)
solution_inf_forrest <- summarise(by_state,  
                              infForrest = getMode(wageHike))

# create a dataframe joining all three prediction models
solution <- inner_join(solution_tree, solution_forrest, by = "state")
solution <- inner_join(solution, solution_inf_forrest, by = "state")

# csv solution file
write.csv(solution, file="prediction_wage_hike_states.csv" , row.names = FALSE)

