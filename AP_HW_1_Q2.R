# AP Homework 1
# Question 2
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(openxlsx)
library(countrycode)


# Part A ------
# Loading in Data
fname_a <- "C:\\Users\\caleb\\Downloads\\Country_and_Territory_Ratings_and_Statuses_FIW1973-2021.xlsx"
sname_a <- "Historical distribution"
df_country <- read_excel(fname_a, sname_a)

# Renaming Columns for ease
df_country <- df_country %>% 
  rename("Year" = "Year(s) Under Review**", 
         "Free" = "% of F Countries", 
         "Not" = "% of NF Countries", 
         "Part" = "% of PF Countries")

# Fixing the years column
year_fix <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df_country$Year = year_fix(df_country$Year, 4)
df_country$Year = as.numeric(df_country$Year)

#Droping footers and extra rows
df_country <- filter(df_country, Year != "")

# Subset with the years needed
df_country <- df_country %>% 
  select(Year, Free, Not, Part) %>% 
  filter(Year >= 1995)

# pivot for easier plotting
df_country <- df_country %>%
  pivot_longer(cols = `Free`:`Part`, names_to = "ID", values_to = "Value")

# plot
theme_set(theme_economist_white())
ggplot(data = df_country, aes(x = Year, y = Value)) + 
  geom_line(aes(group = ID, color = ID)) + 
  scale_color_manual(values=c("#00AFBB", "#E7B800","#FC4E07"), 
                     labels =c("Free", "Not Free", "Partially Free")) + 
  ylab("Share") +
  ggtitle("Share of Free Countries (1995-2020)") 

# Part B ------

# loading in data
fname_b <- "C:\\Users\\caleb\\Downloads\\Country_and_Territory_Ratings_and_Statuses_FIW1973-2021.xlsx"
sname_b <- "Country Ratings, Statuses "
df_freedom <- read.xlsx(xlsxFile = fname_b, sname_b, fillMergedCells = TRUE, startRow = 2, colNames = FALSE)

# cleaning data
Years <- df_freedom[1,]
cols <- df_freedom[2,]
clean_names <- stringr::str_remove(paste(cols, Years, sep = "_"), "_NA")
df_freedom <- df_freedom %>% 
  slice(-1:-2) %>% 
  setNames(clean_names) %>%
  rename("country" = "NA_Year(s) Under Review") %>%
  pivot_longer(cols = PR_1972:Status_2020, names_to = "Year", values_to = "value")

new_year <- year_fix(df_freedom$Year, 4)
category <- substr(df_freedom$Year, 1, 2)
df_freedom$Year <- new_year
df_freedom$category <- category

df_freedom <- df_freedom %>% 
  pivot_wider(names_from = "category", values_from = "value") 

df_freedom$PR <- as.numeric(df_freedom$PR)
df_freedom$CL <- as.numeric(df_freedom$CL)

# the actual calculation --
# index value
df_freedom <- df_freedom %>% 
  mutate(index = ((PR+CL)/2)/10)

# dummy variables
index_list <- df_freedom$index
dum_list <- list()
i <- 2
for(val in index_list) {
  dum_list[[i]] <- index_list[i]-index_list[i-1]
  i = i + 1
}

dum_list[1] = NA
dum_list <- head(dum_list, -1)
df_freedom$net <- dum_list
df_freedom$better <- ifelse(df_freedom$net > 0,1,0)
df_freedom$worse <- ifelse(df_freedom$net < 0,1,0)
df_freedom <- df_freedom %>% 
  filter(Year != "1972")

# dummy shares
year_list <- seq(1973,2020, by=1)
share_better <- list()
share_worse <- list()
i <- 1
for(val in year_list) {
  share_better[[i]] <- df_freedom %>% 
    select(Year, better) %>% 
    filter(Year == year_list[i]) %>%
    summarise(bet = mean(better, na.rm = TRUE))
  
  share_worse[[i]] <- df_freedom %>% 
    select(Year, worse) %>% 
    filter(Year == year_list[i]) %>%
    summarise(bet = mean(worse, na.rm = TRUE))
  i = i + 1
} 

share_better = unlist(share_better)
share_worse = unlist(share_worse)
df_shares <- data.frame(year_list, share_better, share_worse)

# plot prepare
df_shares <- df_shares %>% 
  pivot_longer(cols = `share_better`:`share_worse`, 
               names_to = "share_type", values_to = "share")

# Plot
theme_set(theme_economist_white())
ggplot(data = df_shares, aes(x = year_list, y = share)) + 
  geom_line(aes(group = share_type, color = share_type)) + 
  scale_color_manual(values=c("#00AFBB", "#FC4E07"), 
                     labels =c("Better", "Worse"))+ 
  ylab("Share") +
  xlab("Year") +
  ggtitle("Net Share of Contries FiW Index") 


# Part C ------
fname_c <- "C:\\Users\\caleb\\Downloads\\UNSD.csv"
df_unsd <- read.csv(fname_c)
df_house <- read.xlsx(xlsxFile = fname_b, sname_b, fillMergedCells = TRUE, startRow = 2, colNames = FALSE)


Years <- df_house[1,]
cols <- df_house[2,]
clean_names <- stringr::str_remove(paste(cols, Years, sep = "_"), "_NA")
df_house <- df_house %>% 
  slice(-1:-2) %>% 
  setNames(clean_names) %>%
  rename("country" = "NA_Year(s) Under Review")

country_vec <- df_house$country
df_house$iso.2 <- countrycode(country_vec, 'country.name', destination = "iso2c")
df_house %>% 
  select(country, iso.2)

df_unsd <- df_unsd %>% 
  select(Region.Name, ISO.alpha2.Code, Least.Developed.Countries..LDC.)
  

merged <- left_join(df_house, df_unsd, by = c("iso.2" = "ISO.alpha2.Code"))
merged <- merged %>% 
  pivot_longer(cols = PR_1972:Status_2020, names_to = "Year", values_to = "value")

new_year <- year_fix(merged$Year, 4)
category <- substr(merged$Year, 1, 2)
merged$Year <- new_year
merged$category <- category
merged <- merged %>% 
  pivot_wider(names_from = "category", values_from = "value") 

# creating a path for part D
df_partd = merged

merged <- merged %>% 
  select(Region.Name, Year, St)

merged <- merged %>% 
  add_column(PF = 
               if_else(.$St == "PF", 1, 0)) %>% 
  add_column(NF = 
               if_else(.$St == "NF", 1, 0)) %>% 
  add_column(F = 
               if_else(.$St == "F", 1, 0))

year_list <- c("2005", "2020")
part_free <- list()
full_free <- list()
no_free <- list()

i <- 1
for(val in year_list) {
  part_free[[i]] <- merged %>% 
    select(Year, Region.Name, PF) %>% 
    filter(Year == year_list[i]) %>%
    summarise(PFm = mean(PF, na.rm = TRUE))
  
  no_free[[i]] <- merged %>% 
    select(Year, Region.Name, NF) %>% 
    filter(Year == year_list[i]) %>%
    summarise(NFm = mean(NF, na.rm = TRUE))
  
  full_free[[i]] <- merged %>% 
    select(Year, Region.Name, F) %>% 
    filter(Year == year_list[i]) %>%
    summarise(Fm = mean(F, na.rm = TRUE))
  i = i + 1
} 

part_free <- unlist(part_free)
no_free <- unlist(no_free)
full_free <- unlist(full_free)
free_shares <- data.frame(year_list, part_free, no_free, full_free)


free_shares <- free_shares %>% 
  pivot_longer(cols = `part_free`:`full_free`, 
               names_to = "share_type", values_to = "share")


# Plot
theme_set(theme_economist_white())
ggplot(data = free_shares, aes(x = year_list, y = share)) + 
  geom_point(aes(group = share_type, color = share_type)) + 
  scale_color_manual(values=c("#00AFBB", "#E7B800","#FC4E07"), 
                     labels =c("Part Free", "Not Free", "Fully Free"))+ 
  ylab("Share") + xlab("Year") +ggtitle("Net Share of Contries By Freedom") 



# Part D ------
df_partd$CL <- as.numeric(df_partd$CL)
df_partd$PR <- as.numeric(df_partd$PR)
df_partd <- df_partd %>% 
  mutate(index = ((PR+CL)/2)) %>% 
  rename("LDC" = "Least.Developed.Countries..LDC.")

year_list <- seq(1995,2020, by=1)
ldc <- list()
n_ldc <- list()
i <- 1
for(val in year_list) {
  ldc[[i]] <- df_partd %>% 
    select(Year, LDC, index) %>% 
    filter(Year == year_list[i]) %>%
    filter(LDC == "x") %>% 
    summarise(avg_ldc = mean(index, na.rm = TRUE))
  
  n_ldc[[i]] <- df_partd %>% 
    select(Year, LDC, index) %>% 
    filter(Year == year_list[i]) %>%
    filter(LDC != "x") %>% 
    summarise(avg_nonldc = mean(index, na.rm = TRUE))
  i = i +1
}

ldc <- unlist(ldc)
n_ldc <- unlist(n_ldc)
df_ldc <- data.frame(year_list, ldc, n_ldc)
df_ldc <- df_ldc %>% 
  pivot_longer(cols = `ldc`:`n_ldc`, 
               names_to = "type", values_to = "average")


theme_set(theme_economist_white())
ggplot(data = df_ldc, aes(x = year_list, y = average)) + 
  geom_point(aes(group = type, color = type)) + 
  scale_color_manual(values=c("#00AFBB","#FC4E07"), 
                     labels =c("Least Developed Country", "Other"))+ 
  ylab("Index") + xlab("Year") +ggtitle("Average FIW Index") 


