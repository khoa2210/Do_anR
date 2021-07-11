confirmedraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
str(confirmedraw)
deathsraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
library(tidyr)
library(dplyr)
library(ggplot2)
confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
summary(confirmed)
country <- full_join(confirmed, deaths) %>% full_join(recovered)
str(country) 
country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
str(country) 
country <- country %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)

world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
vietnam<- country %>% filter(Country.Region=="Vietnam")
us<- country %>% filter(Country.Region=="US")

summary(country)
by(country$confirmed, country$Country.Region, summary)
by(country$cumconfirmed, country$Country.Region, summary)
by(country$deaths, country$Country.Region, summary)
by(country$recovered, country$Country.Region, summary)
summary(world)
summary(vietnam)
#Hình 1 
ggplot(world, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=1)  +
  labs(title = "Confirmed Covid-19 cases in the world", x= "Date", y= "Confirmed Case") +
  theme(plot.title = element_text(hjust = 0.5))
#Hình 2 
ggplot(vietnam, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=1) +
  theme_classic() +
  labs(title = "Death Covid-19 cases in Vietnam", x= "Date", y= "Death Case") +
  theme(plot.title = element_text(hjust = 0.5))
# Hình 3 
countrytotal <- country %>% group_by(Country.Region) %>% summarize(cumconfirmed=max(confirmed), cumdeaths=max(deaths), cumrecovered=max(recovered))
library(tmap)
data(World)
class(World)

countrytotal$Country.Region[!countrytotal$Country.Region %in% World$name]
list <- which(!countrytotal$Country.Region %in% World$name)
countrytotal$country <- as.character(countrytotal$Country.Region)
countrytotal$country[list] <-
  c("Andorra", "Antigua and Barbuda", "Bahrain",
    "Barbados", "Bosnia and Herz.", "Myanmar",
    "Cape Verde", "Central African Rep.", "Congo",
    "Dem. Rep. Congo", "Czech Rep.", "Diamond Princess",
    "Dominica", "Dominican Rep.", "Eq. Guinea",
    "Swaziland", "Grenada", "Holy See",
    "Korea", "Lao PDR", "Liechtenstein",
    "Maldives", "Malta", "Mauritius",
    "Monaco", "MS Zaandam", "Macedonia",
    "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
    "San Marino", "Sao Tome and Principe", "Seychelles",
    "Singapore", "S. Sudan", "Taiwan",
    "United States", "Palestine", "W. Sahara")
countrytotal$Country.Region[!countrytotal$country %in% World$name]
World$country <- World$name
worldmap <- left_join(World, countrytotal, by="country")
worldmap$cumconfirmed[is.na(worldmap$cumconfirmed)] <- 0

# Map
ggplot(data = worldmap) + geom_sf(aes(fill=cumconfirmed), color="black") +
  ggtitle("Map showing Covid 19 in the world",
          subtitle="Covid Case in 8-7-2020") +
  theme_bw()
# Hình 4
lasted = subset(country, date == max(date))
x= lasted 
x = arrange(x, desc(confirmed), decreasing= FALSE)
top10 = x[1:10,]
ggplot(top10, aes(x=Country.Region, y=confirmed)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  ggtitle("Top 10 countries with the most cases of covid 19 ",
          subtitle="Covid Case in 8-7-2020") +
  geom_text(aes(label = confirmed),
            position = position_stack(vjust = 0.6)) +
  coord_polar(theta = "y")+
  coord_flip() +
  xlab("") +
  theme_bw()+theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
                   axis.text.y= element_text(size=15), axis.title=element_text(size=18))
# Hình 5
library(treemapify)
ggplot(top10, aes(area =confirmed, fill = Country.Region, label = Country.Region)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)
#Hình 6
top10$prop = sprintf((top10$confirmed / sum(top10$confirmed) *100), fmt = '%#.2f')
ggplot(top10, aes(x="",y = confirmed,fill=Country.Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
    position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("The pie chart shows the rate of cases of the top 10",
    subtitle="Covid case in 9-7-2021") +
  theme_void()
#Hình 7
gg7 = ggplot()
gg7 + geom_line(data = us, aes(x= date, y= confirmed),size = 2,color = 'red') + 
  geom_line(data = us, aes(x= date, y= deaths),size = 2,color = 'blue')+ 
  geom_line(data = us, aes(x= date, y= recovered),size = 2,color = 'green') +
  ggtitle("Chart of the number of people confirmed, died, recovered in the United States",
          subtitle="Covid case 9-7-2021") +
  theme(plot.title = element_text(hjust = 0.5))
#Hình 8
library(tidyverse)              
library(haven)
library(classInt)
library(readstata13)
poplink_US <- "https://github.com/partha-deb/COVID-19-outcomes/blob/master/Data/statepopulation.dta?raw=true"
pop_US <- read.dta13(poplink_US)

covidlink_US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid_US0 <- read.csv(covidlink_US)
ll <- 1                   
maxdays <- 50                                   
ref_date <- "3/20/20"
gtitle <- "COVID-19 summary by US States"
ylabel <- "Cumulative deaths per million people"

covid_US <- covid_US0 %>% 
  select(c(county="Admin2",
           state= "Province_State"),
         everything()) %>% 
  gather(13:ncol(covid_US0) ,                            
         key="Date", 
         value="death") %>% 
  mutate(Date=gsub("x","",Date)) %>%                       
  mutate(Date=as.Date(as.character(Date),"%m.%d.%y")) %>% 
  group_by(state, Date) %>%                              
  summarize(deaths = max(death)) %>% 
  left_join(pop_US, by="state") %>% 
  mutate(days = as.vector(unlist(lapply(table(state), seq_len))))

covid_US$ratedeath = (covid_US$deaths /covid_US$population *100)
r = arrange(covid_US,desc(ratedeath))
r$Date <- NULL
top10r = r[1:10,]
ggplot(top10r, aes(x=state, y=ratedeath)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  ggtitle("The chart of the top 10 states with the highest rate of deaths from covid",
          subtitle="Covid case 9-7-2021")
#Hình 9
ggplot(top10r, aes(x=deaths, y=population)) + 
  geom_point(aes(color=state,size = 6)) + 
  labs(x="deaths", 
       y="population", 
       title="The graph of the number of deaths and the population of 10 states")+ 
  theme_gray()+ # Default theme 
  theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15), axis.title=element_text(size=18))
 
#Hình 10
library(tidyverse)
library(lubridate)

theme_set(theme_minimal())

covid19_raw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid19 <- covid19_raw %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()

covid19 <- covid19 %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()

covid19 %>%
  filter(country_region == "US") %>%
  ggplot(aes(x = date, y = new_cases_n)) +
  geom_line() +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date", y = "New cases",
    title = "New cases in the US"
  )+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Hình 11
covid19 %>%
  filter(country_region %in% c("US", "Australia")) %>%
  group_by(country_region) %>%
  mutate(first_date_over_50_confirmed_cases = min(date[confirmed_n >= 50])) %>%
  ungroup() %>%
  mutate(days_after_50 = (first_date_over_50_confirmed_cases %--% date) / days(1)) %>%
  filter(days_after_50 >= 0) %>%
  ggplot(aes(x = days_after_50, y = new_cases_n, color = country_region)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~country_region, ncol = 1, scales = "free_y") +
  labs(
    x = "Days >= 50 confirmed cases in the country", y = "New cases",
    title = "New cases in the US and Ustralia"
  )

#Hình 12
world$ratedeath = (world$deaths /world$confirmed*100)
world$raterecover = (world$recovered /world$confirmed*100)
ggplot(world, aes(x=date, y=ratedeath)) + geom_bar(stat="identity", width=1) +
  theme_classic() +
  labs(title = "Death rate from covid by date", x= "Date", y= "Death rate") +
  theme(plot.title = element_text(hjust = 0.5))
#Hình 13
ggplot(world, aes(x=date, y=raterecover)) + geom_bar(stat="identity", width=1) +
  theme_classic() +
  labs(title = "Recover rate from covid by date", x= "Date", y= "Recover rate") +
  theme(plot.title = element_text(hjust = 0.5))
#Hình 14
lasted$raterecover = sprintf((lasted$recovered /lasted$confirmed*100), fmt = '%#.2f')
z= arrange(lasted, desc(raterecover), decreasing= FALSE)
top10z = z[1:20,]
ggplot(top10z, aes(x=Country.Region, y=raterecover)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  ggtitle("Top 20 places with the highest recovery rate",
          subtitle="Covid case 8-7-2020") +
  geom_text(aes(label = raterecover),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  coord_flip() +
  xlab("") +
  theme_bw()+theme(plot.title = element_text(size=15),axis.text.x= element_text(size=10),
                   axis.text.y= element_text(size=10), axis.title=element_text(size=15))
#Hình 15
df1 <- lasted[,3:5] %>% summarise_all(funs(sum))
df1$Active <- df1$confirmed -df1$deaths - df1$recovered
df1
df2 <- data.frame(Cases = colnames(df1), n = as.vector(unlist(df1)))

ggplot(df2, aes (x="", y = n, fill = factor(Cases))) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(n / sum(n) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(fill = "Cases",
       x = NULL,
       y = NULL,
       title = "Percent cases of covid 19 in the world") +
  coord_polar("y")
#Hình 16
library(treemapify)
ggplot(df2, aes(area =n, fill = Cases, label = Cases)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)
#Hình 17
library(tidyverse)
library(anytime)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
dta_raw <- read_csv(url, col_types = cols()) %>%
  select(-Lat, -Long)

selection <- c("Japan", "Laos",  "Ukraine", "Vietnam", "France", "Germany", "Austria", "US", "United Kingdom")

dta <-
  dta_raw %>%
  
  rename(province = `Province/State`, country = `Country/Region`) %>%
  pivot_longer(c(-province, -country), "time") %>%
  mutate(time = as.Date(time, "%m/%d/%y")) %>%
  
  filter(country %in% !! selection) %>%
  
  group_by(country, time) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%

  arrange(time) %>%
  group_by(country) %>%
  mutate(diff = value - lag(value)) %>%
  ungroup() %>%
  filter(!is.na(diff)) %>%
  arrange(country, time)

dta %>%
  filter(diff > 0) %>%
  ggplot(aes(x = time, y = diff, fill = country %in% c("China", "Korea, South"))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(country), scales = "free_y") +
  ggtitle("New case", "Corona (COVID-19)")+
  theme_minimal()
#Hình 18
library(tidyverse)
library(lubridate) 
library(gganimate) 
library(tidycensus) 
library(transformr) 
library(ggthemes) 
library(viridis) 
library(scales) 
library(zoo) 
knitr::opts_chunk$set(
  message = F,
  echo = T,
  include = T
)
options( scipen = 10 )
covid_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covid_cases <- pivot_longer(covid_cases, 12:length(covid_cases), names_to = "date", values_to = "cases") %>%
  mutate(date = lubridate::as_date(date, format = "%m/%d/%y")) %>%
  filter(Province_State == 'Arkansas') %>% 
  arrange(date, Combined_Key)
tail(covid_cases %>% select(Combined_Key, date, cases))
population <- tidycensus::get_estimates(geography = "county", "population") %>% 
  mutate(GEOID = as.integer(GEOID)) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  filter(grepl("Arkansas", NAME))
head(population)
ark_covid_cases <- covid_cases %>% 
  filter(`Province_State` == 'Arkansas')
ark_covid_cases <- covid_cases %>% 
  filter(`Province_State` == 'Arkansas')
p <- ark_covid_cases %>%
  filter(cases > 0) %>%
  group_by(Province_State, date) %>%
  mutate(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  scale_x_date(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = unit_format(unit = "k", sep = "", big.mark = ",", scale = 1/1000)) +
  labs(
    title = "Covid case in Arkansas,US",
    x = "", y = ""
    
  )
p

#Hình 19
library(reshape2)
corona_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data_deaths <- corona_deaths %>%
  filter(`Country/Region` == "Germany" | `Country/Region` == "Italy" |  `Country/Region` == "China"  |  `Country/Region` == "US" | `Country/Region` == "France" | `Country/Region` == "New Zealand") %>%
  filter(`Province/State` !="Diamond Princess" | is.na(`Province/State`)) %>%
  select(-`Province/State`) %>%
  select(-Lat,-Long) %>%
  group_by(`Country/Region`) %>%
  summarise_each(list(sum))

n <- data_deaths$`Country/Region`
data_deaths <- as.data.frame(t(data_deaths[,-1]))
colnames(data_deaths) <- n
data_deaths <- tibble::rownames_to_column(data_deaths, "Day")
data_deaths <- data_deaths %>%
  mutate(Day = as.Date(Day,"%m/%d/%y"))

d_deaths <- melt(data_deaths, id.vars="Day")

# plot
ggplot(d_deaths, aes(Day,value, col=variable)) + 
  geom_line() +
  ggtitle("Death per Country") + 
  xlab("Date") + 
  ylab("Deaths")

#Hình 20
gg20 = ggplot()
gg20 + geom_line(data = vietnam, aes(x= date, y= confirmed ),size = 2,color = 'yellow') + 
  geom_line(data = vietnam, aes(x= date, y= deaths),size = 2,color = 'gray')+ 
  geom_line(data = vietnam, aes(x= date, y= recovered),size = 2,color = 'pink') +
  ggtitle("Confirmed, death, recover case in Vietnam",
          subtitle="Covid case 9-7-2021") +
  theme(plot.title = element_text(hjust = 0.5))

