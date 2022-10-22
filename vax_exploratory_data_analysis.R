#Code for covid-19 vaccination exploratory data analysis project

#Update dataset
library(coronavirus)
detach("package:coronavirus", unload = TRUE)
coronavirus::update_dataset()
data(covid19_vaccine)

#load packages to use
library(coronavirus)
library(data.table)
library(ggplot2)
library(xtable)

data(coronavirus)
data(covid19_vaccine)

#Convert data.frame to data.table
setDT(covid19_vaccine)
setDT(coronavirus)

print(max(covid19_vaccine$date))
print(max(coronavirus$date))


#Keep information only on country level
covid19_vaccine = covid19_vaccine[is.na(province_state)]
coronavirus = coronavirus[is.na(province)]


#Create columns for vaccination ratios
covid19_vaccine[, fully_vaccinated_ratio := 
                  round(100*people_fully_vaccinated/population,2)]
covid19_vaccine[, partially_vaccinated_ratio := 
                  round(100*people_partially_vaccinated/population,2)]


#Part 1
#Aggregations by Continent (Europe, South America and North America)

#Aggregate total vaccinations for the three continents of interest
continents_vax = covid19_vaccine[, 
                list(partially_vax=sum(people_partially_vaccinated),
                     fully_vax = sum(people_fully_vaccinated))
                ,by=list(continent_name,date)][continent_name 
                      %in% list('Europe','South America','North America')]

pdf(file="figs/plot1.pdf", width = 8, height = 5)

#Plots to show vaccinated people per continent
ggplot(continents_vax, aes(x=date,y=fully_vax/1000000, color=continent_name)) +
  geom_line() +
  labs( x = "Date", y = "Fully Vaccinated People (millions)", color='Continent') 

dev.off()

#Data Cleaning
#First replace NaN values with previous value
continents_vax = continents_vax[order(continent_name)]
continents_vax[, fully_vax_fixed:= fully_vax[1],.
               (continent_name, cumsum(!is.na(fully_vax)))]
continents_vax[, partially_vax_fixed:= partially_vax[1],.
               (continent_name, cumsum(!is.na(partially_vax)))]


#Second, replace smaller values with previous value
continents_vax[, fully_vax_fixed:= cummax(fully_vax_fixed),
               by=.(continent_name)]
continents_vax[, partially_vax_fixed:= cummax(partially_vax_fixed),
               by=.(continent_name)]


#Plot absolute number of vaccinated people of update dataset
pdf(file="figs/plot2.pdf", width = 8, height = 5)
ggplot(continents_vax, aes(x=date,y=fully_vax_fixed/1000000, color=continent_name)) +
  geom_line() +
  labs( x = "Date", y = "Fully Vaccinated People (millions)", color='Continent')
dev.off()

#Create columns for vaccination ratios

#First find the population of each continent from the last data point 
europe_pop = max(covid19_vaccine[,.(sum(population)),by=
                                   list(continent_name,date)][continent_name==
                                                                'Europe']$V1)

south_am_pop = max(covid19_vaccine[,.(sum(population)),by=
                                     list(continent_name,date)][continent_name==
                                                            'South America']$V1)

north_am_pop = max(covid19_vaccine[,.(sum(population)),by=
                                     list(continent_name,date)][continent_name==
                                                            'North America']$V1)
continents_vax[continent_name=='Europe', `:=` (
  fully_vaccinated_ratio= round(100*fully_vax_fixed/europe_pop,2),
  partially_vaccinated_ratio= round(100*partially_vax_fixed/europe_pop,2))]

continents_vax[continent_name=='South America',`:=`(
  fully_vaccinated_ratio = round(100*fully_vax_fixed/south_am_pop,2),
  partially_vaccinated_ratio = round(100*partially_vax_fixed/south_am_pop,2))]

continents_vax[continent_name=='North America',`:=`(
  fully_vaccinated_ratio = round(100*fully_vax_fixed/north_am_pop,2),
  partially_vaccinated_ratio = round(100*partially_vax_fixed/north_am_pop,2))]

#Plots

pdf(file="figs/plot3.pdf", width = 8, height = 5)

ggplot(continents_vax, aes(x=date,y=fully_vaccinated_ratio, 
                           color=continent_name)) +  geom_line() +
  labs( x = "Date", y = "Fully Vaccinated Ratio (%)", color='Continent') 

dev.off()

pdf(file="figs/plot4.pdf", width = 8, height = 5)

ggplot(continents_vax, aes(x=date,y=partially_vaccinated_ratio, 
                           color=continent_name)) +  geom_line() +
  labs( x = "Date", y = "Partially Vaccinated Ratio (%)", color='Continent') 

dev.off()

#Part 2
#Create data tables for Europe Union for vaccinations and daily deaths
eu_countries = c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia',
                 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece',
                 'Hungary','Ireland', 'Italy', 'Latvia', 'Lithuania','Luxembourg',
                 'Malta', 'Netherlands', 'Poland', 'Portugal','Romania', 'Slovakia',
                 'Slovenia', 'Spain', 'Sweden')


eu_vax = covid19_vaccine[country_region %in% eu_countries]
eu_deaths = coronavirus[country %in% eu_countries & type=='death' 
                            & date>='2021-01-01']

#Keep columns of interest
eu_vax = eu_vax[,.(country_region,date,
           population,fully_vaccinated_ratio,partially_vaccinated_ratio)]
eu_deaths = eu_deaths[,.(date,country,cases,population)]

#calculate cumilative deaths and death ratios
eu_deaths[, daily_deaths_per_100000 := round(100000*cases/population,1)]
eu_deaths[,total_deaths :=cumsum(cases),by=list(country)]
eu_deaths[,total_deaths_per_100000 := as.numeric(round(100000*total_deaths/population))]

#Data Cleaning
#Fix vaccination ratios drops and NaN values
eu_vax[, fully_vaccinated_ratio:= fully_vaccinated_ratio[1],.
               (cumsum(!is.na(fully_vaccinated_ratio)))]

eu_vax[, partially_vaccinated_ratio:= partially_vaccinated_ratio[1],.
               (cumsum(!is.na(partially_vaccinated_ratio)))]

#Second, replace smaller values with previous value
eu_vax[, fully_vaccinated_ratio:= cummax(fully_vaccinated_ratio),
           by=.(country_region)]
            
eu_vax[, partially_vaccinated_ratio:= cummax(partially_vaccinated_ratio),
           by=.(country_region)]
      

#Create Data.Table with latest values 
latest_vax = eu_vax[date==max(eu_vax$date),.(country_region,
                                             fully_vaccinated_ratio
                                            )]
latest_deaths = eu_deaths[date==max(eu_vax$date),.(country,
                                              total_deaths_per_100000)]

latest = merge(latest_deaths,latest_vax, by.x='country', by.y='country_region')

tb1 = xtable(latest,caption = 'Deaths and Vaccinations of European Union 
             countries', type = "latex",display=c('d','s','d','f'))
names(tb1) = c('Country','Deaths per 100,000','Fully Vaccinated (%)')
          
print(tb1, file = "figs/table1.tex")



mean_fully_vax_rate = mean(latest$fully_vaccinated_ratio)
mean_deaths = mean(latest$total_deaths_per_100000)

latest$type_vax <- ifelse(latest$fully_vaccinated_ratio < mean_fully_vax_rate
                          , "below","above") 
latest$type_death <- ifelse(latest$total_deaths_per_100000 < mean_deaths
                          , "below","above") 

cor1= cor(latest$fully_vaccinated_ratio,latest$total_deaths_per_100000)

# Diverging Barcharts

latest <- latest[order(latest$fully_vaccinated_ratio), ]
latest$country<- factor(latest$country, levels =latest$country)
pdf(file="figs/plot5.pdf", width = 8, height = 5)
ggplot(latest, aes(x=country, y=fully_vaccinated_ratio, 
                       label=fully_vaccinated_ratio)) + 
  geom_bar(stat='identity', aes(fill=type_vax), width=.5) +
  scale_fill_manual(name="Compared to EU average", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(x='Country', y='Ratio (%)') + 
  coord_flip()
dev.off()

latest <- latest[order(-latest$total_deaths_per_100000), ]
latest$country<- factor(latest$country, levels =latest$country)
pdf(file="figs/plot6.pdf", width = 8, height = 5)
ggplot(latest, aes(x=country, y=total_deaths_per_100000, 
                       label=total_deaths_per_100000)) + 
  geom_bar(stat='identity', aes(fill=type_death), width=.5) +
  scale_fill_manual(name="Compared to EU average", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#f8766d", "below"="#00ba38")) + 
  labs(x='Country', y='Deaths per 100,000') + 
  coord_flip()
dev.off()

#Scatter plot
pdf(file="figs/plot77.pdf", width = 8, height = 4)
ggplot(latest, aes(x = fully_vaccinated_ratio, y = total_deaths_per_100000)) +
  labs(x='Vaccination Ratio (%)', y='Deaths per 100,000 after January 1st, 2021')+
  geom_point() +
  geom_smooth(color = "red")

dev.off()


#Repeat the analysis for Different Dates
#Cutoff Date February 1st

eu_vax = covid19_vaccine[country_region %in% eu_countries]
eu_deaths = coronavirus[country %in% eu_countries & type=='death' 
                        & date>='2021-02-01']


#calculate cumulative deaths and death ratios
eu_deaths[,total_deaths :=cumsum(cases),by=list(country)]
eu_deaths[,total_deaths_per_100000 := round(100000*total_deaths/population,1)]

#Create Data.Table with latest values 
latest_vax = eu_vax[date==max(eu_vax$date),.(country_region,
                                             fully_vaccinated_ratio)]
latest_deaths = eu_deaths[date==max(eu_vax$date),.(country,
                                                   total_deaths_per_100000)]

latest = merge(latest_deaths,latest_vax, by.x='country', by.y='country_region')
cor2=cor(latest$fully_vaccinated_ratio,latest$total_deaths_per_100000)

#Scatter Plot

pdf(file="figs/plot8.pdf", width = 8, height = 4)
ggplot(latest, aes(x = fully_vaccinated_ratio, y = total_deaths_per_100000)) +
  labs(x='Vaccination Ratio (%)', y='Deaths per 100,000 after February 1st, 2021')+
  geom_point() +
  geom_smooth(color = "red")
dev.off()

#Cutoff Date October 1st

eu_vax = covid19_vaccine[country_region %in% eu_countries]
eu_deaths = coronavirus[country %in% eu_countries & type=='death' 
                        & date>='2021-10-01']

#Keep columns of interest
eu_vax = eu_vax[,.(country_region,date,
                   population,fully_vaccinated_ratio,partially_vaccinated_ratio)]
eu_deaths = eu_deaths[,.(date,country,cases,population)]

#calculate cumulative deaths and death ratios
eu_deaths[,total_deaths :=cumsum(cases),by=list(country)]
eu_deaths[,total_deaths_per_100000 := round(100000*total_deaths/population,1)]

#Create Data.Table with latest values 
latest_vax = eu_vax[date==max(eu_vax$date),.(country_region,
                                             fully_vaccinated_ratio)]
latest_deaths = eu_deaths[date==max(eu_vax$date),.(country,
                                                   total_deaths_per_100000)]

latest = merge(latest_deaths,latest_vax, by.x='country', by.y='country_region')
cor3=cor(latest$fully_vaccinated_ratio,latest$total_deaths_per_100000)

#Scatter Plot

pdf(file="figs/plot9.pdf", width = 8, height = 4)
ggplot(latest, aes(x = fully_vaccinated_ratio, y = total_deaths_per_100000)) +
  labs(x='Vaccination Ratio (%)', y='Deaths per 100,000 after October 1st, 2021')+
  geom_point() +
  geom_smooth(color = "red")
dev.off()

#Plots
ggplot(eu_deaths[country %in% eu_countries],
       aes(x=date,y=total_deaths_per_100000, color=country)) +
  geom_line()+
  labs( x = "Date", y = "Total Deaths per 100,000 people", color='Country') 

ggplot(eu_vax[country_region %in% eu_countries],
       aes(x=date,y=fully_vaccinated_ratio, color=country_region)) +
  geom_line()+
  labs( x = "Date", y = "fully_vaccinated_ratio", color='Country') 

#find population of Greece
greece_pop = eu_vax[country_region=='Greece']$population[1]

#create list of European countries with similar population to Greece
countries = unique(eu_vax[population<=1.1*greece_pop & population>0.9*greece_pop]$country_region)

#plots
ggplot(eu_vax, aes(x=date,y=fully_vaccinated_ratio, color=country_region)) +
  geom_line()+
  labs( x = "Date", y = "Fully Vaccinated Ratio", color='Country') 

ggplot(eu_deaths[country %in% countries], aes(x=date,y=totaldeaths_per_100000, color=country)) +
  geom_line()+
  labs( x = "Date", y = "Deaths per 100,000 people", color='Country') 
