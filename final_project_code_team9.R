library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(rworldmap)
library(rstatix)
library(ggpubr)
library(tidyverse)
library(tidymodels)
library(randomForest)
library(randomForestExplainer)

setwd("C:/Users/LeeHyejin/Desktop/R_project")#set to where the "dataset" directory is".


# II-1. Our World In Data COVID-19 dataset

owid_covid <- read.csv("dataset/owid-covid-data.csv")
testing_policy.df <- read.csv("dataset/covid-19-testing-policy.csv")
contact_tracing.df <- read.csv("dataset/covid-contact-tracing.csv")
face_covering.df <- read.csv("dataset/face-covering-policies-covid.csv")
internal_movement.df <- read.csv("dataset/internal-movement-covid.csv")
international_travel.df <- read.csv("dataset/international-travel-covid.csv")
public_events.df <- read.csv("dataset/public-events-covid.csv")
public_gathering.df <- read.csv("dataset/public-gathering-rules-covid.csv")
public_transport.df <- read.csv("dataset/public-transport-covid.csv")
school_closure.df <- read.csv("dataset/school-closures-covid.csv")
stay_at_home.df <- read.csv("dataset/stay-at-home-covid.csv")
workplace_closure.df <- read.csv("dataset/workplace-closures-covid.csv")

testing_policy.df <- rename(testing_policy.df, iso_code = Code, date = Day) %>% select(-Entity)
contact_tracing.df <- rename(contact_tracing.df, iso_code = Code, date = Day) %>% select(-Entity)
face_covering.df <- rename(face_covering.df, iso_code = Code, date = Day) %>% select(-Entity)
internal_movement.df <- rename(internal_movement.df, iso_code = Code, date = Day) %>% select(-Entity)
international_travel.df <- rename(international_travel.df, iso_code = Code, date = Day) %>% select(-Entity)
public_events.df <- rename(public_events.df, iso_code = Code, date = Day) %>% select(-Entity)
public_gathering.df <- rename(public_gathering.df, iso_code = Code, date = Day) %>% select(-Entity)
public_transport.df <- rename(public_transport.df, iso_code = Code, date = Day) %>% select(-Entity)
school_closure.df <- rename(school_closure.df, iso_code = Code, date = Day) %>% select(-Entity)
stay_at_home.df <- rename(stay_at_home.df, iso_code = Code, date = Day) %>% select(-Entity)
workplace_closure.df <- rename(workplace_closure.df, iso_code = Code, date = Day) %>% select(-Entity)

full_dataset <- owid_covid %>% 
  inner_join(testing_policy.df, by = c("iso_code", "date"))%>%
  inner_join(contact_tracing.df, by = c("iso_code", "date"))%>%
  inner_join(face_covering.df, by = c("iso_code", "date"))%>%
  inner_join(internal_movement.df, by = c("iso_code", "date"))%>%
  inner_join(international_travel.df, by = c("iso_code", "date"))%>%
  inner_join(public_events.df, by = c("iso_code", "date"))%>%
  inner_join(public_gathering.df, by = c("iso_code", "date"))%>%
  inner_join(public_transport.df, by = c("iso_code", "date"))%>%
  inner_join(school_closure.df, by = c("iso_code", "date"))%>%
  inner_join(stay_at_home.df, by = c("iso_code", "date")) %>%
  inner_join(workplace_closure.df, by = c("iso_code", "date")) %>%
  mutate(date = as.Date(date))

colnames(owid_covid)

owid_covid %>% 
  select(new_cases, new_deaths, icu_patients, hosp_patients) %>%
  summary()

# II-2. VAERS (Vaccine Adverse Effect Report System) dataset  

vaersdata <- read.csv("dataset/2021VAERSData.csv")
vaersvax <- read.csv("dataset/2021VAERSVAX.csv")
vaerssym <- read.csv("dataset/2021VAERSSYMPTOMS.csv")

vaersvax <- vaersvax %>%
  filter(VAX_TYPE == "COVID19") %>%
  select(VAERS_ID, VAX_TYPE, VAX_MANU)

vaerssym <- vaerssym %>%
  select(VAERS_ID, SYMPTOM1, SYMPTOM2, SYMPTOM3,SYMPTOM4,SYMPTOM5)

vaersdata <- vaersdata %>%
  select(VAERS_ID, CAGE_YR, SEX)

full_vacc_dataset <- vaersdata %>%
  inner_join(vaersvax, by = "VAERS_ID") %>%
  inner_join(vaerssym, by = "VAERS_ID")

tidy_vacc <- full_vacc_dataset %>% 
  gather(key = "temp", value = "symptom", "SYMPTOM1", "SYMPTOM2", "SYMPTOM3", "SYMPTOM4", "SYMPTOM5") %>%
  filter(!is.na(symptom)) %>%
  filter(symptom != "") %>%
  select(-c(temp, VAX_TYPE)) 

# III-1. Data overview

full_dataset %>% filter(date == as.Date("2021-5-21")) -> recentdata 

mapped_data <- joinCountryData2Map(recentdata, joinCode = 'ISO3', nameJoinColumn = "iso_code")
par(mfrow=c(2,2))
mapCountryData(mapped_data,nameColumnToPlot='total_cases_per_million', mapTitle ="COVID-19 cases by country")
mapCountryData(mapped_data, nameColumnToPlot = 'total_deaths_per_million', mapTitle = 'COVID_19 deaths by country')
mapCountryData(mapped_data, nameColumnToPlot = 'total_vaccinations_per_hundred', mapTitle = 'COVID-19 vaccination by country')

data_CHL <- full_dataset %>%
  filter(location == "Chile" ) %>%
  filter(date > as.Date("2020-12-16")) 
data_ISR <- full_dataset %>%
  filter(location == "Israel" ) %>%
  filter(date > as.Date("2020-12-16")) 
data_GBR <- full_dataset %>%
  filter(location == "United Kingdom" ) %>%
  filter(date > as.Date("2020-12-16")) 
data_USA <- full_dataset %>%
  filter(location == "United States" ) %>%
  filter(date > as.Date("2020-12-16")) 

top4_vaccine <- ggplot() +
  geom_line(data = data_CHL, aes(x = date, y = people_vaccinated_per_hundred, colour = "Chile")) +
  geom_line(data = data_ISR, aes(x = date, y = people_vaccinated_per_hundred, colour = "Israel")) +
  geom_line(data = data_GBR, aes(x = date, y = people_vaccinated_per_hundred, colour = "United Kingdom")) +
  geom_line(data = data_USA, aes(x = date, y = people_vaccinated_per_hundred, colour = "United States"))+
  labs(title="Top 4 vaccinated countries",x ="Date", y = "people vaccinated per hundred")

KOR_vaccine <- full_dataset %>%
  filter(iso_code == "KOR") %>%
  filter(date > as.Date("2020-12-16")) %>%
  ggplot() +
  geom_line(aes(x = date, y = people_vaccinated_per_hundred)) +
  labs(title = "Korea vaccinated", x = "Date", y = "people vaccinated per hundred") -> KOR_vaccine

grid.arrange(top4_vaccine, KOR_vaccine, ncol = 1)


# III-2. Does vaccination really alleviates COVID-19 seriousness?

full_dataset %>% 
  filter(!is.na(people_vaccinated)) %>%
  select(date) %>%
  summary()

start_date <- as.Date("2021-05-3"); end_date <- as.Date("2021-05-16")
after_vaccination <- owid_covid %>%
  filter(date >= start_date & date <= end_date) %>%
  filter(population >= 5000000) %>%
  group_by(iso_code) %>%
  summarise(new_cases = mean(new_cases_per_million, na.rm = TRUE),
            new_deaths = mean(new_deaths_per_million, na.rm = TRUE),
            people_vaccinated = max(people_vaccinated_per_hundred),
            people_fully_vaccinated = max(people_fully_vaccinated_per_hundred)
  ) %>%
  na.omit()

start_date_2 <- as.Date("2021-1-3"); end_date_2 <- as.Date("2021-12-01")
before_vaccination <- owid_covid %>%
  filter(date >= start_date_2 & date <= end_date_2) %>%
  group_by(iso_code) %>%
  summarise(new_cases = max(total_cases_per_million),
            new_deaths = max(total_deaths_per_million),
  ) %>%
  na.omit()

#join two tibbles
compare <- inner_join(before_vaccination, after_vaccination, by="iso_code")


#make new variables
new_compare <- compare %>%
  mutate(case_change = new_cases.y / new_cases.x * 100,
         death_change = new_deaths.y / new_deaths.x * 100)
new_compare <- new_compare %>%
  filter(!is.na(death_change))
```


Then, we wanted to compare the effect of vaccination categorically. Countires were assigned a new variable "vaccination_progress". If the variable "people_vaccinated" is under 20, then it was assigned low. Between 20 and 50 were assigned middle, and over 50 was assigned as high.

```{r echo = FALSE,  warning = FALSE}
new.df <- new_compare %>% 
  mutate(vaccination_progress = ifelse(people_vaccinated < 20, "low", 
                                       ifelse(people_vaccinated > 50, "high", "middle"))) %>%
  group_by(vaccination_progress)

new.df$vaccination_progress <- factor(new.df$vaccination_progress, levels = c("low", "middle", "high"))

table(new.df$vaccination_progress)

my_comparisons <- list( c("low", "middle"), c("low", "high"), c("middle", "high") )

case_plot <- ggboxplot(new.df, x = "vaccination_progress", y = "case_change",
                       color = "vaccination_progress", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", method.args = list(var.equal = FALSE))

death_plot <- ggboxplot(new.df, x = "vaccination_progress", y = "death_change",
                        color = "vaccination_progress", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", method.args = list(var.equal = FALSE))

grid.arrange(case_plot, death_plot, ncol = 2)



# III-3. What is more important between vaccination and several social distancing policies?

# Preprocessing    

standard = as.Date("2020-02-3")

full_dataset <- full_dataset %>% 
  mutate(week = ceiling(as.numeric(date - standard)/7)) %>%
  mutate(week = ifelse(week<0, NA, week)) %>%
  filter(!is.na(week))

full_dataset$testing_policy <- as.numeric(full_dataset$testing_policy)
full_dataset$contact_tracing <- as.numeric(full_dataset$contact_tracing)
full_dataset$restrictions_internal_movements <- as.numeric(full_dataset$restrictions_internal_movements)
full_dataset$restriction_gatherings <- as.numeric(full_dataset$restriction_gatherings)
full_dataset$close_public_transport <- as.numeric(full_dataset$close_public_transport)
full_dataset$school_closures <- as.numeric(full_dataset$school_closures)
full_dataset$stay_home_requirements <- as.numeric(full_dataset$stay_home_requirements)
full_dataset$workplace_closures <- as.numeric(full_dataset$workplace_closures)


week.data <- full_dataset %>% 
  unite(iso_week, iso_code, week, remove = F) %>%
  group_by(iso_week) %>%
  summarise(week = first(week),
            iso = first(iso_code),
            new_cases = mean(new_cases_per_million, na.rm = TRUE),
            new_deaths = mean(new_deaths_per_million, na.rm = TRUE),
            reproduction_rate = mean(reproduction_rate, na.rm = TRUE),
            people_vaccinated = mean(people_vaccinated_per_hundred, na.rm = TRUE),
            people_fully_vaccinated = mean(people_fully_vaccinated_per_hundred, na.rm = TRUE),
            contact_tracing = mean(contact_tracing, na.rm = TRUE),
            facial_coverings = mean(facial_coverings, na.rm = TRUE),
            restrictions_internal_movements = mean(restrictions_internal_movements, na.rm = TRUE),
            international_travel_controls = mean(international_travel_controls, na.rm = TRUE),
            cancel_public_events = mean(cancel_public_events, na.rm = TRUE),
            restriction_gatherings = mean(restriction_gatherings, na.rm = TRUE),
            close_public_transport = mean(close_public_transport, na.rm = TRUE),
            school_closures = mean(school_closures, na.rm = TRUE),
            stay_home_requirements = mean(stay_home_requirements, na.rm = TRUE),
            workplace_closures = mean(workplace_closures, na.rm = TRUE),
            stringency = mean(stringency_index, na.rm = TRUE))

diff <- 3
mut_week.data <- week.data %>%
  filter(!is.na(new_cases)) %>%
  mutate(cases_3weeks = ifelse(paste(iso, week+diff, sep = "_") %in% week.data$iso_week,{
    std_week <- week
    filter(week.data, iso_week == paste(iso, std_week + diff, sep="_"))$new_cases},NA)) %>%
  mutate(deaths_3weeks = ifelse(paste(iso, week+diff, sep = "_") %in% week.data$iso_week,{
    std_week <- week
    filter(week.data, iso_week == paste(iso, std_week + diff, sep="_"))$new_deaths},NA)) 

mut_week.data$case_change <- mut_week.data$cases_3weeks / mut_week.data$new_cases
mut_week.data$death_change <- mut_week.data$deaths_3weeks / mut_week.data$new_deaths

mut_week.data <-  mut_week.data %>% 
  filter(week>10) %>% 
  mutate(people_vaccinated = ifelse(is.na(people_vaccinated), 0, people_vaccinated)) %>%
  mutate(people_fully_vaccinated = ifelse(is.na(people_fully_vaccinated), 0, people_fully_vaccinated)) %>%
  filter(case_change != Inf & death_change != Inf) %>%
  na.omit()

# Model 1. linear regression for the whole world data  

model1.analysis <- lm(case_change~people_vaccinated + contact_tracing+facial_coverings+restrictions_internal_movements+cancel_public_events+restriction_gatherings+close_public_transport+school_closures+stay_home_requirements+workplace_closures,
                      data = mut_week.data)
summary(model1.analysis)

# Model 2. linear regression for Isreal data 

model2.df <- mut_week.data %>%
  filter(iso == "ISR")

model2.analysis <- lm(case_change~people_vaccinated + contact_tracing+facial_coverings+restrictions_internal_movements+cancel_public_events+restriction_gatherings+close_public_transport+school_closures+stay_home_requirements+workplace_closures,
                      data = model2.df)

summary(model2.analysis)

model2_1.analysis <- lm(case_change~ people_vaccinated + contact_tracing+ close_public_transport,
data = model2.df)

summary(model2_1.analysis)

# Model 3. Random Forest for Israel data 

set.seed(423) # 34, 423, 466, ""508"", 987
model2.df %>% initial_split(prop=0.7) -> model3_split

model3_split %>% training() %>%
  recipe(case_change~people_vaccinated + contact_tracing+
           facial_coverings+restrictions_internal_movements+
           cancel_public_events+restriction_gatherings+
           close_public_transport+school_closures+
           stay_home_requirements+workplace_closures,
         data = model2.df)  %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> model3.recipe

model3.recipe %>%
  bake(model3_split %>% testing()) -> model3.test

model3.recipe %>%
  juice() -> model3.train

set.seed(1234)
model3 <- randomForest(case_change~people_vaccinated + contact_tracing+facial_coverings+restrictions_internal_movements+cancel_public_events+restriction_gatherings+close_public_transport+school_closures+workplace_closures,
                       data=model3.train, mtry = 3, importance = TRUE)

model3 %>%
  predict(model3.test) %>%
  bind_cols(model3.test) %>%
  metrics(truth=case_change, estimate=...1)

varImpPlot(model3, type=1)

# III-4. Which adverse effects are common in the vaccines?

tidy_vacc %>%
  filter(substr(VAX_MANU, 1, 6) == "PFIZER") %>%
  filter(symptom != "")-> pfizer

tidy_vacc %>%
  filter(VAX_MANU== "MODERNA")  %>%
  filter(symptom != "")-> moderna

pfizer %>%
  group_by(symptom) %>%
  summarise(freq = n()/ nrow(pfizer) * 100) %>%
  arrange(desc(freq)) %>%
  head(20)

moderna %>%
  group_by(symptom) %>%
  summarise(freq = n()/ nrow(moderna) * 100) %>%
  arrange(desc(freq)) %>%
  head(20)

moderna_over_65 <- 25.3 * 125508313 / 100
moderna_under_65 <- 74.7 * 125508313 / 100
pfizer_over_65 <- 25 * 161050513 / 100
pfizer_under_65 <- 75 * 161050513 / 100


tidy_vacc %>%
  group_by(SEX) %>%
  summarise(number = n())

vacc_by_age_group <- tidy_vacc %>%
  mutate(age_group = ifelse(CAGE_YR>=65, "over_65", "under_65"))
vacc_by_age_group %>% 
  group_by(age_group) %>%
  summarise(number = n())

moderna_by_age_group <- moderna %>%
  mutate(age_group = ifelse(CAGE_YR>=65, "over_65", "under_65"))
moderna_by_age_group %>%
  group_by(age_group) %>%
  summarise(number = n()) %>%
  na.omit() %>%
  cbind(total_vac = c(moderna_over_65, moderna_under_65)) %>%
  mutate(symp_freq = number / total_vac * 100)

pfizer_by_age_group <- pfizer %>%
  mutate(age_group = ifelse(CAGE_YR>=65, "over_65", "under_65"))
pfizer_by_age_group %>%
  group_by(age_group) %>%
  summarise(number = n()) %>%
  na.omit() %>%
  cbind(total_vac = c(pfizer_over_65, pfizer_under_65)) %>%
  mutate(symp_freq = number / total_vac * 100)

pfizer_over_65 <- 
  pfizer_by_age_group %>%
  filter(age_group == "over_65") 
pfizer_over_65 %>%
  group_by(symptom) %>%
  summarize(freq = n()/ nrow(pfizer_over_65)* 100) %>%
  arrange(desc(freq)) %>%
  head(20)

pfizer_under_65 <- pfizer_by_age_group %>%
  filter(age_group == "under_65") 
pfizer_under_65 %>%
  group_by(symptom) %>%
  summarize(freq = n() / nrow(pfizer_under_65) * 100) %>%
  arrange(desc(freq)) 

# III-5. Can we have less stringent social distancing after vaccination?

plot1 <- full_dataset %>% 
  filter(iso_code == "CHL")%>%
  filter(date > as.Date("2020-4-1")) %>%
  ggplot(aes(x = date, y = new_cases_smoothed)) +
  geom_line()

plot2 <- full_dataset %>% 
  filter(iso_code == "CHL") %>%
  filter(date > as.Date("2020-4-1")) %>%
  ggplot(aes(x = date, y = stringency_index)) +
  geom_line()

plot3 <- full_dataset %>% 
  filter(iso_code == "CHL") %>%
  filter(date > as.Date("2020-4-1")) %>%
  ggplot(aes(x = date, y = people_vaccinated_per_hundred)) +
  geom_line()


grid.arrange(plot1, plot2, plot3, ncol=1)

plot1 <- full_dataset %>% 
  filter(iso_code == "ISR")%>%
  filter(date > as.Date("2020-4-1")) %>%
  ggplot(aes(x = date, y = new_cases_smoothed)) +
  geom_line()

plot2 <- full_dataset %>% 
  filter(iso_code == "ISR") %>%
  filter(date > as.Date("2020-4-1")) %>%
  ggplot(aes(x = date, y = stringency_index)) +
  geom_line()

plot3 <- full_dataset %>% 
  filter(iso_code == "ISR") %>%
  filter(date > as.Date("2020-4-1")) %>%
  ggplot(aes(x = date, y = people_vaccinated_per_hundred)) +
  geom_line()


grid.arrange(plot1, plot2, plot3, ncol=1)



