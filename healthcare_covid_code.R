###########################################################################
# Install packages. You only need to install once on the same device
###########################################################################
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('stargazer')
#install.packages('ggrepel')
#install.packages('WDI')
#install.packages('tidyverse')



###########################################################################
# Load packages. You need to load packages when start every new R session
###########################################################################
library("dplyr")
library("ggplot2")
library("stargazer")
library("ggrepel")
library("WDI")
library("tidyr")

###########################################################################
# Set working directory
setwd("/Users/kamilagoldyka/Desktop/uni/statistics/my pilot project")
###########################################################################

###########################################################################
# Prepare data set
###########################################################################

# Load vaccine data
cb.url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-codebook.csv"
codebook = read.csv(cb.url) %>% as_tibble()
codebook = subset(codebook, select = c(column, category))

'url = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
cov.data = read.csv(url)
write.csv(cov.data, "cov.data.csv", row.names = F)'

vac.data = read.csv("cov.data.csv")

table(vac.data$iso_code)
vac.data = vac.data %>% 
  dplyr::filter(!grepl("OWID_", iso_code))

vac.data = vac.data %>% 
  rename("iso3c" = iso_code) %>% 
  mutate(date = as.Date(date))

vac.data = vac.data %>% group_by(iso3c) %>%
  mutate_at(c("total_vaccinations", "people_vaccinated", "people_fully_vaccinated",
              "total_vaccinations_per_hundred", "people_vaccinated_per_hundred",
              "people_fully_vaccinated_per_hundred"), 
            max, na.rm = TRUE) %>%
  mutate(maxDate = max(date)) %>%
  as.data.frame()

vac.data = filter(vac.data, date == maxDate)

# Load WDI data
new_wdi_cache <- WDIcache() 

WDIsearch("Current health expenditure per capita, PPP (current international $)")
curr.spend = WDI(indicator="SH.XPD.CHEX.PP.CD" , 
                country="all", start=2019, end=2019, extra = T)

WDIsearch("Domestic general government health expenditure per capita, PPP (current international $)")
gov.spend.pc = WDI(indicator="SH.XPD.GHED.PP.CD" , 
                 country="all", start=2019, end=2019, extra = T)

WDIsearch("Domestic general government health expenditure (% of current health expenditure)")
gov.perc = WDI(indicator="SH.XPD.GHED.CH.ZS" , 
                   country="all", start=2019, end=2019, extra = T)

WDIsearch("Domestic private health expenditure (% of current health expenditure)")
priv.perc = WDI(indicator="SH.XPD.PVTD.CH.ZS" , 
               country="all", start=2019, end=2019, extra = T)

#wdi.data = left_join(curr.spend, gov.spend.pc, gov.perc, priv.perc, by = c("iso3c"))
wdi.data = left_join(curr.spend, gov.spend.pc, by = c("iso3c"))
wdi.data1 = left_join(wdi.data, gov.perc, by = c("iso3c"))
wdi.data2 = left_join(wdi.data1, priv.perc, by = c("iso3c"))
wdi.data2 = subset(wdi.data2, region.x != "Aggregates")
wdi.data2 = wdi.data2 %>% 
  dplyr::rename("curr.spend" = SH.XPD.CHEX.PP.CD, 
                "gov.spend.pc" = SH.XPD.GHED.PP.CD,
                "gov.perc" = SH.XPD.GHED.CH.ZS,
                "priv, perc" = SH.XPD.PVTD.CH.ZS)



wdi.data <- wdi.data2  
dplyr::rename("priv.perc" = priv, perc)


# Combine vac and WDI data
data = full_join(wdi.data, vac.data, by = c("iso3c"))

# export data
write.csv(data, "vac.WDI.csv", row.names = F)



###########################################################################
# Descrpitives
###########################################################################
#data = read.csv("vac.WDI.csv")

# Outcome (i.e. what to be analyzed)
data = data %>% mutate(outcome = (people_vaccinated_per_hundred))

#check data first
summary(data$outcome)
filter(data, is.na(outcome))
filter(data, outcome < 0)
# drop if outcome is missing
data = filter(data, outcome >= 0 & !is.na(curr.spend))

###################### Which country has vaccinated the most?

# Select top 10% countries
plot.data = data %>% 
  mutate(top = quantile(outcome, prob = .9, na.rm = T)) %>%
  filter(., outcome >= top) %>% select(-top)

ggplot(plot.data, aes(x = reorder(iso3c, outcome, sum),
                      y = outcome)) +
  geom_col(aes(fill = income.x), alpha = 0.8, width = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("People Vaccinated per 100")
ggsave("mostVacCountry.pdf", width = 6, height = 5)
ggsave("mostVacCountry.png", width = 6, height = 5)


# Select countries with top 10% spendings on healthcare

plot.data = data %>% 
  mutate(top = quantile(curr.spend, prob = .9, na.rm = T)) %>%
  filter(., curr.spend >= top) %>% select(-top)

ggplot(plot.data, aes(x = reorder(iso3c, curr.spend, sum),
                      y = curr.spend)) +
  geom_col(aes(fill = income.x), alpha = 0.8, width = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Current overall spendings on healthcare (per capita)")
ggsave("current.spendings.pdf", width = 6, height = 5)
ggsave("current.spendings.png", width = 6, height = 5)

# Select countries with top 10% government spendings on healthcare

plot.data = data %>% 
  mutate(top = quantile(gov.spend.pc, prob = .9, na.rm = T)) %>%
  filter(., gov.spend.pc >= top) %>% select(-top)

ggplot(plot.data, aes(x = reorder(iso3c, gov.spend.pc, sum),
                      y = gov.spend.pc)) +
  geom_col(aes(fill = income.x), alpha = 0.8, width = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Government overall spendings on healthcare (per capita)")
ggsave("government.spendings.pdf", width = 6, height = 5)
ggsave("government.spendings.png", width = 6, height = 5)


# Select countries with top 10% government input in spendings on healthcare

plot.data = data %>% 
  mutate(top = quantile(gov.perc, prob = .9, na.rm = T)) %>%
  filter(., gov.perc >= top) %>% select(-top)

ggplot(plot.data, aes(x = reorder(iso3c, gov.perc, sum),
                      y = gov.perc)) +
  geom_col(aes(fill = income.x), alpha = 0.8, width = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Government health expenditure (% of overall health expenditure)")
ggsave("government.perc.pdf", width = 6, height = 5)
ggsave("government.perc.png", width = 6, height = 5)


# Select countries with top 10% private input in spendings on healthcare

names(data)[names(data) == 'priv, perc'] <- 'priv.perc'

plot.data = data %>% 
  mutate(top = quantile(priv.perc, prob = .9, na.rm = T)) %>%
  filter(., priv.perc >= top) %>% select(-top)

ggplot(plot.data, aes(x = reorder(iso3c, priv.perc, sum),
                      y = priv.perc)) +
  geom_col(aes(fill = income.x), alpha = 0.8, width = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Private health expenditure (% of overall health expenditure)")
ggsave("private.perc.pdf", width = 6, height = 5)
ggsave("private.perc.png", width = 6, height = 5)



###########################################################################
# Regression
###########################################################################

# Transform outcome variable
scatter.data = data %>% filter(!is.na(outcome)) %>%
  mutate(outcome = log(outcome + 1)) # transform to log

ggplot(scatter.data, aes(x = outcome)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white") +
  geom_density(alpha = .2, fill = "antiquewhite3") +
  theme_minimal() +
  xlab("People Vaccinated per 100")
ggsave("logDist.pdf", width = 6, height = 5)

### Visualizing correlation
# gdp and vac
#ggplot(scatter.data, aes(x=log(gdp_per_capita), y=outcome) ) + 
  #geom_point(aes(color = income), size = 2, alpha = 0.5)+
  #geom_smooth(method=lm, se=FALSE) +
  #geom_text_repel(aes(color = income, label = iso3c), size = 2.5) +
  #theme_minimal() +
  #theme(legend.title = element_blank()) +
  #scale_fill_brewer(palette = "Dark2") +
  #scale_color_brewer(palette = "Dark2") +
  #ylab("People Vaccinated per 100 (log)")
#ggsave("gdp_vac.png", width = 6, height = 5)

# current spending and vac
ggplot(scatter.data, aes(x=log(curr.spend), y=outcome) ) + 
  geom_point(aes(color = income.x), size = 2, alpha = 0.5)+
  geom_smooth(method=lm, se=FALSE) +
  geom_text_repel(aes(color = income.x, label = iso3c), size = 2.5) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  ylab("People Vaccinated per 100 (log)") +
  xlab("Current spendings on healthcare (log)")
ggsave("current.spendings_vac.png", width = 6, height = 5)

# domestic government spending and vac
ggplot(scatter.data, aes(x=log(gov.spend.pc), y=outcome) ) + 
  geom_point(aes(color = income.x), size = 2, alpha = 0.5)+
  geom_smooth(method=lm, se=FALSE) +
  geom_text_repel(aes(color = income.x, label = iso3c), size = 2.5) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  ylab("People Vaccinated per 100 (log)") +
  xlab("Government spendings on healthcare per capita, PPP (log)")
ggsave("government.spendings_vac.png", width = 6, height = 5)

# domestic government spending and vac
ggplot(scatter.data, aes(x=log(gov.perc), y=outcome) ) + 
  geom_point(aes(color = income.x), size = 2, alpha = 0.5)+
  geom_smooth(method=lm, se=FALSE) +
  geom_text_repel(aes(color = income.x, label = iso3c), size = 2.5) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  ylab("People Vaccinated per 100 (log)") +
  xlab("Gov health expenditure (% of overall health expenditure) (log)")
ggsave("government.perc_vac.png", width = 6, height = 5)

# domestic government spending and vac
ggplot(scatter.data, aes(x=log(priv.perc), y=outcome) ) + 
  geom_point(aes(color = income.x), size = 2, alpha = 0.5)+
  geom_smooth(method=lm, se=FALSE) +
  geom_text_repel(aes(color = income.x, label = iso3c), size = 2.5) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  ylab("People Vaccinated per 100 (log)") +
  xlab("Private health expenditure (% of overall h.e.) (log)")
ggsave("priv.perc_vac.png", width = 6, height = 5)


# Standardize scale
scatter.data.standard = scatter.data %>% filter(!is.na(outcome)) %>% 
  gather(., key, value, curr.spend, gov.spend.pc, gov.perc, priv.perc)

ggplot(scatter.data.standard, aes(x=log(value), y=outcome) ) + 
  geom_point(aes(color = income.x), size = 2, alpha = 0.5)+
  geom_smooth(method=lm, se=FALSE) +
  geom_text_repel(aes(color = income.x, label = iso3c), size = 2.5) +
  facet_wrap(. ~ key) +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  ylab("People Vaccinated per 100 (log)") +
  xlab("Expenditures (log)")
ggsave("CorrelationPlot.pdf", width = 10, height = 4)
ggsave("CorrelationPlot.png", width = 5, height = 5)


# Estimation
Model1 = lm(outcome ~ log(curr.spend) , data = scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model2)

Model3 = lm(outcome ~ log(gov.perc) , scatter.data)
summary(Model3)

Model4 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model4)

Model.full = lm(outcome ~ log(curr.spend) + 
                  log(gov.spend.pc) +
                  log(gov.perc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model3, Model4, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model3, Model4, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results_28.doc")
stargazer(Model1, Model2, Model3, Model4, Model.full,
          type = "latex", omit.stat=c("LL","ser","f"), out = "results.tex")


# Estimation with 4
Model1 = lm(outcome ~ log(curr.spend) , data = scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model2)

Model3 = lm(outcome ~ log(gov.perc) , scatter.data)
summary(Model3)

Model4 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model4)

Model.full = lm(outcome ~ log(curr.spend) + 
                  log(gov.spend.pc) +
                  log(gov.perc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model3, Model4, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model3, Model4, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results.full.doc")
stargazer(Model1, Model2, Model3, Model4, Model.full,
          type = "latex", omit.stat=c("LL","ser","f"), out = "results.full.tex")

#estimation without gov.spend.pc

Model1 = lm(outcome ~ log(curr.spend) , data = scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(gov.perc) , scatter.data)
summary(Model3)

Model3 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model4)

Model.full = lm(outcome ~ log(curr.spend) + 
                  log(gov.perc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results1.doc")
stargazer(Model1, Model2, Model.full, 
          type = "latex", omit.stat=c("LL","ser","f"), out = "results1.tex")






#estimation without gov.perc
Model1 = lm(outcome ~ log(curr.spend) , data = scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model2)

Model3 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model3)

Model.full = lm(outcome ~ log(curr.spend) + 
                  log(gov.spend.pc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model3, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model3, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results_without.gov.perc.doc")
stargazer(Model1, Model2, Model3, Model.full, 
          type = "latex", omit.stat=c("LL","ser","f"), out = "results_without.gov.perc.tex")


# Estimation with just gov.spend.pc and priv.perc
Model1 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model2)

Model.full = lm(outcome ~ log(gov.spend.pc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results_gov.spend.pc_and_priv.perc.doc")
stargazer(Model1, Model2, Model.full,
          type = "latex", omit.stat=c("LL","ser","f"), out = "results_gov.spend.pc_and_priv.perc.tex")



# Estimation final
Model1 = lm(outcome ~ log(curr.spend) , data = scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model2)

Model3 = lm(outcome ~ log(gov.perc) , scatter.data)
summary(Model3)

Model4 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model4)

Model_perc = lm(outcome ~ log(gov.perc) + 
                  log(priv.perc), scatter.data)
summary(Model_perc)

Model.full = lm(outcome ~ log(curr.spend) + 
                  log(gov.spend.pc) +
                  log(gov.perc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model3, Model4, Model_perc, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model3, Model4, Model_perc, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results.final.doc")
stargazer(Model1, Model2, Model3, Model4, Model_perc, Model.full,
          type = "latex", omit.stat=c("LL","ser","f"), out = "results.final.tex")
stargazer(Model1, Model2, Model3, Model4, Model_perc, Model.full,
          type = "latex", omit.stat=c("LL","ser","f"), out = "results.final.png")


######

Model1 = lm(outcome ~ log(curr.spend) , data = scatter.data)
summary(Model1)

Model2 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model2)

Model3 = lm(outcome ~ log(gov.perc) , scatter.data)
summary(Model3)

Model4 = lm(outcome ~ log(priv.perc) , scatter.data)
summary(Model4)


Model_perc = lm(outcome ~ log(gov.perc) + 
                  log(priv.perc), scatter.data)
summary(Model_perc)


Model5 = lm(outcome ~ log(curr.spend) + 
                  log(gov.perc), scatter.data)
summary(Model5)


Model6 = lm(outcome ~ log(curr.spend) + 
                  log(gov.spend.pc), scatter.data)
summary(Model6)

Model7 = lm(outcome ~ log(gov.perc) + 
              log(gov.spend.pc), scatter.data)
summary(Model7)


Model.full = lm(outcome ~ log(curr.spend) + 
                  log(gov.spend.pc) +
                  log(gov.perc) +
                  log(priv.perc), scatter.data)
summary(Model.full)

stargazer(Model1, Model2, Model3, Model4, Model_perc, Model5, Model6, Model7, Model.full,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model1, Model2, Model3, Model4, Model_perc, Model5, Model6, Model7, Model.full,
          type = "text", omit.stat=c("LL","ser","f"), out = "results_full28.doc")
stargazer(Model1, Model2, Model3, Model4, Model_perc, Model5, Model6, Model7, Model.full,
          type = "latex", omit.stat=c("LL","ser","f"), out = "results.tex")


###### Model with gov spend pc is the best. But here i have to look for an error. 
###### Errors that may occur, according to literature, is GDP per capita and population age.

# Load WDI data
new_wdi_cache <- WDIcache() 

WDIsearch("	Life expectancy at birth, total (years)")
life.expect = WDI(indicator="SP.DYN.LE00.IN" , 
                 country="all", start=2019, end=2019, extra = T)

WDIsearch("GDP per capita (current US$)")
gdp.pc = WDI(indicator="NY.GDP.PCAP.CD" , 
                   country="all", start=2019, end=2019, extra = T)

#wdi.data.final -> so i can make a table
wdi.data.26 = left_join(life.expect, gdp.pc, by = c("iso3c"))
wdi.data26 = subset(wdi.data, region.x != "Aggregates")
wdi.data.final = left_join(wdi.data2, wdi.data.26, by = c("iso3c"))
wdi.data.final = wdi.data.final %>% 
  dplyr::rename("life.expect" = SP.DYN.LE00.IN, 
                "gdp.pc" = NY.GDP.PCAP.CD)

####

# Combine vac and WDI data
data = full_join(wdi.data.final, vac.data, by = c("iso3c"))

# export data
write.csv(data, "vac.WDI.csv", row.names = F)



###########################################################################
# Descrpitives
###########################################################################
#data = read.csv("vac.WDI.csv")

# Outcome (i.e. what to be analyzed)
data = data %>% mutate(outcome = (people_vaccinated_per_hundred))

#check data first
summary(data$outcome)
filter(data, is.na(outcome))
filter(data, outcome < 0)
# drop if outcome is missing
data = filter(data, outcome >= 0 & !is.na(curr.spend))

scatter.data = data %>% filter(!is.na(outcome)) %>%
  mutate(outcome = log(outcome + 1)) # transform to log

#### making a table, looking for an error

Model2 = lm(outcome ~ log(gov.spend.pc) , scatter.data)
summary(Model2)

Model26_1 = lm(outcome ~ log(life.expect) , scatter.data)
summary(Model26_1)

Model26_2 = lm(outcome ~ log(gdp.pc), scatter.data)
summary(Model26_2)

Model26_all = lm(outcome ~ log(gov.spend.pc) +
                   log(life.expect) +
                   log(gdp.pc), scatter.data)
summary(Model26_all)

Model28_three = lm(outcome ~ log(gov.spend.pc) +
                   log(life.expect), scatter.data)
summary(Model28_three)


stargazer(Model2, Model26_1, Model26_2, Model26_all, Model28_three,
          type = "text", omit.stat=c("LL","ser","f"))
stargazer(Model2, Model26_1, Model26_2, Model26_all, Model28_three,
          type = "text", omit.stat=c("LL","ser","f"), out = "results_error28.doc")

stargazer(Model2, Model26_1, Model26_2, Model26_all Model28_three,
          type = "text", omit.stat=c("LL","ser","f"), out = "results_error28.doc")

