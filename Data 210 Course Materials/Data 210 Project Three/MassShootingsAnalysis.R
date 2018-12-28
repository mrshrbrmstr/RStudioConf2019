library(readxl)
library(tidyverse)
# install.packages("janitor")
library(janitor)
library(plyr)
library(dplyr)
library(gridExtra)

mass_shootings <- read_excel("~/Dropbox/Great Bay ALL/GBCC Data 210/Project Three/MJ_US_Mass_Shootings_1982-2018_updated.xlsx")
View(mass_shootings)
# This section or block of code is entirely meant to deal with the City and State
# variables. Since technically DC is not listed as a state, we needed to include code
# that assigned the two-digit abbreviation "DC" along with the other 50 states that are
# listed in the vector "state.abb" <-- built-in to the pre-existing R data files
# Also pre-existing is "state.name". Print them out and see they include all 50 states

dat2 = sapply(mass_shootings$Location, strsplit, ",")
dat2 = lapply(dat2, trimws)
dat2 = data.frame(matrix(unlist(dat2), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
names(dat2) = c("City", "State")

'%!in%' <- function(x,y) (  !(x %in% y)  )

dat2$State <- gsub(".","",dat2$State, fixed=TRUE)

State_abb <- state.abb[as.integer(match(dat2$State,state.name))]
State_abb <- State_abb[!is.na(State_abb)]

dat2$State[dat2$State %!in% state.abb & dat2$State != "DC"] <- State_abb

mass_shooting_final <-
# mass_shootings %>%
  janitor::clean_names(mass_shootings) %>%
  mutate(city = dat2$City) %>%
  mutate(state = dat2$State) %>%
  mutate(year) %>%
  # mutate(date = Date) %>%
  mutate(total_victims = str_replace_all(total_victims, "[^[:digit:]]", "")) %>% # make total victims numeric
  mutate(total_victims = as.integer(total_victims)) %>%
  mutate(venue = trimws(tolower(venue))) %>% # normalize venue
  mutate(race = trimws(tolower(race))) %>% # normalize race
  mutate(gender = trimws(tolower(gender))) %>% # clean up/normalize gender
  mutate(gender = str_replace(gender, "male", "m")) %>% 
  mutate(gender = str_replace(gender, "fe.*", "f")) %>%
  mutate(age = as.numeric(str_extract(summary, "\\d\\d"))) %>%
  mutate(prior_signs_of_mental_health_issues = trimws(tolower(prior_signs_of_mental_health_issues))) %>%
  mutate(prior_signs_of_mental_health_issues = str_replace(prior_signs_of_mental_health_issues, 
                                                           "(.*)yes(.*)", "y")) %>%
  mutate(prior_signs_of_mental_health_issues = str_replace(prior_signs_of_mental_health_issues, 
                                                           "(.*)no(.*)", "n")) %>%
  mutate(weapons_obtained_legally = trimws(tolower(weapons_obtained_legally))) %>%
  
  mutate(weapons_obtained_legally = str_replace(weapons_obtained_legally, 
                                                "(.*)yes(.*)", "y")) %>%
  mutate(weapons_obtained_legally = str_replace(weapons_obtained_legally, 
                                                "(.*)pass(.*)", "y")) %>%
  mutate(weapons_obtained_legally = str_replace(weapons_obtained_legally, 
                                                "no", "n")) %>%
  mutate(weapons_obtained_legally = str_replace(weapons_obtained_legally, 
                                                "unknwn", "tbd")) %>%
  mutate(used_rifle = str_detect(type_of_weapons, fixed("rifle", ignore_case=TRUE))) %>% # rifle used?
  mutate(used_handgun = str_detect(type_of_weapons, regex("handgun|revolv|derringer|pistol", ignore_case=TRUE))) %>% # handgun used?
  mutate(used_knife = str_detect(type_of_weapons, fixed("kni", ignore_case=TRUE))) %>% # knife used?
  mutate(used_shotgun = str_detect(type_of_weapons, fixed("shotgun", ignore_case=TRUE))) %>% # shotgun used?
  mutate(semiauto_used = str_detect(type_of_weapons, fixed("semi", ignore_case=TRUE))) %>% # semi-auto guns used?
  mutate(assault_style = str_detect(type_of_weapons, fixed("assau", ignore_case=TRUE))) %>%  # assault-style weaopns used? 
  mutate(ar15_used = str_detect(weapon_details, "(.*)AR[-]*(15|55)(.*)")) %>%
  mutate(wasr10_used = str_detect(weapon_details, "(.*)WASR[-]*10(.*)")) %>%
  mutate(ak47_used = str_detect(weapon_details, "(.*)AK[-]*(47|74)(.*)")) %>%
  mutate(mcx_used = str_detect(weapon_details, "(.*)MCX(.*)")) %>%
  mutate(mp15_used = str_detect(weapon_details, "(.*)M&P15(.*)")) %>%
  mutate(xm15_used = str_detect(weapon_details, "(.*)(XM15|XM[ ]15)(.*)")) %>%
  mutate(assault_rifle_used = ar15_used | ak47_used | wasr10_used | mcx_used | mp15_used | xm15_used)

# mass_shooting_final$weapons_obtained_legally[4] <- "y" 
# mass_shooting_final$weapons_obtained_legally[24] <- "y"
# The following code is intended to encode variables that are "TRUE" or "FALSE" as factors
# with more descriptive text showing what "TRUE" means and what "FALSE" means

mass_shooting_final$used_rifle <- factor(mass_shooting_final$used_rifle, levels = c(TRUE, FALSE),
                                         labels = c("rifle", "no rifle"))
mass_shooting_final$used_handgun <- factor(mass_shooting_final$used_handgun, levels = c(TRUE, FALSE),
                                         labels = c("handgun", "no handgun"))
mass_shooting_final$used_shotgun <- factor(mass_shooting_final$used_shotgun, levels = c(TRUE, FALSE),
                                         labels = c("shotgun", "no shotgun"))
mass_shooting_final$semiauto_used <- factor(mass_shooting_final$semiauto_used, levels = c(TRUE, FALSE),
                                         labels = c("semiautomatic", "semiautomatic no?"))
mass_shooting_final$assault_style <- factor(mass_shooting_final$assault_style, levels = c(TRUE, FALSE),
                                         labels = c("assault style", "assault style no?"))
mass_shooting_final$ar15_used <- factor(mass_shooting_final$ar15_used, levels = c(TRUE, FALSE),
                                            labels = c("AR-style used", "Other"))
mass_shooting_final$ak47_used <- factor(mass_shooting_final$ak47_used, levels = c(TRUE, FALSE),
                                        labels = c("AK-style used", "Other"))
mass_shooting_final$wasr10_used <- factor(mass_shooting_final$wasr10_used, levels = c(TRUE, FALSE),
                                        labels = c("WASR-10 used", "Other"))
mass_shooting_final$mcx_used <- factor(mass_shooting_final$mcx_used, levels = c(TRUE, FALSE),
                                          labels = c("Sig MCX used", "Other"))
mass_shooting_final$mp15_used <- factor(mass_shooting_final$mp15_used, levels = c(TRUE, FALSE),
                                       labels = c("S & W M&P15 used", "Other"))
mass_shooting_final$xm15_used <- factor(mass_shooting_final$xm15_used, levels = c(TRUE, FALSE),
                                        labels = c("Bushmaster XM15 used", "Other"))
mass_shooting_final$assault_rifle_used <- factor(mass_shooting_final$assault_rifle_used, levels = c(TRUE, FALSE),
                                        labels = c("Assault rifle", "Other"))

mass_shooting_final$prior_signs_of_mental_health_issues <- 
  factor(mass_shooting_final$prior_signs_of_mental_health_issues, 
        levels = c("y", "n", "tbd", "unclear"),
        labels = c("prior mental health", "no mental health flag", "mental health tbd", "mental health unclear"))

mass_shooting_final$weapons_obtained_legally <- 
  factor(mass_shooting_final$weapons_obtained_legally, 
        levels = c("y", "n", "tbd"),
        labels = c("legal", "not legal", "legal tbd"))

data_fatalities <- log(mass_shooting_final$fatalities)/log(2)
cond <- c(rep(3,62), rep(2,16), rep(1,19))
# cond <- factor(cond, levels = c(1,2,3), labels = c("Before Ban", "During Ban", "After Ban"))
grouped_data <- as.data.frame(cbind(cond, data_fatalities))
grouped_data$cond <- factor(cond, levels = c(1,2,3), labels = c("12 yrs Before Ban", "10 yrs During Ban", "12 yrs After Ban"))
mass_shooting_final <- cbind(mass_shooting_final, grouped_data)


g14 <- ggplot(mass_shooting_final, aes(x=data_fatalities, fill=cond)) +
  geom_histogram(binwidth=1.0, alpha=.5, position="dodge") +
  guides(fill=guide_legend(title="Time Spans")) +
  xlab("Fatalities (Log Base 2 of)") +
  ylab("Number of Incidents At Each Level of Fatality")
# g14 <- g14 +title("Mass Shooting Fatalities By Time Period Based On Assault Weapons Ban")
  # + geom_vline(data=ddply(grouped_data, "cond", summarise, fatalities.mean=mean(data_fatalities)), 
             # aes(xintercept=fatalities.mean,  
             #    colour=cond),
             # linetype="dashed", size=1)
print(g14)

g15 <- ggplot(mass_shooting_final, aes(x=cond, y=fatalities)) +
  geom_boxplot() +
  # guides(fill=guide_legend(title="Time Spans Relative To Federal Ban")) +
  xlab("Time Spans Relative To Federal Ban") +
  ylab("Distribution of Fatalities Each Time Period")
# g15 <- g15 +title("Mass Shooting Fatalities By Time Period Based On Assault Weapons Ban")
# + geom_vline(data=ddply(grouped_data, "cond", summarise, fatalities.mean=mean(data_fatalities)), 
# aes(xintercept=fatalities.mean,  
#    colour=cond),
# linetype="dashed", size=1)
print(g15)

g16 <- ggplot(mass_shooting_final, aes(x=cond, y=log(total_victims))) +
  geom_boxplot() +
  # guides(fill=guide_legend(title="Time Spans Relative To Federal Ban")) +
  xlab("Time Spans Relative To Federal Ban") +
  ylab("Distribution of Victims (log) Each Time Period")

mass_shooting_final$fatalities <- 
  factor(as.character(as.integer((mass_shooting_final$fatalities/10) + 1)), 
         levels = c("1","2","3","4","5", "6"),
         labels = c("up to 9","10 to 19","20 to 29","30 to 39","40 to 49", "50 and higher"))

g1 <- qplot(year, total_victims, data=mass_shooting_final, shape=assault_style, color=assault_style,
      log = "y",
      facets = prior_signs_of_mental_health_issues ~ weapons_obtained_legally,
      size = I(3), xlab = "Year", ylab = "Number of Victims (log)")
print(g1)

g2 <- qplot(year, total_victims, data=mass_shooting_final, shape=semiauto_used, color=semiauto_used,
           log = "y",
           facets = prior_signs_of_mental_health_issues ~ weapons_obtained_legally,
           size = I(3), xlab = "Year", ylab = "Number of Victims (log)")
print(g2)

g3 <- qplot(year, total_victims, data=mass_shooting_final, shape=semiauto_used, color=assault_style,
            log = "y",
            facets = prior_signs_of_mental_health_issues ~ weapons_obtained_legally,
            size = I(3), xlab = "Year", ylab = "Number of Victims (log)")
print(g3)

g4 <- qplot(year, age, data=mass_shooting_final, color=fatalities,
            main = "Shootings By Year and Age Split By Mental Health and Weapon Legality",
            facets = prior_signs_of_mental_health_issues ~ weapons_obtained_legally,
            size = fatalities, xlab = "Year", ylab = "Age of Shooter")
print(g4)

g5 <- qplot(year, age, data=mass_shooting_final, shape=semiauto_used, color=fatalities,
            facets = prior_signs_of_mental_health_issues ~ weapons_obtained_legally,
            size = I(3), xlab = "Year", ylab = "Age of Shooter")
print(g5)

g6 <- qplot(year, log(total_victims), data=mass_shooting_final, shape=weapons_obtained_legally,
             main = "All Mass Shootings Overall", color=assault_rifle_used,
             size = fatalities, xlab = "Year", ylab = "Number of Victims (log)") + theme(legend.position="none")
 print(g6)

data_after_1999 <- mass_shooting_final[mass_shooting_final$year > 1999,]
data_after_2007 <- mass_shooting_final[mass_shooting_final$year > 2007,]
data_during_assault_ban <- mass_shooting_final[1994 < mass_shooting_final$year & mass_shooting_final$year < 2005,]
data_after_1994 <- mass_shooting_final[mass_shooting_final$year > 1994,]

g7 <- qplot(age, log(total_victims), data=data_after_1994, color=weapons_obtained_legally,
            main = "Shootings After 1994", ylim=c(0,8),
            size = fatalities, xlab = "Age of Shooter", ylab = "Number of Victims (log)")
# print(g7)
mod8 <- lm(age ~ total_victims, data=data_during_assault_ban)
g8 <- qplot(age, log(total_victims), data=data_during_assault_ban, color=assault_rifle_used,
            main = "Shootings During Assault Weapons Ban Enacted 1994 (10 Years)", ylim=c(0,8), 
            size = fatalities, shape=weapons_obtained_legally,
            xlab = "Age of Shooter", ylab = "Number of Victims (log)") 
g81 <- qplot(resid(mod8), fitted(mod8), data=data_during_assault_ban,
            main = "Shootings During Assault Weapons Ban Enacted 1994 (10 Years)",
            xlab = "Age of Shooter", ylab = "Number of Victims (log)")
            # + theme(legend.position="none")
# print(g8)

g9 <- qplot(age, log(total_victims), data=data_after_2007, color=assault_rifle_used,
            main = "Shootings 2008 - 2017 (10 Years)", ylim=c(0,8), shape=weapons_obtained_legally,
            size = fatalities, xlab = "Age of Shooter", ylab = "Number of Victims (log)")

grid.arrange(g8,g9,g15,g14, nrow=2)
grid.arrange(g16,g9,g15,g14, nrow=2)

g10 <- qplot(year, log(total_victims), data=data_after_1994, shape=weapons_obtained_legally,
            main = "Shootings After 1994", ylim=c(0,8), color=assault_rifle_used,
            # opts(legend.position="none"),
            # theme(legend.position="none"),
            size = fatalities, xlab = "Year", ylab = "Number of Victims (log)") + theme(legend.position="none")
# print(g10)

g11 <- qplot(year, log(total_victims), data=data_during_assault_ban, shape=weapons_obtained_legally,
             main = "Public Mass Shootings During Ban Years vs Victims", ylim=c(0,8), color=assault_rifle_used,
             # opts(legend.position="none"),
             # theme(legend.position="none"),
             size = fatalities, xlab = "Year", ylab = "Number of Victims (log)") + theme(legend.position="none")
# print(g11)

g12 <- qplot(year, log(total_victims), data=data_after_2007, shape=weapons_obtained_legally,
             main = "Public Mass Shootings After Ban Lifted Year vs Victims", ylim=c(0,8), color=assault_rifle_used,
             size = fatalities, xlab = "Year", ylab = "Number of Victims (log)")
# print(g12)

grid.arrange(g10,g11,g12,nrow=2)