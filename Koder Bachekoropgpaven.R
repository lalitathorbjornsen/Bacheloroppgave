#Laste ned pakkene
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(knitr)
library(cowplot)
library(readr)
library(stats) # regresjonsmodell


# Laste med datasettet frs NSD
NSD2995 <- read_csv("NSD2995.csv")

#Filter vekk resual og vet ikke
df <- NSD2995 %>%
  select(A1a, ts_stor, alder, SKH14, H85, kjoenn, utdnivaa, # Viktige variabler
         saminnt, SKH7, #Økonomiske forhold
         H13, SKH8, SKH9, #Helsemessig forhold
         C15, SKH11, H56, #Sosiale forhold
         H84, H80, # Naturskjønnhet
         H47) %>% #Tillit
  filter(ts_stor != 9 & ts_stor != 99 & A1a!= 98 & A1a!= 99 & # Viktige
           SKH14!=98 & SKH14!=99 & H85!= 98 & H85!= 99 & utdnivaa!= 0 & utdnivaa!= 9 &  # viktige
           saminnt!=98 & saminnt!=99 & SKH7!=98 & SKH7!=99 & # Økonomiske forhold           
           H13!=8 & H13!=9 & SKH8!= 98 & SKH8!= 99 & SKH9!= 98 & SKH9!= 99 & #Helsemessig forhold
           C15!=8 & C15!=9 & SKH11!= 98 & SKH11!= 99 & H56!= 8 & H56!= 9 & # Sosiale forhold
           H84!=8 & H84!=9 & H80!=8 & H80!=9 & # Naturskjønnhet
           H47!=98 & H47!=99) #Tillit
df


#fjerne NA fra datasettet
df <- na.omit(df)

#Legge på kolonner; alder opphøyd i 2
df <- df %>% 
  mutate(alder_2 = alder^2)
df

#Lage kolonne med 3 kategorier
df$bosted <-case_when(
  df$ts_stor %in% c(11, 12, 13, 14) ~ "Tettsted",
  df$ts_stor %in% c(15) ~ "By",
  TRUE ~ "Storby")
df

#Se om verdiene er factor og endre
df$bosted <- as.factor(df$bosted)
is.factor(df$bosted)

#endre utdanning
df$utdanning <-case_when(
  df$utdnivaa %in% c(1,2) ~ "Grunnskole",
  df$utdnivaa %in% c(3,4,5) ~ "Videregående skole",
  df$utdnivaa %in% c(6) ~ "Kort høyere utdanning",
  df$utdnivaa %in% c(7) ~ "Lang høyere utdanning",
  TRUE ~ "Forskernivå (20+)")

df$utdanning <- as.factor(df$utdanning)

#Endre kjoenn, H84 og H80 til binære variabler
# df$kjoenn <- as.numeric(df$kjoenn)
df$kjoenn <- ifelse(df$kjoenn == 1, 0, 1) # VET IKKE OM DET ER RIKTIG
df$kjoenn <- as.factor(df$kjoenn)

df$H84 <- as.numeric(df$H84)
df$H80 <- as.numeric(df$H80)
df$H80 <- ifelse(df$H80 == 1, 1, 0)
df$H84 <- ifelse(df$H84 == 1, 1, 0)

# Oppdatering av verdier i H56 - Hvor ofte er du sammen med gode venner?
df$H56 <- factor(df$H56, ordered = TRUE,
                 levels = c(6, 5, 4, 3, 2, 1),
                 labels = c("Daglig", 
                            "Hver uke", 
                            "Hver måned", 
                            "Noen ganger i året", 
                            "Sjeldnere", 
                            "Har ingen gode venner"))

# Oppdatering av verdier i C15 - Jeg ha følt nærhet med andre (dikotom)
df$C15 <- factor(df$C15,
                 ordered = TRUE,
                 levels = c(1, 2, 3, 4, 5),
                 labels = c("ikke i det hele tatt",
                            "sjelden",
                            "en del av tiden",
                            "ofte",
                            "hele tiden"))

#Gjøre saminnt i tusen istedenfor
df <- df %>%
  mutate(saminnt_1000 = saminnt / 1000)


# Laste ned pakkene til beskrivende data
library(vtable)
library(kableExtra)

# Velg ut de variabler vi skal ha med i tabellen
beskrivende_data <- subset(df, select = c("A1a", "SKH14", "H85", "alder", "alder_2",
                                          "bosted", "kjoenn", "utdanning", #Viktige 
                                          "saminnt", "saminnt_1000", "SKH7", #Økonomiske forhold
                                          "H13", "SKH8", "SKH9", #Helsemessig forhold
                                          "C15", "SKH11", "H56", #Sosiale forhold
                                          "H84", "H80", # Naturskjønnhet
                                          "H47")) # tillit


# Gi beskrivende navn til variablene (i samme rekkefølge som de ligger i datasettet)
labs <- c("Alt i alt - Hvor førnøyd er du med livet ditt?","Hvor fornøyd er du med stedet (strøket/bygda/bydelen) du bor?",
          "I hvilken grad føler du at du hører til på stedet der du bor?", "Alder", "Alder^2", "Bosted", "Kjønn", "Utdanning",
          "Samlet inntekt", "Samlet inntekt i tusen", "Hvor fornøyd er du med din økonomiske situasjon?", # Økonomiske forhold
          "Hvordan vurderer du din helse alt i alt?", "Hvordan vurderer du din fysisk helse?", "Hvordan vurderer du din mentale helse?", # Helsemessig
          "Jeg har følt nærhet til andre mennesker", "Hvor fornøyd er du med forholdet du har til dine venner?",  #Sosiale
          "Hvor ofte er du sammen med gode venner?",
          "Finnes det et turterreng innen 500 meter fra boligen? ", "Støy fra naboer eller annen støy utefra, f.eks. fra trafikk, industri eller anlegg?", #Naturskjønnhet
          "I hvilken grad vil du si at det politiske systemet i Norge gi folk som deg innflytelse på det myndighetene gjør?" #Tillit
          
)

# Lag tabellen for å vise den beskrivende dataene
st(beskrivende_data, labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Beskriv hvilken statistikk du ønsker å vise
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') # Gi navn til kolumnene
   ))

#Endre referansevariablene
df$bosted <- relevel(df$bosted, ref = "Tettsted")
df$utdanning <- relevel(df$utdanning, ref = "Grunnskole")

# Laste ned pakkene til regresjonsmpdell
library(sjPlot)
library(sjmisc)
library(sjlabelled)

#Reg med tettsted, liten by og storby tall
reg1 <- lm(A1a ~ bosted, data = df) #A1a vs Bosted
summary(reg1)

reg2 <- lm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning, data = df) #Viktigste variablene
summary(reg2)

reg3 <- lm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7, data = df) # Økonomiske forhold
summary(reg3)

reg4 <- lm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56, data = df) # Sosiale forhold
summary(reg4)

reg5 <- lm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56 + H13 + SKH8 + SKH9, data = df) # helsemessige forhold 
summary(reg5) 

reg6 <- lm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56 + H13 + SKH8 + SKH9 + H84 + H80, data = df) #naturskjønnhet
summary(reg6)

reg7 <- lm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56 + H13 + SKH8 + SKH9 + H84 + H80 + H47, data = df) #tillit
summary(reg7)


#evt endre navn på variablene som viser i tabellen
pl <- c(
  `(Intercept)` = "Skjæringspunkt",
  "bosted" = "Bosted",
  "kjoenn1" = "Kvinne", 
  "alder" = "Alder", 
  "alder_2" = "Alder^2", 
  "SKH14" = "Hvor fornøyd er du med stedet (støket/bygda/bydelen) du bor i?",
  "H85" = "I hvilken grad føler du at du hører til på stedet der du bor?",
  "saminnt_1000" = "Samlet inntekt (i tusen)",
  "SKH7" = "Hvor fornøyd er du med din økonomiske situasjon?",
  "C15.L" = "Jeg har følt nærhet til andre mennesker",
  "SKH11" = "Hvor fornøyd er du med forholdet du har til dine venner?",
  "H56.L" = "Hvor ofte er du sammen med gode venner?",
  "H13" = "Hvordan vurderer du din helse alt i alt?",
  "SKH8" = "Hvordan vurderer du din fysisk helse?",
  "SKH9" = "Hvordan vurderer du din mentale helse?",
  "H84" = "Finnes det et turterreng innen 500 meter fra boligen?",
  "H80" = "Støy fra naboer eller annen støy utefra, f.eks. fra trafikk, industri eller anlegg?",
  "H47" = "I hvilken grad vil du si at det politiske systemet i Norge gi folk som deg innflytelse på det myndighetene gjør?"
)

# tabellen
model1 <- tab_model(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
                    pred.labels = pl,
                    string.est = "Estimat",
                    string.ci = "KI",
                    rm.terms = c("C15.C", "C15.Q", "C15^4",
                                 "H56.Q","H56.C","H56^4","H56^5"),
                    dv.labels = c("Model 1: Bosted", "Model 2: + Viktige variabler", "Model 3: + Økonomiske forhold",
                                   "Model 4: + Sosiale forhold", "Model 5: + Helsemessig forhold", "Model 6: + Naturskjønnhet", "Model 7: + Tillit"),  
                    p.style = "stars",
                    digits = 4)

model1

library(car)
vif(reg7)

# til robustanalyse
library(MASS)

#robustanalyse av modellene
reg1rob <- rlm(A1a ~ bosted, data = df) #A1a vs Bosted
summary(reg1rob)

reg2rob <- rlm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning, data = df) #Viktigste variablene
summary(reg2rob)

reg3rob <- rlm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7, data = df) # Økonomiske forhold
summary(reg3rob)

reg4rob <- rlm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56, data = df) # Sosiale forhold
summary(reg4rob)

reg5rob <- rlm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56 + H13 + SKH8 + SKH9, data = df) # helsemessige forhold 
summary(reg5rob) 

reg6rob <- rlm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56 + H13 + SKH8 + SKH9 + H84 + H80, data = df) #naturskjønnhet
summary(reg6rob)

reg7rob <- rlm(A1a ~ bosted + kjoenn + alder + alder_2 + SKH14 + H85 + utdanning + saminnt_1000 + SKH7 + C15 + SKH11 + H56 + H13 + SKH8 + SKH9 + H84 + H80 + H47, data = df) #tillit
summary(reg7rob)

# tabellen
model_robust <- tab_model(reg1rob, reg2rob, reg3rob, reg4rob, reg5rob, reg6rob, reg7rob,
                          pred.labels = pl,
                          string.est = "Estimat",
                          string.ci = "KI",
                          rm.terms = c("C15.C", "C15.Q", "C15^4",
                                       "H56.Q","H56.C","H56^4","H56^5"),
                          dv.labels = c("Model 1: Bosted", "Model 2: + Viktige variabler", "Model 3: + Økonomiske forhold",
                                        "Model 4: + Sosiale forhold", "Model 5: + Helsemessig forhold", "Model 6: + Naturskjønnhet", "Model 7: + Tillit"),  
                          p.style = "stars",
                          digits = 4)
model_robust

#finn residual standard error av ols modellen 
summary(reg7)$sigma

# Lage nytt dataframe med vanlig modell
RSE <- data.frame(Model1 = summary(reg1)$sigma)
RSE$Model2 <- summary(reg2)$sigma
RSE$Model3 <- summary(reg3)$sigma
RSE$Model4 <- summary(reg4)$sigma
RSE$Model5 <- summary(reg5)$sigma
RSE$Model6 <- summary(reg6)$sigma
RSE$Model7 <- summary(reg7)$sigma

# Transpose the RSE dataset
RSE <- t(RSE)

#residual standard error av ols modellen
summary(reg7rob)$sigma

# lage nytt dataframe med robust modelen 
RSE2 <- data.frame(rModel1 = summary(reg1rob)$sigma)
RSE2$Model2 <- summary(reg2rob)$sigma
RSE2$Model3 <- summary(reg3rob)$sigma
RSE2$Model4 <- summary(reg4rob)$sigma
RSE2$Model5 <- summary(reg5rob)$sigma
RSE2$Model6 <- summary(reg6rob)$sigma
RSE2$Model7 <- summary(reg7rob)$sigma
# Transpose the RSE dataset
RSE2 <- t(RSE2)


combined <- cbind(RSE, RSE2)
colnames(combined) <- c("RSE vanlig", "RSE robust")
print(combined)

combined <- tibble(combined)
combined <- as_numeric(combined)
print(combined)

combined <- tibble(
  "RSE" = c("Model1", "Model2", "Model3", "Model4", "Model5", "Model6", "Model7"),
  "RSE vanlig" = round(c(2.220232, 2.014329, 1.884039, 1.744829, 1.562009, 1.562061, 1.560389), 2),
  "RSE robust" = round(c(1.706254, 1.850923, 1.770318, 1.580457, 1.381961, 1.380072, 1.379826), 2)
)

print(combined)

#lage til tabell
print(combined)
kable(combined, format = "html") %>%
  kable_styling(full_width = FALSE)


# sjekke
sigma(reg1, reg2, reg3, reg4, reg5, reg6, reg7)
sigma(reg1rob, reg2rob, reg3rob, reg4rob, reg5rob, reg6rob, reg7rob)

#---------------------------------------------------------------------
#Grafer til oppgaven 

# testing av gjennomsnitt og medianen
# Bruk aggregate-funksjonen til å beregne gjennomsnittet av variabel1 for hver unike verdi av variabel2
mean <- aggregate(A1a ~ alder, data = df, FUN = mean)
median <- aggregate(A1a ~ alder, data = df, FUN = median)

# Vis resultatet
print(mean)
print(median)

#prøve ny måte
gjennomsnitt <- df %>%
  group_by(bosted) %>%
  summarise(A1a = mean(A1a))

#fargekoder til grafene
fargekoder <- c(Tettsted = "#FF6961", By = "#77DD77", Storby = "#1E90FF")

#A1a vs bosted
p_bosted <- ggplot(gjennomsnitt, aes(x = bosted, y = A1a, fill = bosted)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(A1a, 2)), vjust = -0.5) +
  labs(x = "bosted", 
       y = "Gjennomsnitt av A1a - Alt i alt. Hvor fornøyd er du med livet ditt for tiden?",
       title = "Gjennomsnitt av A1a - Alt i alt. Hvor fornøyd er du med livet ditt for tiden? for hvert bosted") + 
  scale_y_continuous(limits = c(0, 10),
                     breaks = seq(from = 0, to = 10, by = 1)) + 
  scale_fill_manual(values = fargekoder) +
  theme_economist()
p_bosted

#Tettsted, by, storby
p_omrade <- df %>%
  ggplot(aes(x=bosted, fill = bosted)) + 
  geom_bar() + 
  labs(x ="Bosted",
       y="Antall observasjoner",
       title="Bosted") +
  theme_economist()
p_omrade

# SKH14 -  Hvor fornøyd er du med stedet(strøket/bygda/bydelen) du bor i?
# SKH14 vs bosted
average_SKH14 <- df %>%
  group_by(bosted) %>%
  summarise(SKH14 = mean(SKH14))

p_average_SKH14 <- ggplot(average_SKH14, aes(x = bosted, y = SKH14, fill = bosted)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(SKH14, 2)), vjust = -0.5) +
  labs(x = "bosted", 
       y = "SKH14 - Hvor fornøyd er du med stedet (strøket/bygda/bydelen) du bor i?",
       title = "Gjennomsnitt av SKH14 - Hvor fornøyd er du med stedet (strøket/bygda/bydelen) du bor i??") + 
  scale_y_continuous(limits = c(0, 10),
                     breaks = seq(from = 0, to = 10, by = 1)) + 
  scale_fill_manual(values = fargekoder) +
  theme_economist()
p_average_SKH14

#H85 vs bosted
average_h85 <- df %>%
  group_by(bosted) %>%
  summarise(H85 = mean(H85))


pH85 <- ggplot(average_h85, aes(x = bosted, y = H85, fill = bosted)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(H85, 2)), vjust = -0.5) +
  labs(x = "bosted", 
       y = "H85 - I hvilken grad føler du at du hører til på stedet der du bor?",
       title = "Gjennomsnitt av H85 - I hvilken grad føler du at du hører til på stedet der du bor?") + 
  scale_y_continuous(limits = c(0, 10),
                     breaks = seq(from = 0, to = 10, by = 1)) + 
  scale_fill_manual(values = fargekoder) +
  theme_economist()
pH85


