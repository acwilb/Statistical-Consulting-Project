library(dplyr)
library(ggplot2)

setwd("~/STATS 141XP")

######## EDA ##########

# Reading in Cleaned Data
data2019 <- read.csv("DOPA_pre_post_2019.csv")
data2019 <- data2019 %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor)


# Distribution of Zip Codes -----------------------------------------------

data2019 %>% filter(Survey == "Pre") %>% 
  ggplot(aes(x = Zipcode, fill = Race)) +
  geom_bar(stat = "count") +
  coord_flip() +
  scale_fill_manual(values = c("cornflowerblue", "dodgerblue4")) +
  labs(y = "Frequency", x = "Zipcode", color = "Race") +
  ggtitle("Distribution of Zipcode by Race") 


# Pre-Visit EDA -----------------------------------------------------------


pre_survey <- data2019 %>% filter(Survey == "Pre")
post_survey <- data2019 %>% filter(Survey == "Post")

# Distribution of Race - Pre-Visit Survey
count(pre_survey, Race)

# Counts of Harmfulness Ratings
h1.c <- xtabs(~ Hal.Harmful + Race, data = pre_survey)
h2.c <- xtabs(~ MDMA.Harmful + Race, data = pre_survey)
h3.c <- xtabs(~ GHB.Harmful + Race, data = pre_survey)

# Counts of Addictiveness Ratings
a1.c <- xtabs(~ Hal.Addictive + Race, data = pre_survey)
a2.c <- xtabs(~ MDMA.Addictive + Race, data = pre_survey)
a3.c <- xtabs(~ GHB.Addictive + Race, data = pre_survey)

# Hallucinogens Harmfulness Rating by Race
pre_survey %>% group_by(Race) %>%
  count(Hal.Harmful) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = Hal.Harmful, y = prop, fill = Race)) +
  geom_bar(stat="identity") + labs(x = "Rating", y = "Proportion", title = "Hallucinogens Harmfulness Rating by Race") +
  scale_fill_manual(values = c("darkkhaki", "darkolivegreen"))

# Hallucinogens Addictiveness Rating by Race
pre_survey %>% group_by(Race) %>%
  count(Hal.Addictive) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = Hal.Addictive, y = prop, fill = Race)) +
  geom_bar(stat="identity") + labs(x = "Rating", y = "Proportion", title = "Hallucinogens Addictiveness Rating by Race") +
  scale_fill_manual(values = c("darkkhaki", "darkolivegreen"))

# MDMA Harmfulness Rating by Race
pre_survey %>% group_by(Race) %>%
  count(MDMA.Harmful) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = MDMA.Harmful, y = prop, fill = Race)) +
  geom_bar(stat="identity") + labs(x = "Rating", y = "Proportion", title = "MDMA Harmfulness Rating by Race") +
  scale_fill_manual(values = c("coral3", "coral"))

# MDMA Addictiveness Rating by Race
pre_survey %>% group_by(Race) %>%
  count(MDMA.Addictive) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = MDMA.Addictive, y = prop, fill = Race)) +
  geom_bar(stat="identity") + labs(x = "Rating", y = "Proportion", title = "MDMA Addictiveness Rating by Race") +
  scale_fill_manual(values = c("coral3", "coral"))

# GHB Harmfulness Rating by Race
pre_survey %>% group_by(Race) %>%
  count(GHB.Harmful) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = GHB.Harmful, y = prop, fill = Race)) +
  geom_bar(stat="identity") + labs(x = "Rating", y = "Proportion", title = "GHB Harmfulness Rating by Race") +
  scale_fill_manual(values = c("darkslategray4", "darkslategray3"))

# GHB Addictiveness Rating by Race
pre_survey %>% group_by(Race) %>%
  count(GHB.Addictive) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = GHB.Addictive, y = prop, fill = Race)) +
  geom_bar(stat="identity") + labs(x = "Rating", y = "Proportion", title = "GHB Addictiveness Rating by Race") +
  scale_fill_manual(values = c("darkslategray4", "darkslategray3"))


# Changes from Pre-Visit to Post-Visit ----------------------------------------


data2019_changes <- data.frame(
  Hal.Harm = rep(0, 47),
  Hal.Add = rep(0, 47),
  M.Harm = rep(0, 47),
  M.Add = rep(0, 47),
  G.Harm = rep(0, 47), 
  G.Add = rep(0, 47)
)

for (i in 1:nrow(data2019_changes)){
  data2019_changes$Hal.Harm[i] <- post_survey$Hal.Harmful[i] - pre_survey$Hal.Harmful[i]
  data2019_changes$Hal.Add[i] <- post_survey$Hal.Addictive[i] - pre_survey$Hal.Addictive[i]
  data2019_changes$M.Harm[i] <- post_survey$MDMA.Harmful[i] - pre_survey$MDMA.Harmful[i]
  data2019_changes$M.Add[i] <- post_survey$MDMA.Addictive[i] - pre_survey$MDMA.Addictive[i]
  data2019_changes$G.Harm[i] <- post_survey$GHB.Harmful[i] - pre_survey$GHB.Harmful[i]
  data2019_changes$G.Add[i] <- post_survey$GHB.Addictive[i] - pre_survey$GHB.Addictive[i]
  
  if (is.na(data2019_changes$Hal.Harm[i]) == TRUE){
    data2019_changes$Hal.Harm[i] <- 0
  }
  if (is.na(data2019_changes$Hal.Add[i]) == TRUE){
    data2019_changes$Hal.Add[i] <- 0
  }
  if (is.na(data2019_changes$M.Harm[i]) == TRUE){
    data2019_changes$M.Harm[i] <- 0
  }
  if (is.na(data2019_changes$M.Add[i]) == TRUE){
    data2019_changes$M.Add[i] <- 0
  }
  
  if (is.na(data2019_changes$G.Harm[i]) == TRUE){
    data2019_changes$G.Harm[i] <- 0
  }
  if (is.na(data2019_changes$G.Add[i]) == TRUE){
    data2019_changes$G.Add[i] <- 0
  }
}

#write.csv(data2019_changes, "data2019_changes_bycat.csv", row.names=FALSE)
cat.changes <- read.csv("data2019_changes_bycat.csv")
cat.changes$DrugCat <- factor(cat.changes$DrugCat, levels = c("Hallucinogens", "MDMA", "GHB"))

# Change in Harmfulness Rating of All Categories
cat.changes %>% 
  group_by(DrugCat) %>% 
  ggplot(aes(x = factor(Harmful), fill = DrugCat)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Change in Harmfulness Rating") + 
  labs(x = "Rating", y = "Frequency", fill = "Category") + 
  scale_fill_manual(values = c("darkolivegreen", "coral", "darkslategray3"))

# Change in Addictiveness Rating of All Categories
cat.changes %>% 
  group_by(DrugCat) %>% 
  ggplot(aes(x = factor(Addictive), fill = DrugCat)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Change in Addictiveness Rating") + 
  labs(x = "Rating", y = "Frequency", fill = "Category") + 
  scale_fill_manual(values = c("darkolivegreen", "coral", "darkslategray3"))
