library(dplyr)

setwd("~/STATS 141XP")


# Pre-Visit Data ---------------------------------------------------------


# Reading in Data - Spring 2019 Pre-Visit
data19pre <- read.csv("Spring 2019 (Pre-covid) Pre Visit.csv")

# removing NA row
data19pre <- na.omit(data19pre)

# Extracting Party Drugs Variables of Interest
party.drugs.pre <- data19pre[, c(50:51, 55:56, 60:61, 71:75)]
party.drugs.pre$Survey <- rep("Pre", nrow(party.drugs.pre))

# Renaming Variables
party.data.pre <- party.drugs.pre %>%
  rename(
    Hal.Harmful = How.harmful.do.you.think.this.drug.is..9,
    Hal.Addictive = How.addictive.is.this.drug..9,
    MDMA.Harmful = How.harmful.do.you.think.this.drug.is..10,
    MDMA.Addictive = How.addictive.is.this.drug..10,
    GHB.Harmful = How.harmful.do.you.think.this.drug.is..11,
    GHB.Addictive = How.addictive.is.this.drug..11,
    Race = What.is.your.race.or.ethnicity..Please.check.all.groups.you.identify.with.,
    Zipcode = What.is.your.zip.code.,
    ID.Q1 = What.are.the.first.three.letters.of.your.mother.s.name.,
    ID.Q2 = What.is.your.favorite.color.,
    ID.Q3 = What.is.your.favorite.number.
  )

# Converting to Factor
party.data.pre <- party.data.pre %>% mutate_if(is.numeric, as.factor) %>% mutate_if(is.character, as.factor)

# Recoding Invalid Zipcodes
party.data.pre[,8] <- recode_factor(party.data.pre[,8], "0" = "90034") # recoding to most frequent zipcode
party.data.pre[,8] <- recode_factor(party.data.pre[,8], "9004" = "90004")

# Reordering
party.data.pre <- party.data.pre %>% select(12, 8, 7, 1, 2, 3, 4, 5, 6, 9, 10, 11)


# Post-Visit Data --------------------------------------------------------------


# Reading in Data - Spring 2019 Post-Visit
data19post <- read.csv("Spring 2019 (Pre-covid) Post Visit.csv")

# Extracting Party Drugs Variables of Interest
party.drugs.post <- data19post[, c(39:40, 43:44, 47:48, 60:63)]
party.drugs.post$Survey <- rep("Post", nrow(party.drugs.post))
party.drugs.post$Zipcode <- rep(NA, nrow(party.drugs.post))

# Renaming Variables
party.data.post <- party.drugs.post %>%
  rename(
    Hal.Harmful = How.harmful.do.you.think.this.drug.is..9,
    Hal.Addictive = How.addictive.do.you.think.this.drug.is..9,
    MDMA.Harmful = How.harmful.do.you.think.this.drug.is..10,
    MDMA.Addictive = How.addictive.do.you.think.this.drug.is..10,
    GHB.Harmful = How.harmful.do.you.think.this.drug.is..11,
    GHB.Addictive = How.addictive.do.you.think.this.drug.is..11,
    Race = What.is.your.race.or.ethnicity..Please.check.all.groups.you.identify.with.,
    ID.Q1 = What.are.the.first.three.letters.of.your.mother.s.name.,
    ID.Q2 = What.is.your.favorite.color.,
    ID.Q3 = What.is.your.favorite.number.
  )

# Converting to Factor
party.data.post <- party.data.post %>% mutate_if(is.numeric, as.factor) %>% mutate_if(is.character, as.factor)
#party.data[,7] <- as.factor(party.data[,7])

# Reordering
party.data.post <- party.data.post %>% select(11, 12, 7, 1, 2, 3, 4, 5, 6, 8, 9 , 10)


# Categorize Race - Hispanic or Not Hispanic ------------------------------


# Combine Pre-Visit and Post-Visit Data
pre_post_2019 <- rbind(party.data.pre, party.data.post)

# Hispanic or Not Hispanic - Pre-Visit Data
pre_post_2019 <- pre_post_2019 %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor)

# Removing observations that overlap between Hispanic or Latino & Others
overlap <- c("Asian, Hispanic or Latino, White", "Decline to state",
             "Hispanic or Latino, White", "American Indian or Alaska Native, Hispanic or Latino, White", "Mix")
for (i in 1:nrow(pre_post_2019)){
  if (pre_post_2019[i, 3] %in% overlap){
    pre_post_2019 <- pre_post_2019 %>% filter(Race != pre_post_2019[i, 3])
  }
}

# Recoding other races to "Not Hispanic or Latino"
not.hispanic <- c("Black or African American", "White", "Asian", "Native Hawaiian or Other Pacific Islander", "Asian, Black or African American", "Asian, Native Hawaiian or Other Pacific Islander", "White, Armenian", "White, egyptian", "Asian, Native Hawaiian or Other Pacific Islander, White")
for (i in 1:nrow(pre_post_2019)){
  if (pre_post_2019[i, 3] %in% not.hispanic){
    pre_post_2019 <- pre_post_2019 %>% transform(Race = gsub(pre_post_2019[i, 3], "Not Hispanic or Latino", Race))
  }
  dup <- c("Not Hispanic or Latino, Not Hispanic or Latino", "Not Hispanic or Latino American", "Not Hispanic or Latino, Armenian", "Not Hispanic or Latino, egyptian ", "Not Hispanic or Latino, Not Hispanic or Latino, Not Hispanic or Latino")
  if (pre_post_2019[i, 3] %in% dup){
    pre_post_2019 <- pre_post_2019 %>% transform(Race = gsub(pre_post_2019[i, 3], "Not Hispanic or Latino", Race))
  }
  if (pre_post_2019[i, 3] != "Hispanic or Latino"){
    pre_post_2019[i, 3] <- "Not Hispanic or Latino"
  }
}


# Cleaned + Combined Dataset ----------------------------------------------


write.csv(pre_post_2019, "DOPA_pre_post_2019.csv", row.names=FALSE)
