## ----echo=F, eval=T, message = F---------------------------------------------------------

library(rpart)

library(ggthemr)
ggthemr('fresh')

library(dplyr)

library(tidyverse)

library(data.table)

library(readr)


library(modelr)

library(formattable)

library(ggplot2)

library(tidyr)

library(egg)

options(max.print = 5)

knitr::opts_chunk$set(comment = NA, echo = TRUE, eval = TRUE, tidy.opts=list(width.cutoff=20),tidy=TRUE, cache = F, message = FALSE, warning = FALSE)


## ----echo = F, eval = T------------------------------------------------------------------

library(readr)
library(stringr)

setwd("~/Desktop/Forage/Quantium")

transactions <- read_csv("QVItransactiondata.csv")

behaviour <- read_csv("QVI_purchase_behaviour.csv")



## ----eval = T, echo = T------------------------------------------------------------------
head(transactions)
names(transactions)

head(behaviour)
names(behaviour)


## ----------------------------------------------------------------------------------------
dim(transactions)
dim(behaviour)


## ----------------------------------------------------------------------------------------
length(unique(transactions$LYLTY_CARD_NBR)) == 
length(unique(behaviour$LYLTY_CARD_NBR))


## ----------------------------------------------------------------------------------------
combi <- merge(transactions, behaviour, by="LYLTY_CARD_NBR")


## ----------------------------------------------------------------------------------------
colSums(is.na(combi))


## ----------------------------------------------------------------------------------------
range(combi$PROD_QTY)


## ----------------------------------------------------------------------------------------
combi <- combi[-c(which(combi$PROD_QTY == 200)),]

range(combi$PROD_QTY)

dim(combi)

rownames(combi) <- NULL


## ----echo=T, eval=T----------------------------------------------------------------------
head(combi$PROD_NAME, 4)


## ----echo = T----------------------------------------------------------------------------
combi$BRAND_NAME <- word(combi$PROD_NAME, 1)


## ----------------------------------------------------------------------------------------
unique(combi$BRAND_NAME)


## ----------------------------------------------------------------------------------------
combi$BRAND_NAME <- ifelse(combi$BRAND_NAME == "Natural", "Natural Chip Co",
                    ifelse(combi$BRAND_NAME == "Red", "Red Rock Deli",
                    ifelse(combi$BRAND_NAME == "Grain", "Grain Waves",
                    ifelse(combi$BRAND_NAME == "RRD", "Red Rock Deli",
                    ifelse(combi$BRAND_NAME == "Old", "Old El Paso",
                    ifelse(combi$BRAND_NAME == "GrnWves", "Grain Waves",
                    ifelse(combi$BRAND_NAME == "Snbts", "Sunbites",
                    ifelse(combi$BRAND_NAME == "Infzns", "Infuzions",
                    ifelse(combi$BRAND_NAME == "Dorito", "Doritos",
                    ifelse(combi$BRAND_NAME == "NCC", "Natural Chip Co",
                    combi$BRAND_NAME))))))))))


## ----------------------------------------------------------------------------------------
unique(combi$BRAND_NAME)


## ----------------------------------------------------------------------------------------
combi$WEIGHT <- parse_number(combi$PROD_NAME)


## ----------------------------------------------------------------------------------------
combi$WEIGHT


## ----------------------------------------------------------------------------------------
combi$WEIGHT <- as.numeric(combi$WEIGHT)

range(combi$WEIGHT)


## ----------------------------------------------------------------------------------------
combi$PROD_PRICE <- combi$TOT_SALES/combi$PROD_QTY


## ----------------------------------------------------------------------------------------
ggplot(combi, aes(x = WEIGHT, y = TOT_SALES)) +
  geom_bin2d(bins = 35) +
  scale_fill_continuous(low = "navy", high = "pink", name = "Occurrences") +
  xlab("Weight (g)") +
  ylab("Total sales (dollars)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position ="right",
        axis.text.x = element_text(angle = 0, vjust = 1, size = 8, hjust = 1))


## ----echo = T, eval = T, output = F------------------------------------------------------
LIFEREV <- combi %>% group_by(LIFESTAGE) %>% summarise(REVENUE = sum(TOT_SALES))
LIFEREV <- LIFEREV %>% arrange(REVENUE)
LIFEREV$LIFESTAGE <- factor(LIFEREV$LIFESTAGE, levels = LIFEREV$LIFESTAGE)

g1 <- ggplot(LIFEREV, aes(x = LIFESTAGE, y = REVENUE)) +
  geom_bar(stat = "identity") + 
  ggtitle("Revenue distribution by customer life stage") +
  xlab("Life stage of customer") +
  scale_y_continuous(name = "Price (AUD)", labels = scales::comma) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))


## ----echo = T, eval = T, output = F------------------------------------------------------
LIFECOUNT <- combi %>% group_by(LIFESTAGE) %>% summarise(COUNT = n())
LIFECOUNT <- LIFECOUNT %>% arrange(COUNT)
LIFECOUNT$LIFESTAGE <- factor(LIFECOUNT$LIFESTAGE, levels = LIFECOUNT$LIFESTAGE)

g2 <- ggplot(LIFECOUNT, aes(x = LIFESTAGE, y = COUNT)) +
  geom_bar(stat = "identity") + 
  ggtitle("Distribution of customer life stage") +
  xlab("Life stage of customer") +
  scale_y_continuous(name = "Count", labels = scales::comma) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))


## ----------------------------------------------------------------------------------------
ggarrange(g1, g2, ncol = 2)


## ----echo = T, eval = T, output = F------------------------------------------------------
PREM <- combi %>% group_by(PREMIUM_CUSTOMER) %>% summarise(REVENUE = sum(TOT_SALES))
PREM <- PREM %>% arrange(REVENUE)
PREM$PREMIUM_CUSTOMER <- factor(PREM$PREMIUM_CUSTOMER, levels = PREM$PREMIUM_CUSTOMER)

g3 <- ggplot(PREM, aes(x = PREMIUM_CUSTOMER, y = REVENUE)) +
  geom_bar(stat = "identity") + 
  ggtitle("Revenue distribution by customer quality") +
  xlab("Life stage of customer") +
  scale_y_continuous(name = "Price (AUD)", labels = scales::comma) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))


## ----echo = T, eval = T, output = F------------------------------------------------------
PREMC <- combi %>% group_by(PREMIUM_CUSTOMER) %>% summarise(COUNT = n())
PREMC <- PREMC %>% arrange(COUNT)
PREMC$PREMIUM_CUSTOMER <- factor(PREMC$PREMIUM_CUSTOMER, levels = PREMC$PREMIUM_CUSTOMER)

g4 <- ggplot(PREMC, aes(x = PREMIUM_CUSTOMER, y = COUNT)) +
  geom_bar(stat = "identity") + 
  ggtitle("Distribution of customer life stage") +
  xlab("Life stage of customer") +
  scale_y_continuous(name = "Count", labels = scales::comma) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))


## ----------------------------------------------------------------------------------------
ggarrange(g3, g4, ncol = 2)


## ----------------------------------------------------------------------------------------
STt5 <- combi %>% group_by(STORE_NBR) %>% summarise(COUNT = n()) 
STt5 <- STt5 %>% arrange(-COUNT) %>% dplyr::slice(1:5)
STt5 <- as.data.frame(STt5)
library(systemfonts)
library(kableExtra)


## ----------------------------------------------------------------------------------------
kbl(STt5)


## ----------------------------------------------------------------------------------------
STb5 <- combi %>% group_by(STORE_NBR) %>% summarise(COUNT = n()) 
STb5 <- STb5 %>% arrange(COUNT) %>% dplyr::slice(1:5)
STb5 <- as.data.frame(STb5)
library(systemfonts)
library(kableExtra)


## ----------------------------------------------------------------------------------------
kbl(STb5)


## ----------------------------------------------------------------------------------------
combi[which(combi$STORE_NBR == 76),2]
combi[which(combi$STORE_NBR == 92),2]
combi[which(combi$STORE_NBR == 11),2]
combi[which(combi$STORE_NBR == 31),2]
combi[which(combi$STORE_NBR == 206),2]

