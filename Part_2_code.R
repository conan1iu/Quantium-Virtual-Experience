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



## ----echo = F----------------------------------------------------------------------------
combi <- merge(transactions, behaviour, by="LYLTY_CARD_NBR")

combi <- combi[-c(which(combi$PROD_QTY == 200)),]

rownames(combi) <- NULL

combi$BRAND_NAME <- word(combi$PROD_NAME, 1)

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

combi$WEIGHT <- parse_number(combi$PROD_NAME)

combi$WEIGHT <- as.numeric(combi$WEIGHT)

range(combi$WEIGHT)

combi$PROD_PRICE <- combi$TOT_SALES/combi$PROD_QTY


## ----------------------------------------------------------------------------------------
range(combi$DATE)


## ----------------------------------------------------------------------------------------
dates <- unique(combi$DATE)
dates <- as.numeric(dates)
dates[order(dates)]

