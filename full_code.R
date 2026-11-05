library(dplyr)
library(tidyr)
library(ggpubr)
library(tidyverse)
library(readxl)

df <- read_excel("data/clh_2023_tract_2_0.xlsx", 
                                 sheet = "Data")

variables <- c(
  "ACS_AVG_HH_SIZE", "ACS_PCT_DISABLE", "ACS_PCT_MALE", "ACS_PCT_NON_CITIZEN",
  "ACS_PCT_ENGLISH", "ACS_PCT_HH_LIMIT_ENGLISH", "ACS_PCT_OTH_LANG",
  "ACS_PCT_VET", "ACS_MEDIAN_AGE", "ACS_PCT_AGE_ABOVE65",
  "ACS_PCT_AIAN", "ACS_PCT_ASIAN", "ACS_PCT_BLACK", "ACS_PCT_HISPANIC",
  "ACS_PCT_MULT_RACE", "ACS_PCT_NHPI", "ACS_PCT_OTHER_RACE", "ACS_PCT_WHITE",
  "ACS_PCT_CHILD_1FAM",
  "ACS_PCT_AIAN_COMB", "ACS_PCT_ASIAN_COMB", "ACS_PCT_BLACK_COMB",
  "ACS_PCT_NHPI_COMB", "ACS_PCT_WHITE_COMB",
  "ACS_PCT_HH_NO_COMP_DEV", "ACS_PCT_HH_SMARTPHONE", "ACS_PCT_HH_INTERNET",
  "ACS_PCT_HH_CELLULAR", "ACS_PCT_BACHELOR_DGR", "ACS_PCT_1UP_PERS_1ROOM",
  "ACS_PCT_HU_SOLAR", "ACS_MEDIAN_YEAR_BUILT", "ACS_PCT_HU_NO_VEH",
  "ACS_PCT_UNINSURED", "CRE_TOT_RISK0",
  "ACS_PCT_DIVORCED_F", "ACS_PCT_DIVORCED_M",
  "ACS_PCT_MARRIED_SP_AB_M", "ACS_PCT_MARRIED_SP_AB_F",
  "ACS_PCT_MARRIED_SP_PR_F", "ACS_PCT_MARRIED_SP_PR_M",
  "ACS_PCT_NVR_MARRIED_F", "ACS_PCT_NVR_MARRIED_M",
  "ACS_PCT_WIDOWED_F", "ACS_PCT_WIDOWED_M", "ACS_MEDIAN_HH_INC"
)

df_selected <- df %>%
  select(variables) %>%
  drop_na() %>%
  mutate(across(everything(), as.numeric))

model <- lm(ACS_MEDIAN_HH_INC ~ ., data=df_selected)
summary(model)

X <- as.matrix(df_selected %>% 
  select(!c('ACS_MEDIAN_HH_INC')))

pca <- prcomp(X, scale=T)
pca$rotation
var_explained <- pca$sdev**2 / sum(pca$sdev**2)
ggplot(data = data.frame(x = c(1:ncol(X)), y = var_explained), 
       mapping = aes(x = x, y = y)) + geom_line() + 
  xlab("PCs") + ylab("Variance Explained") + ggtitle("Scree Plot") +
  ylim(0, 1) +
  theme_pubr()

pca_loadings <- pca$rotation[, 1:26]
X_pca <- X %*% pca_loadings
y <- as.matrix(df_selected %>% 
                 select(c('ACS_MEDIAN_HH_INC')))
model2 <- lm(y ~ X_pca)
summary(model2)
