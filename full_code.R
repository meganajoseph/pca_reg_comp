library(xml2)
library(dplyr)
library(tidyr)
library(ggpubr)

df <- read.delim('JPR robbery.tab', header = T, sep="\t")
ddi <- read_xml("JPR robbery-ddi.xml")   # use your file's name
xml_ns_strip(ddi)                  # removes the namespace so the paths below work

vars <- xml_find_all(ddi, ".//var")

data_dict <- data.frame(
  name  = xml_attr(vars, "name"),
  label = xml_text(xml_find_first(vars, "./labl")),
  type  = xml_attr(xml_find_first(vars, "./varFormat"), "type")
)

df <- df %>% drop_na()

y <- df$lnrobberyrate
X <- as.matrix(df %>% select(!c(lnrobberyrate, llnrobberyrate, lngdpsq, country, name, period, year, flldummyrobbery)))

model <- lm(y ~ X)
summary(model)

pca <- prcomp(X, scale=T)
pca$rotation
var_explained <- pca$sdev**2 / sum(pca$sdev**2)
ggplot(data = data.frame(x = c(1:10), y = var_explained), 
       mapping = aes(x = x, y = y)) + geom_line() + 
  xlab("PCs") + ylab("Variance Explained") + ggtitle("Scree Plot") +
  ylim(0, 1) +
  theme_pubr()

pca_loadings <- pca$rotation[, 1:6]
X_pca <- X %*% pca_loadings
model2 <- lm(y ~ X_pca)
summary(model2)
