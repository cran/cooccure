## ----options, include = FALSE-----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5, fig.align = "center", dpi = 96)
old_options <- options(digits = 3, width = 90)
has_igraph <- requireNamespace("igraph", quietly = TRUE)

## ----package----------------------------------------------------------------------------
library(cooccure)

## ----data-------------------------------------------------------------------------------
head(movies)

## ----counts-----------------------------------------------------------------------------
net <- cooccurrence(movies, field = "genres", sep = ",")
print(net, n = 5)

## ----jaccard----------------------------------------------------------------------------
jac <- cooccurrence(movies, field = "genres", sep = ",",
                    similarity = "jaccard", min_occur = 20)
print(jac, n = 8)

## ----summary----------------------------------------------------------------------------
summary(jac)

## ----nodes------------------------------------------------------------------------------
head(as.data.frame(jac, what = "nodes"))

## ----heatmap, fig.height = 5------------------------------------------------------------
plot(jac)

## ----degree, fig.height = 3.5-----------------------------------------------------------
plot(jac, type = "degree")

## ----decades----------------------------------------------------------------------------
by_decade <- cooccurrence(movies, field = "genres", sep = ",",
                          split_by = "decade", similarity = "jaccard",
                          min_occur = 5)
as.data.frame(by_decade, what = "groups")

## ----igraph, eval = has_igraph----------------------------------------------------------
g <- as_igraph(jac)
g

## ----restore, include = FALSE-------------------------------------------------
options(old_options)

