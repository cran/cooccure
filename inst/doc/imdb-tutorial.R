## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", fig.width = 8, fig.height = 6,
  message = FALSE, warning = FALSE
)

## ----load-data----------------------------------------------------------------
library(cooccure)

## -----------------------------------------------------------------------------
head(movies)

## ----genre-basic--------------------------------------------------------------
cooccurrence(movies, field = "genres", sep = ",")

## ----genre-jaccard------------------------------------------------------------
library(cograph)

Net <- co(movies, field = "genres", sep = ",", similarity = "jaccard")

Gr<- as_cograph(Net, directed = TRUE)

degree_distribution(Gr)

## ----sim-none-----------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "none", top_n = 3)

## ----sim-jaccard--------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "jaccard", top_n = 3)

## ----sim-cosine---------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "cosine", top_n = 3)

## ----sim-inclusion------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "inclusion", top_n = 3)

## ----sim-association----------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "association", top_n = 3)

## ----sim-dice-----------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "dice", top_n = 3)

## ----sim-equivalence----------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "equivalence", top_n = 3)

## ----counting-full------------------------------------------------------------
co(movies, field = "genres", sep = ",", top_n = 5)

## ----counting-fractional------------------------------------------------------
co(movies, field = "genres", sep = ",", counting = "fractional", top_n = 5)

## ----scale-log----------------------------------------------------------------
co(movies, field = "genres", sep = ",", scale = "log", top_n = 5)

## ----scale-minmax-------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "jaccard", scale = "minmax", top_n = 5)

## ----scale-binary-------------------------------------------------------------
co(movies, field = "genres", sep = ",", scale = "binary", top_n = 5)

## ----scale-sqrt---------------------------------------------------------------
co(movies, field = "genres", sep = ",", scale = "sqrt", top_n = 5)

## ----scale-combined-----------------------------------------------------------
co(movies, field = "genres", sep = ",",
   similarity = "association", scale = "log", min_occur = 20, top_n = 5)

## ----min-occur----------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "jaccard", min_occur = 20)

## ----threshold----------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "jaccard", threshold = 0.15)

## ----top-n--------------------------------------------------------------------
co(movies, field = "genres", sep = ",", similarity = "jaccard", top_n = 10)

## ----combined-filter----------------------------------------------------------
co(movies, field = "genres", sep = ",",
   similarity = "association", counting = "fractional",
   min_occur = 15, threshold = 0.001, top_n = 20)

## ----actor-net----------------------------------------------------------------
co(actors, field = "actor", by = "tconst",
   similarity = "jaccard", min_occur = 3, threshold = 0.1)

## ----actor-frac---------------------------------------------------------------
co(actors, field = "actor", by = "tconst",
   similarity = "jaccard", counting = "fractional",
   min_occur = 3, threshold = 0.05)

## ----split-decade-------------------------------------------------------------
co(movies, field = "genres", sep = ",",
   split_by = "decade", similarity = "jaccard",
   min_occur = 5, top_n = 5)

## ----filter-decade------------------------------------------------------------
decades <- co(movies, field = "genres", sep = ",",
              split_by = "decade", similarity = "jaccard",
              min_occur = 5, top_n = 5)

subset(decades, group == "2010s")

## ----decade-groups------------------------------------------------------------
decade_summary <- summary(decades)

as.data.frame(decade_summary, what = "groups")

## ----split-rating-------------------------------------------------------------
movies$rating_band <- ifelse(movies$averageRating >= 8, "8+", "7-7.9")
co(movies, field = "genres", sep = ",",
   split_by = "rating_band", similarity = "jaccard",
   min_occur = 10, top_n = 5)

## ----out-default--------------------------------------------------------------
co(movies, field = "genres", sep = ",", top_n = 5)

## ----out-gephi----------------------------------------------------------------
co(movies, field = "genres", sep = ",",
   similarity = "jaccard", output = "gephi", top_n = 10)

## ----gephi-export, eval=FALSE-------------------------------------------------
# write.csv(
#   co(movies, field = "genres", sep = ",", similarity = "jaccard", output = "gephi"),
#   "genre_network.csv", row.names = FALSE
# )

## ----out-cograph, fig.width=8, fig.height=8-----------------------------------
library(cograph)
net <- co(movies, field = "genres", sep = ",",
          similarity = "jaccard", min_occur = 20, output = "cograph")
splot(net, layout = "fr", scale_nodes_by = "degree")

## ----cograph-styled, fig.width=8, fig.height=8--------------------------------
library(cograph)
splot(net, layout = "gephi", label_size = .8, label_fontface = "bold",
           node_fill = "#F9C22E",  
           node_border_width = 0.0001, edge_color = "black",
           scale_nodes_by = "degree", edge_width_range = c(0.1:4))

## ----out-igraph---------------------------------------------------------------
g <- co(movies, field = "genres", sep = ",",
        similarity = "jaccard", min_occur = 20, output = "igraph")
g

## ----igraph-metrics-----------------------------------------------------------
igraph::degree(g)
igraph::betweenness(g)

## ----out-matrix---------------------------------------------------------------
mat <- co(movies, field = "genres", sep = ",",
          similarity = "jaccard", min_occur = 20, output = "matrix")
round(mat[1:6, 1:6], 3)

## ----conv-matrix--------------------------------------------------------------
result <- co(movies, field = "genres", sep = ",",
             similarity = "jaccard", min_occur = 20)
as_matrix(result)

## ----conv-raw-----------------------------------------------------------------
as_matrix(result, type = "raw")

## ----conv-igraph--------------------------------------------------------------
as_igraph(result)

## ----fmt-delimited------------------------------------------------------------
res1 <- co(movies, field = "genres", sep = ",")

## ----fmt-long-----------------------------------------------------------------
genre_long <- do.call(rbind, lapply(seq_len(nrow(movies)), function(i) {
  gs <- trimws(strsplit(movies$genres[i], ",")[[1]])
  data.frame(movie_id = movies$tconst[i], genre = gs, stringsAsFactors = FALSE)
}))
res2 <- co(genre_long, field = "genre", by = "movie_id")

## ----fmt-binary---------------------------------------------------------------
all_genres <- sort(unique(genre_long$genre))
bin <- matrix(0L, nrow = nrow(movies), ncol = length(all_genres),
              dimnames = list(movies$tconst, all_genres))
for (i in seq_len(nrow(genre_long))) {
  row <- match(genre_long$movie_id[i], movies$tconst)
  bin[row, genre_long$genre[i]] <- 1L
}
res3 <- co(bin)

## ----fmt-list-----------------------------------------------------------------
res4 <- co(lapply(strsplit(movies$genres, ","), trimws))

## ----fmt-verify---------------------------------------------------------------
all.equal(res1$weight, res2$weight)
all.equal(res1$weight, res3$weight)
all.equal(res1$weight, res4$weight)

## ----pipeline, fig.width=8, fig.height=8--------------------------------------
co(movies, field = "genres", sep = ",",
   similarity = "jaccard", counting = "fractional",
   scale = "minmax", min_occur = 15, threshold = 0.05,
   output = "cograph") |>
  splot(layout = "gephi", edge_width = 3, label_size = 0.9,
                 title = "IMDB Genre Co-occurrence (Jaccard, fractional, min 15 movies)")

