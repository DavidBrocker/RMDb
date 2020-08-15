library(spotifyr)
library(dplyr)
library(collapsibleTree)
# Set Environment
Sys.setenv(SPOTIFY_CLIENT_ID = '1f6c9bc7d5314065bb3467e9ff984084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b1b24f99d46541769013cf20539b191e')

access_token <- get_spotify_access_token()

spotifyr::get_related_artists("The Beatles")

btl <- get_artist_audio_features("The Beatles")

get_related_artists(btl$artist_id[1])


get_similar_artists <- function(artist){
  art_st <- get_artist_audio_features(artist)
  related_artists <- get_related_artists(art_st$artist_id[1])
  related_artists <- related_artists %>% 
    select(name,id)
  related_artists
}

beatle_sim<- get_similar_artists("The beatles")

beatle_sim_sim <- map_df(beatle_sim$id,get_related_artists)

beatle_df <- data.frame(
  parent=rep(beatle_sim$name,each=20),
  child=beatle_sim_sim$name
)

collapsibleTree(beatle_df,
                hierarchy = c("parent","child"),
                root="The Beatles",
                fill=c(
                  "black",
                  rep("grey",length(unique(beatle_df$parent))),
                  rep("maroon",length(unique(paste(beatle_df$parent,beatle_df$child))))
                  )
                )

krs <- get_similar_artists("Krosis")

dgd <- get_similar_artists("Dance Gavin Dance")

dgd_rec <- map_df(dgd$id,get_related_artists)

krs_rec <- map_df(krs$id,get_related_artists)

krs_recz <- map(krs$id,get_related_artists)

unlist(krs_recz)

map_df(krs)

