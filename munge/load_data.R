fnames_study1 <- list.files("data/study1", full.names = T)
fnames_study2 <- list.files("data/study2", full.names = T)
fnames_study3 <- list.files("data/study3", full.names = T)

map_df(fnames_study1, \(x){
  read_csv(x)[c(-1,-2),] |>
    mutate(
      .before = 1,
      date = str_extract(x, "(?<=data_)[0-9_]+")
    )
}) -> data_study1

map_df(fnames_study2, \(x){
  read_csv(x)[c(-1,-2),] |>
    mutate(
      .before = 1,
      date = str_extract(x, "(?<=data_)[0-9_]+")
    )
}) -> data_study2

map_df(fnames_study3, \(x){
  read_csv(x)[c(-1,-2),] |>
    mutate(
      .before = 1,
      date = str_extract(x, "(?<=data_)[0-9_]+")
    )
}) -> data_study3
