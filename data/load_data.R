# Study 1         =======
fnames_study1 <- list.files(paste0(relative_path,"data/study1"), full.names = T)

raw_data <- list()
raw_data[["s1"]] <- map_df(fnames_study1, \(x){
  read_csv(x)[c(-1,-2),] |> mutate(
    .before = 1,
    date = str_extract(x, "(?<=data_)[0-9_]+") )
  })


# Study 2         ======
fnames_study2 <- list.files(paste0(relative_path,"data/study2"), full.names = T)

raw_data[["s2"]] <- map_df(fnames_study2, \(x){
  read_csv(x)[c(-1,-2),] |> mutate(
    .before = 1,
    date = str_extract(x, "(?<=data_)[0-9_]+") )
  })


# Study 3         ======
fnames_study3 <- list.files(paste0(relative_path,"data/study3"), full.names = T)

raw_data[["s3"]] <- map_df(fnames_study3, \(x){
  read_csv(x)[c(-1,-2),] |> mutate(
    .before = 1,
    date = str_extract(x, "(?<=data_)[0-9_]+") )
  })