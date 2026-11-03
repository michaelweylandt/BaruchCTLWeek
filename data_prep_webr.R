library(tidyverse)
library(gt)
library(httr2)

get_imdb_file <- function(fname){
    BASE_URL <- "https://datasets.imdbws.com/"
    fname_ext <- paste0(fname, ".tsv.gz")
    fs::dir_create("data")
    fname <- fs::path("data", fname_ext)
    if(!fs::file_exists(fname)){
        FILE_URL <- paste0(BASE_URL, fname_ext)
        download.file(FILE_URL,
                      destfile = fname)
    }
    readr::read_tsv(fname,
                    lazy=FALSE,
                    show_col_types=FALSE,
                    n_max=5000000,
                    na="\\N") |>
        mutate(across(ends_with("year"), as.numeric))
}

NAME_BASICS      <- get_imdb_file("name.basics")
TITLE_BASICS     <- get_imdb_file("title.basics") |>
    filter_out(is.na(genres))

TITLE_RATINGS    <- get_imdb_file("title.ratings")
TITLE_CREW       <- get_imdb_file("title.crew")
TITLE_PRINCIPALS <- get_imdb_file("title.principals") |>
    filter(category %in% c("actor", "actress", "self")) |>
    select(-ordering, -job)


NAME_BASICS <- NAME_BASICS |>
    filter(str_count(knownForTitles, ",") > 1)

TITLE_RATINGS <- TITLE_RATINGS |>
    filter(numVotes >= 500)

TITLE_BASICS <- TITLE_BASICS |>
    semi_join(TITLE_RATINGS,
              join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
    semi_join(TITLE_RATINGS,
              join_by(tconst == tconst))

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
    semi_join(TITLE_RATINGS, join_by(tconst == tconst))

TITLE_BASICS <- TITLE_BASICS |>
    filter_out(isAdult==1) |>
    filter(titleType=="movie")|>
    select(-isAdult, -titleType)

TITLE_CREW <- TITLE_CREW |>
    separate_longer_delim(directors, ",") |>
    separate_longer_delim(writers, ",")

NAME_BASICS_LONGER <- NAME_BASICS |>
    separate_longer_delim(knownForTitles, ",")

TITLE_BASICS_LONGER <- TITLE_BASICS |>
    separate_longer_delim(genres, ",") |>
    rename(genre=genres)

fs::dir_create("docs/data/processed")
write_csv(NAME_BASICS, "docs/data/processed/name_basics.csv.gz")
write_csv(TITLE_BASICS, "docs/data/processed/title_basics.csv.gz")
write_csv(NAME_BASICS_LONGER, "docs/data/processed/name_basics_longer.csv.gz")
write_csv(TITLE_BASICS_LONGER, "docs/data/processed/title_basics_longer.csv.gz")
write_csv(TITLE_CREW, "docs/data/processed/title_crew.csv.gz")
write_csv(TITLE_PRINCIPALS, "docs/data/processed/title_principals.csv.gz")
write_csv(TITLE_RATINGS, "docs/data/processed/title_ratings.csv.gz")
