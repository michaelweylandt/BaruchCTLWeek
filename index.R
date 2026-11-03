## ----------------------------------------------------------------------------------------
#| include: false
#| warning: false
#| message: false
library(tidyverse)
library(gt)
library(httr2)


## library(tidyverse)
## 
## NAME_BASICS <- read_csv("")

## 1 + 2 + 3

## ----------------------------------------------------------------------------------------
#| label: 'imdb_name_basics'
#| message: false 
#| warning: false
#| cache: true
library(tidyverse)
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
                    n_max=100000, 
                    na="\\N") |>
        mutate(across(ends_with("year"), as.numeric))
}

NAME_BASICS      <- get_imdb_file("name.basics")


## ----------------------------------------------------------------------------------------
#| label: 'imdb_title_basics'
#| message: false 
#| warning: false
#| cache: true
TITLE_BASICS     <- get_imdb_file("title.basics")


## ----------------------------------------------------------------------------------------
#| label: 'imdb_title_ratings'
#| message: false 
#| warning: false
#| cache: true
TITLE_RATINGS    <- get_imdb_file("title.ratings")


## ----------------------------------------------------------------------------------------
#| label: 'imdb_title_crew'
#| message: false 
#| warning: false
#| cache: true
TITLE_CREW       <- get_imdb_file("title.crew")


## ----------------------------------------------------------------------------------------
#| label: 'imdb_title_principals'
#| message: false 
#| warning: false
#| cache: true
TITLE_PRINCIPALS <- get_imdb_file("title.principals")


## ----------------------------------------------------------------------------------------
#| label: 'name_basics_filter'
#| cache: true
NAME_BASICS <- NAME_BASICS |> 
    filter(str_count(knownForTitles, ",") > 1)


## ----------------------------------------------------------------------------------------
#| label: 'title_ratings_tail'
TITLE_RATINGS |>
    ggplot(aes(x=numVotes)) + 
    geom_histogram(bins=30) +
    xlab("Number of IMDB Ratings") + 
    ylab("Number of Titles") + 
    ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") + 
    theme_bw() + 
    scale_x_log10(label=scales::comma) + 
    scale_y_continuous(label=scales::comma)


## ----------------------------------------------------------------------------------------
#| label: 'title_ratings_quantile'
TITLE_RATINGS |>
    pull(numVotes) |>
    quantile()


## ----------------------------------------------------------------------------------------
#| label: 'title_ratings_filter'
#| cache: true
TITLE_RATINGS <- TITLE_RATINGS |>
    filter(numVotes >= 500)


## ----------------------------------------------------------------------------------------
#| cache: true
#| label: 'title_other_filter'
#| message: false
TITLE_BASICS <- TITLE_BASICS |>
    semi_join(TITLE_RATINGS, 
              join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
    semi_join(TITLE_RATINGS, 
              join_by(tconst == tconst))

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
    semi_join(TITLE_RATINGS, join_by(tconst == tconst))


## ----------------------------------------------------------------------------------------
#| label: title_filter_final
#| cache: false
TITLE_BASICS <- TITLE_BASICS |> 
    filter_out(isAdult==1) |> 
    filter(titleType=="movie")|>
    select(-isAdult, -titleType) 


## ----------------------------------------------------------------------------------------
#| echo: false
#| message: false
library(gt)
TITLE_CREW |> 
    filter(str_detect(directors, ",") | str_detect(writers, ",")) |>
    filter_out(is.na(writers)) |>
    slice_head(n=10) |>
    gt(id="tbl_crew_comma") |>
    cols_label(tconst="Title ID", 
               directors="Director(s)",
               writers="Writer(s)") |>
    tab_header("Selected Titles with Multiple Directors or Writers")


## ----------------------------------------------------------------------------------------
#| echo: false
#| message: false
library(gt)
TITLE_CREW |> 
    filter(str_detect(directors, ",") | str_detect(writers, ",")) |>
    filter_out(is.na(writers)) |>
    slice_head(n=10) |>
    separate_longer_delim(directors, ",") |>
    separate_longer_delim(writers, ",") |>
    gt(id="tbl_crew_comma") |>
    cols_label(tconst="Title ID", 
               directors="Director(s)",
               writers="Writer(s)") |>
    tab_header("Selected Titles with Multiple Directors or Writers")


## ----------------------------------------------------------------------------------------
#| cache: true
#| label: split_delim
TITLE_CREW <- TITLE_CREW |> 
    separate_longer_delim(directors, ",") |>
    separate_longer_delim(writers, ",")

NAME_BASICS <- NAME_BASICS |>
    separate_longer_delim(knownForTitles, ",") 

TITLE_BASICS <- TITLE_BASICS |> 
    separate_longer_delim(genres, ",") |>
    rename(genre=genres)


## ----------------------------------------------------------------------------------------
#| echo: false
NAME_BASICS |> 
    slice_sample(n=20) |>
    gt(id="tbl_name_basics") |>
    tab_header("NAME_BASICS")


## ----------------------------------------------------------------------------------------
#| echo: false
TITLE_BASICS |> 
    slice_sample(n=20) |>
    gt(id="tbl_title_basics") |>
    tab_header("TITLE_BASICS")


## ----------------------------------------------------------------------------------------
#| echo: false
TITLE_CREW |> 
    slice_sample(n=20) |>
    gt(id="tbl_title_crew") |>
    tab_header("TITLE_CREW")


## ----------------------------------------------------------------------------------------
#| echo: false
TITLE_RATINGS |> 
    slice_sample(n=20) |>
    gt(id="tbl_title_ratings") |>
    tab_header("TITLE_RATINGS")


## ----------------------------------------------------------------------------------------
#| echo: false
TITLE_PRINCIPALS |> 
    slice_sample(n=20) |>
    gt(id="tbl_title_principals") |>
    tab_header("TITLE_PRINCIPALS")


## ----------------------------------------------------------------------------------------
#| eval: false
# JOIN_TYPE(TABLE_1_NAME,
#           TABLE_2_NAME,
#           join_by(TABLE_1_KEY_COLUMN == TABLE_2_KEY_COLUMN))


## ----------------------------------------------------------------------------------------
library(dplyr)
inner_join(band_members, 
           band_instruments, 
           join_by(name == name))


## ----------------------------------------------------------------------------------------
library(dplyr)
inner_join(band_members, 
           band_instruments, 
           join_by(name == name)) |>
    filter(plays == "guitar") 


## NAME_BASICS |>
##     filter(primaryName == "Harrison Ford",
##            birthYear == 1942 # Make sure to ignore the silent-film era Ford
##            ) |>
##     inner_join(TITLE_BASICS,
##                join_by(knownForTitles == tconst)) |>
##     select(primaryName, knownForTitles, primaryTitle)
