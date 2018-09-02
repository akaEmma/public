get_factors <- function(df) {
    is.fact <- sapply(df, is.factor)
    names(df[is.fact])
}

get_characters <- function(df) {
    is.char <- sapply(df, is.character)
    names(df[is.char])
}

get_numbers <- function(df) {
    is.num <- sapply(df, is.numeric)
    names(df[is.num])
}

drop <- function(df, var_name) {
    df %>% select(df, -var_name)
}

