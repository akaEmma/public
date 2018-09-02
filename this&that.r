# These are simple project-specific functions

#get factor column names
get_factors <- function(df) {
    is.fact <- sapply(df, is.factor)
    names(df[is.fact])
}

#get character column names
get_characters <- function(df) {
    is.char <- sapply(df, is.character)
    names(df[is.char])
}

#get numerical column names
get_numbers <- function(df) {
    is.num <- sapply(df, is.numeric)
    names(df[is.num])
}

# get rid of all the vars in a vector

delete_vars <- function(df, delete_these) {
    names.use <- names(df)[!(names(df) %in% delete_these)]
    df[, names.use]
}

save_my_data <- function(df) {
    write_rds(df, "./data/less_data.rds")
}

#Convert text into 'sentence' case (first word capitalised, others lower case)
# I got this from http://rcourse.rupertoverall.net/Extras.R. Thanks, Rupert!
tosentence <- function(s, split=NA) {
	s <- tolower(s)
		s <- as.character(sapply(s, function(ss){
			ss <- strsplit(ss, split)[[1]]
			substring(ss, 1, 1) <- toupper(substring(ss, 1, 1))
			if(!is.na(split)) ss <- paste(ss, collapse=split)
			return(ss)
		}))
	return(s)
}
