# These are simple project-specific functions

#get factor column names
get_factor_names <- function(df) {
    is.fact <- sapply(df, is.factor)
    names(df[is.fact])
}

#get factor column indices
get_factor_i <- function(df, factor_names) {
    which(names(df) %in% factor_names)
    } 

#get character column names
get_character_names <- function(df) {
    is.char <- sapply(df, is.character)
    names(df[is.char])
}

#get numerical column names
get_number_names <- function(df) {
    is.num <- sapply(df, is.numeric)
    names(df[is.num])
}

get_inc_names <- function(df) {
    c("incporar", "incporar_i")
}

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
    )
}

# get rid of all the vars in a vector
delete_vars <- function(df, delete_these) {
    names.use <- names(df)[!(names(df) %in% delete_these)]
    df[, names.use]
}

#keep only the vars in a vector
get_these_vars <- function(df, keep_these) {
    df[, keep_these]
}

save_my_data <- function(df) {
    write_rds(df, "workfile")
}

#Convert text into 'sentence' case (first word capitalised, others lower case)
# I got this from http://rcourse.rupertoverall.net/Extras.R. Thanks, Rupert!
tosentence <- function(s, split=NA) {
	s <- tolower(s)
		s <- as.character(sapply(s, function(ss){
			ss <- strsplit(ss, split)[[1]]
			substring(ss, 1, 1) <- toupper(substring(ss, 1, 1))
			if(!is.na(split)) ss <- paste(ss, collapse = split)
			return(ss)
		}))
	return(s)
}


# strip out any empty rows from a df

strip_empty_rows <- function(df) {
  df %>% filter(rowSums(is.na(.)) != ncol(.))
}

#print a df's table of contents

#---CONTENTS---#
toc <- function(df) {
  df.CONTENTS <- contents(df)$contents
  
  print(df.CONTENTS)
  
  #IN CASE THAT THE R CONSOLE CANNOT DISPLAY WHOLE CONTENTS,
  #YOU CAN SAVE CONTENTS INTO A FILE
  write.table(df.CONTENTS, file = paste("./data/","toc",sep = ""))
}

# Get demographic variable names, specific to the vaccine project

get_dem_names <- function(df) {
    s <- c("agegrp",
      "bf_endr06", 
      "bf_exclr06",
      "bf_endr06",
      "cbf_01",
      "num_hh",
      "respondent",
      "cen_reg",
      "childnm",
      "wic_ever",
      "mom_ed",
      "frstbrn",
      "i_hisp_k",
      "poverty",
      "incq298a",
      "language",
      "agecpoxr",
      "had_cpox",
      "mom_var",
      "born_diff",
      "num_cells_hh",
      "num_cells_parents",
      "child_race",
      "rent_own",
      "sex",
      "state",
      "n_prvr",
      "prov_fac",
      "vfc_order",
      "hep_brth",
      "ins_stat_i",
      "ins_stat",
      "mom_age")
    return(s)
}

get_vaccine_names <- function(df) {
    c("dtp", "flu", "hep", "hib", "mmr", "pcv", "polio", "rot", "vrc")
}

get_wt_names <- function(df) {
    indf(df, (".wt."))
}

#returns a vector of all matching variable names
indf <- function(df, s) {
    x <- grep(s, names(df))
    paste(names(df[x]))
}

merge_vacs <- function(df) {
    dtp <- indf(df, "^ddtp") #get all dtp variables
    flu <- indf(df, "^dflu") #get all flu variables, etc...
    hep <- indf(df, "^dhep")
    hib <- indf(df, "^dhib")
    mmr <- indf(df, "^dmmr")
    pcv <- indf(df, "^dpcv")
    polio <- indf(df, "^dpolio") 
    rot <- indf(df, "^drot")
    vrc <- indf(df, "^dvrc")
    df$dtp <- count_vac(df, dtp) #count how many dtp shots given
    df <- df %>% select(-one_of(dtp)) #store the count in a new var
    df$flu <- count_vac(df, flu) #count how many flu shots given
    df <- df %>% select(-one_of(flu)) #store the count in a new var....
    df$hep <- count_vac(df, hep)
    df <- df %>% select(-one_of(hep))
    df$hib <- count_vac(df, hib)
    df <- df %>% select(-one_of(hib))
    df$mmr <- count_vac(df, mmr)
    df <- df %>% select(-one_of(mmr))
    df$pcv <- count_vac(df, pcv)
    df <- df %>% select(-one_of(pcv))
    df$polio <- count_vac(df, polio)
    df <- df %>% select(-one_of(polio))
    df$rot <- count_vac(df, rot)
    df <- df %>% select(-one_of(rot))
    df$vrc <- count_vac(df, vrc)
    df <- df %>% select(-one_of(vrc))
    var_label(df$dtp) <- "Number of DTP Vaccinations" #label new var...
    var_label(df$flu) <- "Number of Flu Vaccinations"
    var_label(df$hep) <- "Number of HepB Vaccinations"
    var_label(df$hib) <- "Number of HIB Vaccinations"
    var_label(df$mmr) <- "Number of MMR Vaccinations"
    var_label(df$pcv) <- "Number of PCV Vaccinations"
    var_label(df$polio) <- "Number of Polio Vaccinations"
    var_label(df$rot) <- "Number of ROT Vaccinations"
    var_label(df$vrc) <- "Number of Varicella Vaccinations"
    return(df) #send back the tibble
}

#Calculate total number of all vaccinations
# merge_vacs_more <- function(df) {
#     df$vaccinations <- df[,["dtp"]] + df[[, "flu"]]) 
#     var_label(df$vaccinations) <- "Total Vaccinations per Child"
#     return(df)
# }



# 1 or 0 for any or none in a group having NA as a value

count_vac <- function(df, vaccine_type) {
    entry_count <- apply(df[,vaccine_type], 1, function(x) sum(!is.na(x)))
    return(entry_count)
} 


create_dummies <- function(df, dummies) {
    newdf <- df %>% dummy_cols(select_columns = c(dummies),
                               remove_first_dummy = TRUE)
    #eliminate the old var in favor of the dummies
    newdf %>% select(-one_of(c(dummies)))
}




















# 
# Beautiful shadow plot for someone else's data set.
#
#upset(movies, 
#       sets=c("Action", "Adventure", "Comedy", "Drama", "Mystery", 
#              "Thriller", "Romance", "War", "Western"), 
#       keep.order=T, 
#       order.by = "freq",
#       queries = list(
#           list(query = function(row, value){ data <- (row["Drama"] == value)}, 
#                color="blue",
#                params = list(1),
#                active = T 
#           )
#           ,  
#           list(query = function(row, value){ data <- (row["Action"] == value)}, 
#                color="orange",
#                params = list(1),
#                active = T 
#           )
#           ,
#           list(
#               query = function(row, date){data <- (row["ReleaseDate"] %in% date)},
#               params = list(1995),
#               color = "red",
#               active = T
#           )
#       )       
# )
