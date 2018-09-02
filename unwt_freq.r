#---UNWEIGHTED FREQUENCY---#
unwt_freq <- function(UNWT.VAR){#FUNCTION TO PRINT UNWEIGHTED FREQUENCIES
    
    unwt.tab <- wtd.table(UNWT.VAR, weights =  NULL, type = 'table')
    unwtd.freq <- data.frame(cbind(
        unwt.tab, round(unwt.tab/sum(unwt.tab)*100,2),
        cumsum(unwt.tab), cumsum(round(unwt.tab/sum(unwt.tab)*100,2))))
    names(unwtd.freq) <- c("Frequency", "Percent", "Cumulative Frequency", "Cumulative Percent")
    unwtd.title <- paste('2016 NIS', 'UNWEIGHTED FREQUENCIES', var_label(UNWT.VAR), sep = "\n")
    var_label(unwtd.freq) <- unwtd.title
    
    print(unwtd.freq)
}