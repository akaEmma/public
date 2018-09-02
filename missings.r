# print_missings_shadows
# input is a tibble
# output is graphic
#


print_missing_shadows <- function(tib) {
    library(ggpubr)
    library(viridis)
    library(UpSetR)
    library(naniar)
    tib %>% as_shadow_upset() %>% upset()
}