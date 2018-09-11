## Make a joyplot of any scale by any factor variable
# wtib = work tibble (data)
# fctr = factor variable
# fctr_nm = Factor name for axis title
# scl = scale variable
# title = Plot title
# sample_nm = Sample name, e.g., "Vaccines"
# sample_num = sample number for filename (e.g., 2016)
#
# prints the plot to a file with the name format "Joyplot", sample_num, " ", title, ".png", sep = ""

joyplot_scale_by_factor <- function(plot_tib, 
                                    fctr, fctr_nm,
                                    scl, scl_nm,
                                    title, 
                                    sample_nm, sample_num) {
        shhh(library(ggjoy))
        shhh(library(ggridges))
        shhh(library(scales))
        shhh(library(lubridate))
        ggplot(plot_tib, aes(y = fctr, x = scl)) +
                geom_density_ridges(scale = 4, 
                                    aes(fill = fctr),
                                    alpha = 3/4, 
                                    na.rm = FALSE,
                                    quantile_lines = TRUE,
                                    quantiles = 2) +
                scale_x_continuous(scl_nm) +
                guides(fill = FALSE, color = FALSE) +
                labs(title = title, 
                     y = fctr_nm, 
                     x = "How Characteristic", 
                     caption = (paste(sample_nm,
                                      today(), 
                                      sep = " "))) +
                z_theme()
        plotfile <- paste("Joyplot", 
                          " ", title, 
                          ".png",
                          sep = "")
        ggsave(plotfile,
               height = 8, 
               width = 8, 
               dpi = 120,
               type = "cairo-png")
}
