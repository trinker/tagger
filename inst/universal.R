library(dplyr)

universal_pos_map <- setNames(lapply(file.path("universal-pos", dir("universal-pos")), function(x){
    y <- data.frame(do.call(rbind, strsplit(readLines(x), "\\s+")), stringsAsFactors = FALSE)
    setNames(y, c("tag", "universal")) %>%
    dplyr::mutate(tag = Trim(tag),
         universal = Trim(universal)
    )

}), tools::file_path_sans_ext(dir("universal-pos"))) 


