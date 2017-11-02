#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' var_names()

#lubripack::lubripack("tidyverse","labelled","haven","sjmisc","magrittr")

### Hilfsfunktion ###

# var_names findet Variablennamen -- benötigt "labelled" und "tidyverse" package!
var_names <- function(data, keyword = "") {
  keyword <- ifelse(keyword %in% "all", "", keyword)
  #if 'all' turn into void, else copy keyword
  lablist <-  data %>%
    var_label() %>% # extract variable labels
    bind_rows() %>% # binding list elements as dataframe
    t() # transpose dataframe
  name_pos <- stringr::str_detect(tolower(lablist[, 1]), tolower(keyword))
  # get position of string
  if(any(name_pos)){ #if the string is found
    dat <-data.frame(var_codes=names(lablist[name_pos, ]),
                     var_names=lablist[name_pos, ],
                     row.names = NULL, stringsAsFactors = F)
    #colnames(dat) <- "var_names"
    cat(paste0("####---- Nice! You found ", nrow(dat) , " variables! ----#### \n \n "))
    return(dat)
  } else{
    cat("Variable Name not found. Try again, Stupid!")
  }
}


