str_to_date <- function(x){

    std_year <-
        dplyr::case_when(
            stringr::str_sub(x, start = 1, end = 4) %in% c("", "....") ~ "1900",
            TRUE ~ stringr::str_sub(x, start = 1, end = 4))

    std_month <-
        dplyr::case_when(
            stringr::str_sub(x, start = 5, end = 6) %in% c("", "..") ~ "01",
            TRUE ~ stringr::str_sub(x, start = 5, end = 6))

    std_day <-
        dplyr::case_when(
            stringr::str_sub(x, start = 7, end = 8) %in% c("", "..") ~ "01",
            TRUE ~ stringr::str_sub(x, start = 7, end = 8))

    std_date <- lubridate::mdy(paste0(std_month, '/', std_day, '/', std_year))

}
