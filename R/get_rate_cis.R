get_rate_cis <- function(x, per = 100000, alpha = 0.05){

    if (alpha == 0.05) {

        ci <- 1.96

        ll <- as_tibble_col(
            (per/pull(x, pop_estimate))*(pull(x, n) - (ci*sqrt(pull(x, n)))), "lower_lim") %>%
            round(., 1)%>%
            mutate(lower_lim = if_else(lower_lim < 0, 0.0, lower_lim))

        ul <- as_tibble_col(
            (per/pull(x, pop_estimate))*(pull(x, n) + (ci*sqrt(pull(x, n)))), "upper_lim") %>%
            round(., 1)

    }

    if (alpha == 0.01) {

        ci <- 2.68

        ll <- as_tibble_col(
            (per/pull(x, pop_estimate))*(pull(x, n) - (ci*sqrt(pull(x, n)))), "lower_lim") %>%
            round(., 1)%>%
            mutate(lower_lim = if_else(lower_lim < 0, 0.0, lower_lim))

        ul <- as_tibble_col(
            (per/pull(x, pop_estimate))*(pull(x, n) + (ci*sqrt(pull(x, n)))), "upper_lim") %>%
            round(., 1)

    }

    bind_cols(x, ll, ul)

}
