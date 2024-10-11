############# TESTS ###############

# d'après https://stackoverflow.com/questions/64845777/compute-statistical-tests-groupwise-in-r-using-dplyr


#' Mann-Kendall test combined to Sen-Theil slope estimation on a vector
#'
#' @param x A vector of class "numeric" or a time series object of class "ts".
#' @param ... Arguments to be passed to the function trend::mk.test()
#'
#' @return A dataframe with the test statistics.
#' @export
#' 
#' @importFrom trend mk.test sens.slope
#'
#' @examples
#' vector <- c(0, 3, 2, 5, 7, 6, 9, 8, 13, 16, 12)
#' mann_kendall_sen(vector)
mann_kendall_sen <- function(x, ...) 
{
  mk_pvalue <- mk.test(x, ...)
  sens_slope <- sens.slope(x, ...)
  
  # output
  data.frame(mk_pvalue = mk_pvalue$p.value,
             sens_slope = sens_slope$estimates)
}


#' Multiple Mann-Kendall test combined to Sen-Theil slope estimation (1 grouping variable)
#' 
#' The input dataframe must contain one value of var_y for each var_group x var_x combination.
#' The groups with less than 3 observations are removed
#'
#' @param df Dataframe containing the data. Must contain the variables described below.
#' @param var_groupe Grouping variable. One slope will be estimated by group. 
#' @param var_y Variable to be tested (is its slope significant ?).
#' @param var_x Variable used to order var_y (e.g. time in case of temporal trend test).
#'
#' @return A dataframe with the statistics for each group.
#' @export
#' 
#' @importFrom dplyr enquo arrange group_by group_modify pull ungroup mutate case_when
#'
#' @examples
#' \dontrun{
#' tester_pente_mk_multi(df = fbi_metrics_median,
#'   var_groupe = metric,
#'   var_x = year,
#'   var_y = p50)
#' }
tester_pente_mk_multi <- function(df,
                                  var_groupe,
                                  var_y,
                                  var_x)
  
{
  var_groupe <- enquo(var_groupe)
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df <- df %>% 
    filter(!is.na(!!var_y))
  
  kept_groups <- df %>% 
    group_by(!!var_groupe) %>% 
    tally() %>% 
    filter(n > 2) %>%  # MK test requires 3 data mini
    pull(!!var_groupe) %>% 
    unique()
  
  df <- df %>% 
    filter(!!var_groupe %in% kept_groups)           
  
  output <- df %>%
    arrange(!!var_groupe, !!var_x) %>% 
    group_by(!!var_groupe) %>%
    group_modify(~ mann_kendall_sen(.x %>% pull(!!var_y))) %>% 
    ungroup() %>% 
    mutate(sig = ifelse(mk_pvalue < 0.05, TRUE, FALSE),
           trend = case_when(
             sign(sens_slope) == 1 & sig ~ "Increase",
             sign(sens_slope) == -1 & sig ~ "Decrease",
             TRUE ~ "No trend"))
  
  return(output)
  
}

#' Multiple Mann-Kendall test combined to Sen-Theil slope estimation (2 grouping variables)
#' 
#' The input dataframe must contain one value of var_y for each var_group x var_x combination.
#'
#' @param df Dataframe containing the data. Must contain the variables described below.
#' @param var_groupe1,var_groupe2  Grouping variables. One slope will be estimated by grouping 
#'     variables combination. 
#' @param var_y Variable to be tested (is its slope significant ?).
#' @param var_x Variable used to order var_y (e.g. time in case of temporal trend test).
#'
#' @return A dataframe with the statistics for each group.
#' @export
#' 
#' @importFrom dplyr enquo arrange group_by group_modify pull ungroup mutate case_when
#'
#' @examples
#' \dontrun{
#' tester_pente_mk_multi2(df = fbi_metrics_median,
#'   var_groupe1 = metric,
#'   var_groupe1 = network,
#'   var_x = year,
#'   var_y = p50)
#' }
tester_pente_mk_multi2 <- function(df,
                                   var_groupe1 = NULL,
                                   var_groupe2 = NULL,
                                   var_y,
                                   var_x)
  
{
  var_groupe1 <- enquo(var_groupe1)
  var_groupe2 <- enquo(var_groupe2)
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df <- df %>% 
    filter(!is.na(!!var_y))
  
  kept_groups <- df %>% 
    group_by(!!var_groupe1, !!var_groupe2) %>% 
    tally() %>% 
    filter(n > 2) %>%  # MK test requires 3 data mini
    select(!!var_groupe1, !!var_groupe2) %>% 
    distinct()
  
  df <- kept_groups %>% 
    left_join(df)    
  
  output <- df %>%
    filter(!is.na(!!var_y)) %>% # avoids error for combinations without enough values
    arrange(!!var_groupe1, !!var_groupe2, !!var_x) %>%
    group_by(!!var_groupe1, !!var_groupe2) %>% 
    group_modify(~ mann_kendall_sen(.x %>% pull(!!var_y))) %>%
    ungroup() %>%
    mutate(sig = ifelse(mk_pvalue < 0.05, TRUE, FALSE),
           trend = case_when(
             sign(sens_slope) == 1 & sig ~ "Increase",
             sign(sens_slope) == -1 & sig ~ "Decrease",
             TRUE ~ "No trend"))
  
  
  
  
  return(output)
  
}

