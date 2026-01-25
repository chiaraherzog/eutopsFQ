#' Food Quotient Based on Macronutrients computed from nutrients()
#'
#' The macquotient function calculates a food quotient for a participant based
#' on average daily protein, carbs, and fat consumed for an individual or a
#' group. In contrast to the quotient function, macquotient is able to generate
#' reliable average food quotients for a group of people rather than only
#' individual level. Group level estimates are recomended in some studies to
#' control for response bias.
#'
#' @param  df data frame (returned from nutrients() function)
#' @param protein_col Column name of column containing protein_g information
#' @param fat_col Column name of column containing fat_g information
#' @param carb_col Column name of column containing carb_g information
#' @return one value per participant will be returned, representing the food
#'   quotient for the individual
#' @export
#'
quotient <- function(df,
                        protein_col = 'protein_g',
                        fat_col = 'fat_g',
                        carb_col = 'carb_g') {
  p1 = df[[protein_col]] * 4
  f1 <- df[[fat_col]] * 9
  c1 <- df[[carb_col]] * 4

  denom <- p1+f1+c1

  rq <- ifelse(denom > 0,
               (p1/denom)*0.81 + (f1/denom)*0.7 + (c1/denom)*1,
               NA_real_)

  df_new <- data.frame(df,
                       RQ = rq)
  return(df_new)

}

