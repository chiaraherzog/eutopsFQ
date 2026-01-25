#' Frequency Factor
#'
#' The Frequency Factor function converts values 0-8, representing different
#' frequency factor responses from the eutops ffq, to average daily servings consumed
#' for that individual.
#'
#' @param f 0-8, representing different frequency factor responses from the
#' eutops ffq. These can be in a dataframe, vector, or just single values
#' Coding is as follows:
#'    0 =		Never or <1x/month
#'    1 =		1-3x/month
#'    2 = 	1x/week
#'    3 = 	2-4x/week
#'    4 = 	5-6x/week
#'    5 = 	1x/day
#'    6 = 	2-3x/day
#'    7 = 	4-5x/day
#'    8 = 	6x/day or more
#' @return a dataframe, vector, or single value of the same dimension as the
#' input, with each position holding the average daily servings consumed
#' for each food (columns) for each individual(rows).
#' @export
#' @examples
#' test <- c(1, 5, 7, 3, 9, 2, 4, 3, 6, 8)
#' fq(test)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(6)
#'
#' fq(df)
#'
fq <- function(f) {
  ifelse(f > 7, 6,
         ifelse(f > 6, 4.5,
                ifelse(f > 5, 2.5,
                       ifelse(f > 4, 1,
                              ifelse(f > 3, 0.8,
                                     ifelse(f > 2, 0.43,
                                            ifelse(f > 1, 0.14,
                                                   ifelse(f == 1, 0.08,
                                                          0)
                                            )
                                     )
                              )
                       )
                )
         )
  )
}
