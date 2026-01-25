#' Nutrients
#'
#' The Nutrients function takes the age of a participant and their responses on
#' the hsffq to generate an estimate of the participant's total daily
#' micronutrients, macronutrients, and calories consumed for each food
#'
#' @param  df data frame with rows (individual observations) and
#' columns, representing 90 different frequency factor responses from the
#' eutops FFQ
#' @param label_col Column name of column containing food label variable
#' @param name_col Column name of column containing food name
#' @param nutrient_cols Column names of nutrient outputs to be returned
#' @return a list with two items: summary and per food group nutrients and
#' macros and per_food group
#' @export
nutrients <- function(df,
                      label_col = 'varlabel',
                      name_col = 'foodname',
                      nutrient_cols = c('serving_size',
                                        'calories_kcal',
                                        'protein_g',
                                        'fat_g',
                                        'carb_g',
                                        'calcium_mg',
                                        'iron_mg',
                                        'zinc_mg',
                                        'vitamin_c_mg',
                                        'vitamin_b6_mg',
                                        'vitamin_a_mg',
                                        'folate_mg')){

  data("eutopsFQ_db", package = "eutopsFQ", envir = environment())
  key <- as.character(eutopsFQ_db[[label_col]])
  vars <- intersect(colnames(df), key)
  idx <- match(vars, key)
  food_labels = as.character(eutopsFQ_db[[name_col]])[idx]

  # Store responses as matrix
  X <- as.matrix(df[, vars, drop = FALSE])
  storage.mode(X) <- "double"

  # Recode requency
  F <- fq(X)  # same dims as X

  # Nutrient weights
  W <- as.matrix(eutopsFQ_db[idx, nutrient_cols, drop = FALSE])
  storage.mode(W) <- "double"

  out <- as.data.frame(F %*% W)
  names(out) <- nutrient_cols
  out <- out |>
    dplyr::select(-serving_size)

  per_food_list <- lapply(seq_along(nutrient_cols), function(j) {
    mat <- F * matrix(W[,j], nrow = nrow(F), ncol = ncol(F),
                      byrow = T)
    colnames(mat) <- paste0(food_labels, "__", nutrient_cols[j])
    mat
  })
  per_food <- as.data.frame(do.call(cbind, per_food_list))

  return(list(summary = out,
              per_food = per_food))

}
