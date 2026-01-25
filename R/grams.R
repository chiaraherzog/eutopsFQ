#' Grams
#'
#' The grams function takes responses on the eutops FFQ to generate an
#' estimate of the participant's total daily units/grams consumed
#' for each food.
#'
#' @param df A dataframe representing different frequency factor
#' responses from the FFQ (columns = items, rows = individuals)
#' @param label_col Column name of column containing food label variable
#' @param name_col Column name of column containing food name
#' @param serving_col Column name of column containing serving size
#' @param unit_col Column name of column containing serving size unit
#' @return A dataframe representing the estimated total
#' daily grams (or ml) of each food consumed for the participant.
#' @export
grams <- function(df,
                  label_col = 'varlabel',
                  name_col = 'foodname',
                  serving_col = 'serving_size',
                  unit_col = 'unit') {

  stopifnot(is.data.frame(df))

  # Build named serving-size lookup: names are varlabels
  data("eutopsFQ_db", package = "eutopsFQ", envir = environment())
  key <- as.character(eutopsFQ_db[[label_col]])
  ss_map <- setNames(eutopsFQ_db[[serving_col]], key)

  # Vars to convert
  vars <- intersect(colnames(df), names(ss_map))
  if (length(vars) == 0) {
    stop("No matching columns between df and eutopsFQ_db varlabels.")
  }
  idx <- match(vars, key)
  food_labels <- as.character(eutopsFQ_db[[name_col]][idx])
  unit_labels <- as.character(eutopsFQ_db[[unit_col]][idx])
  food_labels <- paste(food_labels, "__", unit_labels, sep = '')

  # Align serving sizes to df column order
  ss_vec <- ss_map[vars]

  X <- as.matrix(df[, vars, drop = FALSE])
  storage.mode(X) <- "double"

  grams_mat <- fq(X) * matrix(ss_vec, nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
  grams_df <- as.data.frame(grams_mat)
  names(grams_df) <- food_labels

  grams_df_w_alcohol <- alcohol_g_ethanol(grams_df)

  return(grams_df_w_alcohol)

}



#' alcohol_g_ethanol
#' Function to compute g of alcohol per day from ml
#'
#' @param grams_df grams dataframe
#' @param alcohol_items which alcohol items exist
#' @param abv approximate percentage
#' @param rho ethanol density
#' @param suffix which suffix should be given to the new column?
#'
#' @returns dataframe with appended ethanol g per item and overall
#' @export
alcohol_g_ethanol <- function(grams_df,
                              alcohol_items = c("beer",
                                                "red_wine",
                                                "white_wine",
                                                "liqueur_schnapps"),
                              abv = c(beer = 0.05,
                                      red_wine = 0.12,
                                      white_wine = 0.12,
                                      liqueur_schnapps = 0.35),
                              rho = 0.789,
                              suffix = '__ethanol_g') {

  # Find ml columns and extract basenames
  ml_cols <- grep("__ml$", colnames(grams_df), value = T)
  if (length(ml_cols) == 0) {
    warning("No __ml columns found; no ethanol computed.")
    return(grams_df)
  }
  basenames <- sub("__ml$", "", ml_cols)
  keep <- basenames %in% alcohol_items
  if (!any(keep)) {
    warning("No recognised alcohol beverages among __ml columns.")
    return(grams_df)
  }

  ml_cols <- ml_cols[keep]
  basenames <- basenames[keep]

  for(i in seq_along(ml_cols)){
    nm <- basenames[i]
    grams_df[[paste0(nm, suffix)]] <-
      as.numeric(grams_df[[ml_cols[i]]] * abv[[nm]] * rho)
  }

  # Total ethanol grams/day
  ethanol_cols <- paste0(basenames, suffix)
  grams_df$ethanol_g_day <- rowSums(grams_df[, ethanol_cols, drop = FALSE], na.rm = TRUE)

  return(grams_df)
}
