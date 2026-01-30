#' Title
#'
#' @param summary_df Outputs of nutrients
#'
#' @returns macro distribution dataframe
#' @export
macro_distribution <- function(summary_df) {

  macro_distr <- summary_df |>
    dplyr::mutate(
      protein_kcal = protein_g * 4,
      carb_kcal    = carb_g * 4,
      fat_kcal     = fat_g * 9,
      total_macro_kcal = protein_kcal + carb_kcal + fat_kcal,
      protein_pct = protein_kcal / total_macro_kcal * 100,
      carb_pct    = carb_kcal    / total_macro_kcal * 100,
      fat_pct     = fat_kcal     / total_macro_kcal * 100
    )

  return(macro_distr)

}
