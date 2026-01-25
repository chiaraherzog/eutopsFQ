#' make components for dietary indices
#'
#' @param intake output of grams function
#'
#' @returns dataframe with components grouped
#' @export
#'
make_components <- function(intake) {
  intake |>
    dplyr::transmute(

      # ----- Fruits (g) -----
      fruits = pick_items(
        intake,
        items = c("grape_raisin","apple_pear","peaches","orange","lemon",
                  "watermelon","sugar_melon","strawberries","blueberry",
                  "plum","pineapple","mango","cherries","banana"),
        unit = "g"
      ),

      # ----- Vegetables (g) -----
      vegetables = pick_items(
        intake,
        items = c("tomato","tomato_sauce","zucchini_cucumber","lettuce","cabbage",
                  "broccoli","cauliflower","carrots","beets","peas","green_beans",
                  "spinach_raw","spinach_cooked","celery","garlic","mushroom"),
        unit = "g"
      ),

      # ----- Legumes (g) -----
      legumes = pick_items(intake, c("lentils","soya"), unit = "g"),

      # ----- Nuts & seeds (g) -----
      nuts_seeds = pick_items(intake, c("nuts","flax_wheatgerm"), unit = "g"),

      # ----- Fish (g) -----
      fish = pick_items(
        intake,
        c("tuna_sardines","fish_freshwater","fish_saltwater"),
        unit = "g"
      ),

      # ----- Whole grains (g) -----
      whole_grains = pick_items(intake, c("bread_wholegrain"), unit = "g"),

      # ----- Refined grains (g) -----
      refined_grains = pick_items(
        intake,
        c("bread_white","pasta","dumpling","rice","polenta","cold_cereal"),
        unit = "g"
      ),

      # ----- Dairy (g) -----
      dairy_total = pick_items(
        intake,
        c("milk","low_fat_milk","yogurt","curd_cheese","cheese","cream"),
        unit = "g"
      ),

      lowfat_dairy = pick_items(
        intake,
        c("low_fat_milk","yogurt"),
        unit = "g"
      ),

      # ----- Meats (g) -----
      red_meat = pick_items(
        intake,
        c("beef_veal","pork","game_meat","mutton_venison"),
        unit = "g"
      ),

      processed_meat = pick_items(
        intake,
        c("cold_cuts","bacon"),
        unit = "g"
      ),

      # ----- Sweets (g) -----
      sweets = pick_items(
        intake,
        c("chocolate","pastry_donut_muffin","ice_cream",
          "candy","honey","cookies","jam"),
        unit = "g"
      ),

      # ----- Sugar-sweetened beverages (mL) -----
      ssb = pick_items(
        intake,
        c("coke","fanta_sprite","energy_drink"),
        unit = "ml"
      ),

      # ----- Alcohol (mL beverage; ethanol handled separately if needed) -----
      alcohol = pick_items(
        intake,
        c("red_wine","white_wine","beer","liqueur_schnapps"),
        unit = "ethanol_g"
      ),

      # ----- Fats/oils (g) -----
      fats = pick_items(
        intake,
        c("olive_oil","seed_oil","butter","margarine","lard","mayo"),
        unit = "g"
      )
    )
}


#' pick_items is a helper function for make_components to ensure we can grab
#' the relevant items (within make_components)
#'
#' @param df output of the grams() function element
#' @param items this is defined in the make_components function
#' @param unit unit element
#'
#' @returns rowSums
#' @export
pick_items <- function(df,
                       items,
                       unit = c("g", "ml", "ethanol_g")) {
  unit <- match.arg(unit)
  cols <- paste0("^(", paste(items, collapse = "|"), ")__", unit, "$")
  df |>
    dplyr::select(matches(cols)) |>
    rowSums(na.rm = TRUE)
}

#' Mediterranean Diet score (MDS) by Trichopoulou
#'
#' @param components output of make_components from grams function
#' @param alcohol_col label of alcohol column
#' @param alcohol_range_g range - 5 to 25
#' @param include_alcohol T or F
#'
#' @returns MDS score tibble
#' @export
mds_trichopoulou <- function(components,
                             alcohol_col = "alcohol",
                             alcohol_range_g = c(5, 25),  # g/day ethanol equivalent
                             include_alcohol = TRUE) {

  # Beneficial components
  benef <- c("fruits", "vegetables", "legumes", "fish", "nuts_seeds", "whole_grains")

  # Detrimental components
  detr <- c("red_meat", "processed_meat", "dairy_total")

  # Medians
  med_benef <- sapply(benef, \(x) median(components[[x]], na.rm = TRUE))
  med_detr  <- sapply(detr,  \(x) median(components[[x]], na.rm = TRUE))

  score_benef <- rowSums(
    sweep(components[, benef, drop = FALSE], 2, med_benef, `>`),
    na.rm = TRUE
  )

  score_detr <- rowSums(
    sweep(components[, detr, drop = FALSE], 2, med_detr, `<`),
    na.rm = TRUE
  )

  score_alc <- 0
  if (include_alcohol && alcohol_col %in% names(components)) {
    a <- components[[alcohol_col]]
    score_alc <- ifelse(!is.na(a) & a >= alcohol_range_g[1] & a <= alcohol_range_g[2], 1, 0)
  }

  MDS <- tibble::tibble(MDS = score_benef + score_detr + score_alc)

  return(MDS)
}

#' modifiedDASH (7) for DASH diet index
#'
#' @param components output of make_components() from grams()
#' @param ssb_col soda column label
#'
#' @returns dash score DF
#' @export
modifiedDASH <- function(components,
                         ssb_col = "ssb") {

  df <- components |>
    dplyr::mutate(
      redproc_meat = red_meat + processed_meat
    )

  qscore <- function(x, reverse = FALSE, k = 5L) {
    # If all NA or constant, return neutral (3)
    if (all(is.na(x)) || dplyr::n_distinct(x, na.rm = TRUE) <= 1) {
      out <- rep(3L, length(x))
      return(out)
    }

    # Rank-based binning avoids non-unique quantile breaks
    r <- rank(x, na.last = "keep", ties.method = "average")
    bins <- dplyr::ntile(r, k)

    if (reverse) (k + 1L) - bins else bins
  }


  scores <- tibble::tibble(
    dash_fruits       = qscore(df$fruits),
    dash_vegetables   = qscore(df$vegetables),
    dash_nuts_legumes = qscore(df$nuts_seeds + df$legumes),
    dash_whole_grains = qscore(df$whole_grains),
    dash_lowfat_dairy = qscore(df$lowfat_dairy),
    dash_redproc_meat = qscore(df$redproc_meat, reverse = TRUE),
    dash_ssb          = qscore(df[[ssb_col]], reverse = TRUE)
  )

  scores <- scores |>
    dplyr::mutate(DASH7 = rowSums(dplyr::across(dplyr::everything()), na.rm = FALSE))

  return(scores)

}

#' Refined carb score
#'
#' @param components output of make_components()
#'
#' @returns refined carb score value; quintiles from 1-5 (lower is better)
#' @export
RefCarb <- function(components) {
  burden <- components$refined_grains + components$ssb + components$sweets
  dplyr::ntile(-burden, 5)
}

#' Low carb diet score
#'
#' @param summary nutrients() summary of carb/protein/fat per day
#'
#' @returns Low-carb diet score in percentiles, whigh higher scores being lower carb
#' @export
LCD <- function(summary) {
  pct_carb <- (summary$carb_g * 4) /
    (summary$protein_g*4 + summary$fat_g*9 + summary$carb_g*4)

  # higher score = lower carb
  dplyr::ntile(1 - pct_carb, 10)
}

#' healthy Plant Diet Index
#'
#' @param components output of make_components()
#'
#' @returns healthy plant diet index, ranging from ~ -9 to +3
#' @export
hPDI <- function(components) {

  healthy_plant <- components$fruits +
    components$vegetables +
    components$legumes +
    components$nuts_seeds +
    components$whole_grains

  unhealthy_plant <- components$refined_grains +
    components$sweets +
    components$ssb

  animal <- components$red_meat +
    components$processed_meat +
    components$dairy_total

  score <- dplyr::ntile(healthy_plant, 5) -
    dplyr::ntile(unhealthy_plant, 5) -
    dplyr::ntile(animal, 5)

  return(score)
}



