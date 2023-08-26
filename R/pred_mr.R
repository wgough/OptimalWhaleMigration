# Hello, world!
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

pred_mr <- function(
  mass_kg,
  mr_method = c("kleiber", "kolokotronesetal", "nagy", "savageetal", "whiteseymour"),
  unit = c("kcal_day", "kJ_day", "mlO2_hr", "W"),
  multiplier = 1
) {
  #' Predicted metabolic rate
  #'
  #' Predict mammalian metabolic rates from various scaling equations.
  #'
  #' The scaling relationship between metabolic rate and body size is often
  #' debated. The "Kleiber Curve" asserts a 3/4 power law (Kleiber 1975), which is
  #' supported by some modern studies (Savage et al. 2004) whereas others find a
  #' 2/3 power law (White and Seymour 2003). The very validity of power laws for
  #' analyzing metabolic rates has even been called into question (Kolokotrones et
  #' al. 2010). Furthermore, different measurements of metabolic rates, such as
  #' basal and field, may scale differently (Nagy 1999).
  #'
  #' @param mass_kg Body mass in kg
  #' @param mr_method Scaling equation to use. One of "kleiber",
  #'   "kolokotronesetal", "nagy", "savageetal", or "whiteseymour".
  #' @param unit Unit of predicted metabolic rate. One of "kcal_day" (kilocalories
  #'   per day), "kJ_day" (kilojoules per day), "mlO2_hr" (ml O2 per hour), "W"
  #'   (watts).
  #' @param multiplier Multiplier for converting BMR to FMR.
  #'
  #' @return Estimated metabolic rate in the unit specified by \code{unit}.
  #'
  #' @references
  #' Kleiber, Max. \emph{The Fire of Life: An Introduction to Animal Energetics}. Huntington, NY: R. E. Krieger, 1975.
  #'
  #' Kolokotrones, Tom, Van Savage, Eric J. Deeds, and Walter Fontana. “Curvature in Metabolic Scaling.” \emph{Nature} 464, no. 7289 (April 2010): 753–56. \url{https://doi.org/10.1038/nature08920}.
  #'
  #' Nagy, K. A., I. A. Girard, and T. K. Brown. “Energetics of Free-Ranging Mammals, Reptiles, and Birds.” \emph{Annual Review of Nutrition} 19, no. 1 (1999): 247–77. \url{https://doi.org/10.1146/annurev.nutr.19.1.247}.
  #'
  #' Savage, V. M., J. F. Gillooly, W. H. Woodruff, G. B. West, A. P. Allen, B. J. Enquist, and J. H. Brown. “The Predominance of Quarter-Power Scaling in Biology.” \emph{Functional Ecology} 18, no. 2 (2004): 257–82. \url{https://doi.org/10.1111/j.0269-8463.2004.00856.x}.
  #'
  #' White, Craig R., and Roger S. Seymour. “Mammalian Basal Metabolic Rate Is Proportional to Body Mass\ifelse{html}{<sup>2/3</sup>}{\textsuperscript{2/3}}.” \emph{Proceedings of the National Academy of Sciences of the United States of America} 100, no. 7 (April 1, 2003): 4046–49. \url{https://doi.org/10.1073/pnas.0436428100}.
  #'
  #' @examples
  #' # Nagy-estimated FMR of a small mammal in watts
  #' pred_mr(0.5, fmr_method = "nagy", unit = "W")
  #' # Kleiber BMR of a human-sized mammal
  #' pred_mr(100, fmr_method = "kleiber", unit = "kcal_day", multiplier = 1)
  #' # FMR of a blue whale predicted using Kolokotrones et al.'s quadratic BMR with a 2.5x multiplier in ml O2 per hour.
  #' pred_mr(1e5, fmr_method = "kolokotronesetal", unit = "mlO2_hr", multiplier = 2.5)
  #' # BMR curves from White and Seymour (2004) and Kolokotrones et al. (2010)
  #' body_size <- 10^seq(-1, 4, length.out = 10)
  #' bmr_whiteseymour <- pred_mr(body_size, "whiteseymour", "kcal_day")
  #' bmr_kolokotrones <- pred_mr(body_size, "kolokotronesetal", "kcal_day")
  #' plot(log10(body_size), log10(bmr_whiteseymour),
  #'      type = "l", lty = 1, col = "blueviolet",
  #'      xlab = expression(log[10]*(Mass~(kg))),
  #'      ylab = expression(log[10]*(BMR~(kcal~day^{-1}))),
  #'      ylim = log10(range(bmr_whiteseymour, bmr_kolokotrones)))
  #' lines(log10(body_size), log10(bmr_kolokotrones), lty = 2, col = "brown")
  #' legend(
  #'   "topleft",
  #'   legend = c("White & Seymour (2004)", "Kolokotrones et al. (2010)"),
  #'   lty = c(1, 2),
  #'   col = c("blueviolet", "brown"),
  #'   bty = "n"
  #' )

  # Check arguments
  stopifnot(is.numeric(mass_kg))
  fmr_method <- match.arg(mr_method)
  unit <- match.arg(unit)
  stopifnot(is.numeric(multiplier), length(multiplier) == 1)

  # Conversion factors
  kcal_day_to_W <- 0.04843
  kJ_day_to_W <- 0.01157
  mlO2_hr_to_W <- 20.1 / 3600 # assuming 20.1 J per ml O2

  # Estimate FMR in W
  mass_g <- mass_kg * 1000
  mr_W <- switch(
    mr_method,
    # Kleiber 1975 (kcal day^-1)
    # p. 179
    kleiber = 70 * mass_kg^0.75 * kcal_day_to_W,
    # Kolokotrones et al 2010 (W)
    # Table 1
    kolokotronesetal = 10^(-1.5078 + 0.5400 * log10(mass_g) + 0.0322 * log10(mass_g)^2),
    # Nagy 1999 (kJ day^-1)
    # Table 2, Mammals
    nagy = 4.82 * mass_g^0.734 * kJ_day_to_W,
    # Savage et al. 2004 (W)
    # Figure 2, BMR
    savageetal = 10^(-1.739 + 0.737 * log10(mass_g)),
    # White and Seymour 2003 (ml O2 hr^-2)
    # Figure 2B
    whiteseymour = 4.34 * mass_g^0.67 * mlO2_hr_to_W
  ) * multiplier

  # Convert to appropriate units
  mr_W * switch(
    unit,
    kcal_day = 1 / kcal_day_to_W,
    kJ_day = 1 / kJ_day_to_W,
    mlO2_hr = 1 / mlO2_hr_to_W,
    W = 1
  )
}


