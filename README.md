# Probability Distribution Calculator

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

This app calculates the area under the curve (probability) for seven
statistical distributions used in the course. You choose the distribution, set
its parameters, and pick the region of interest. The app shades that area on
the plot, marks the boundary with a dashed line and an arrow showing which way
the area runs, and reports the probability.

A plain-English interpretation of the result is shown below the plot, along
with the exact R code you would use to reproduce the calculation yourself.

Every distribution opens on the upper tail, with the boundary set at the value
you would look up in a critical-value table (for example, the t value that
leaves 2.5% in the upper tail). The app can then be used alongside the tables
in Whitlock & Schluter to see what those table values actually mean.

## Distributions supported

- Normal (mean and SD adjustable)
- Binomial (n and p adjustable)
- t (degrees of freedom adjustable)
- F (two sets of degrees of freedom)
- Chi-squared (degrees of freedom adjustable)
- Tukey q, for multiple comparisons after ANOVA (number of groups k and error
  degrees of freedom adjustable)
- Mann-Whitney U (sample sizes n1 and n2 adjustable)

### Notes on Tukey q

By default, q is scaled to match the q table in Whitlock & Schluter. Untick
"Use Whitlock & Schluter's q" to switch to the version used by R's `qtukey()`
and most other textbooks, which is larger by a factor of the square root of 2.
The interpretation also reports the critical value of q at alpha = 0.05.

### Notes on Mann-Whitney U

U only takes whole-number values, so it is drawn as bars, like the binomial.
The default region is the two-tailed P-value. Enter either U1 or U2: because
U1 + U2 = n1 x n2, the app marks both values and gives the same P-value
either way. The interpretation also reports the critical value of U at
alpha = 0.05, matching the Whitlock & Schluter table (reject the null
hypothesis if the larger of U1 and U2 is equal to or greater than this value).

## How to use

1. Select a distribution and set its parameters on the left.
2. Choose the region type. Depending on the distribution, this can be the
   lower tail, upper tail, both tails, middle, an exact value, or the
   two-tailed P-value.
3. Set the boundary value a (and b for two-boundary regions).
4. Read the shaded area, interpretation, and R code in the main panel.

## Learning goals

- Connect the visual area under a curve to a numerical probability
- Understand what a p-value means in terms of tail area
- See where the values in printed critical-value tables come from
- See the R functions (`pnorm`, `pt`, `pf`, `pchisq`, `pbinom`, `ptukey`,
  `pwilcox`) that perform these calculations

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).