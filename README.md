# Probability Distribution Calculator

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

This app calculates the area under the curve (probability) for five common
statistical distributions. You specify the distribution, its parameters, and
the region of interest (lower tail, upper tail, both tails, middle, or exact
value), and the app shades the relevant area on the plot and reports the
probability.

A plain-English interpretation of the result is shown below the plot, along
with the exact R code you would use to reproduce the calculation yourself.

## Distributions supported

- Normal (mean and SD adjustable)
- Binomial (n and p adjustable)
- t (degrees of freedom adjustable)
- F (two sets of degrees of freedom)
- Chi-squared (degrees of freedom adjustable)

## How to use

1. Select a distribution and set its parameters on the left.
2. Choose the region type: lower tail, upper tail, both tails, middle, or exact.
3. Set the boundary value(s) a (and b for two-boundary regions).
4. Read the shaded area, interpretation, and R code in the main panel.

## Learning goals

- Connect the visual area under a curve to a numerical probability
- Understand what a p-value means in terms of tail area
- See the R functions (pnorm, pt, pf, pchisq, pbinom) that perform these
  calculations

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).