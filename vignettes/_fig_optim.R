# Shrink the rendered vignette figures.
#
# Why this file exists:
#
#   The vignettes never chose a graphics device, so they fell through to the
#   default png(), which writes RGBA truecolor -- an alpha channel these opaque
#   plots never use, over ~30,000 anti-aliased colours that PNG cannot compress.
#   The result was 3.5 MB of figures in inst/doc and a 4.7 MB tarball against
#   CRAN's 5 MB limit, none of it visible in the source tree (inst/doc only
#   exists after R CMD build renders it).
#
#   Two independent savings, measured on the gg_rfsrc() survival-curve figure:
#     * ragg::agg_png writes RGB, dropping the unused alpha channel   (-20%)
#     * quantising to a 256-colour palette absorbs the anti-aliasing  (-54%)
#   256 colours is visually indistinguishable here: mean |pixel difference|
#   against the unquantised render is 1.55 on a 0-255 scale (~0.6%).
#
# colorspace = "sRGB" is load-bearing. ImageMagick's "RGB" means *linear* RGB,
# so quantising in it silently undoes the sRGB gamma curve and shifts every
# pixel -- that lands at mean |diff| 24.5, which is plainly visible as darker,
# harsher curves. magick's default does the same thing. The figures that suffer
# most are exactly the ones worth protecting: gg_rfsrc() and gg_variable() draw
# hundreds of alpha-blended curves whose opacity gradations *are* the density
# information, so a tonal shift corrupts what the plot is there to show.
#
# Both steps are build-time only and degrade to no-ops. ragg and magick are
# Suggests: if either is missing, the figure is left exactly as rendered and the
# vignette still builds. That matters because CRAN rebuilds vignettes on
# machines whose ImageMagick we do not control -- a missing library must cost us
# file size, never a failed check.

quantize_png <- function(path) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    return(path)
  }
  img <- magick::image_read(path)
  magick::image_write(
    magick::image_quantize(img, max = 256, colorspace = "sRGB"),
    path = path, format = "png"
  )
  path
}

if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png")
}
knitr::opts_chunk$set(fig.process = quantize_png)
