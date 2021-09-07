
#-------------------------------------------------------------------------------
# lmForc_hexSticker.R written by lucius-verus-fan 2021-09-06
#
# Produces hexSticker for lmForc package.
#-------------------------------------------------------------------------------

# install.packages("Rcpp")
# install.packages("rsvg")
# install.packages("hexSticker")

library(hexSticker)

sticker(
  subplot = "lmForc_logo.png", 
  dpi = 2000,
  package = "lmForc", 
  s_x = 1, 
  s_y = 0.87, 
  s_width = 0.95,
  s_height = 0.95,
  p_x = 1,
  p_y = 1.45,
  p_size = 9, 
  p_color = "#160f09",
  h_fill = "#faefdd",
  h_color = "#785130",
  white_around_sticker = TRUE,
  filename="lmForc_hexSticker.png")
