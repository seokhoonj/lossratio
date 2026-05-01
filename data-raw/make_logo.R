# Generate the lossratio hex sticker.
#
# Run from the package root:
#   source("data-raw/make_logo.R")
#
# Output: man/figures/logo.png

library(hexSticker)
library(ggplot2)


# Subplot: development triangle grid + bottom-right wordmark ----------------
# The grid occupies the upper-left half of an imaginary n x n square.
# "lossratio" is placed at the bottom-right corner of that imaginary
# square, right-aligned so its trailing edge meets the square's edge.

n <- 5
grid <- expand.grid(coh = seq_len(n), elp = seq_len(n))
grid <- subset(grid, coh + elp - 1L <= n)
grid$fill_val <- grid$elp / n

sub <- ggplot() +
  geom_tile(
    data      = grid,
    aes(x = elp, y = -coh, fill = fill_val),
    colour    = "#0D1B3D",
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x        = n + 0.5,
    y        = -n - 0.45,
    label    = "lossratio",
    colour   = "#F4C24A",
    size     = 26,
    fontface = "bold",
    hjust    = 1,
    vjust    = 0
  ) +
  scale_fill_gradient(low = "#F8D77A", high = "#E8A23B") +
  coord_equal(clip = "off") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin     = margin(0, 0, 0, 0))


# Hex sticker ---------------------------------------------------------------

sticker(
  subplot   = sub,
  s_x       = 1,
  s_y       = 1.00,
  s_width   = 1.18,
  s_height  = 1.18,
  package   = "",
  p_size    = 0,
  h_fill    = "#0D1B3D",
  h_color   = "#F4C24A",
  h_size    = 1.2,
  filename  = "man/figures/logo.png",
  dpi       = 600
)
