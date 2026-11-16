# Install once: install.packages(c('ggplot2','dplyr','cowplot','magick'))
library(ggplot2)
library(dplyr)
library(cowplot)
library(magick)
# Run from the folder containing the original CSV and screenshot.
d <- read.csv('Chicago Rat Hole.csv', check.names = FALSE, fileEncoding = 'UTF-8-BOM') |>
  transmute(Species = ifelse(Species == 'Scurius niger', 'Sciurus niger', Species),
            body = `Snout-to-tail base length (mm)`,
            forelimb = `Forelimb length (mm)`) |>
  filter(!is.na(Species), Species != '', is.finite(body), is.finite(forelimb))
colors <- c('Chicago Rat Hole'='#ed2028', 'Mus musculus'='#b7a497',
 'Peromyscus leucopus'='#db83b0', 'Sciurus carolinensis'='#eebc67',
 'Tamias striatus'='#85bcd4', 'Glaucomys volans'='#ef979b',
 'Ondatra zibethicus'='#829bb7', 'Rattus norvegicus'='#e5acd0',
 'Sciurus niger'='#9cc995')
rat <- filter(d, Species == 'Chicago Rat Hole')
other <- filter(d, Species != 'Chicago Rat Hole')
hull <- rat[chull(rat$body, rat$forelimb), ]
rat_mean <- summarise(rat, body = mean(body), forelimb = mean(forelimb))
p <- ggplot() +
  geom_point(data = other, aes(body, forelimb, fill = Species),
             shape = 21, color = 'grey35', stroke = .25, size = 2.6, alpha = .8) +
  geom_polygon(data = hull, aes(body, forelimb), fill = '#ed2028',
               color = '#ed2028', alpha = .13, linewidth = .85) +
  geom_point(data = rat, aes(body, forelimb), shape = 21, fill = '#ed2028',
             color = 'white', stroke = .25, size = 2.5, alpha = .8) +
  geom_point(data = rat_mean, aes(body, forelimb), shape = 21, fill = '#ed2028',
             color = 'black', size = 5.8, stroke = 1.1) +
  scale_fill_manual(values = colors) +
  coord_cartesian(xlim = c(45,450), ylim = c(10,102)) +
  labs(title = 'Chicago Rat Hole: morphometric comparison',
       subtitle = 'Eight rodent species and 75 imprint measurements',
       x = 'Snout-to-tail base length (mm)', y = 'Forelimb length (mm)') +
  theme_minimal(base_size = 13) +
  theme(legend.position = 'none', panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold'))
# Crop individual silhouettes from the provided reference screenshot.
ref <- image_read('Screenshot 2026-11-16 161822.png')
# Each row: left, top, right, bottom in original image pixels.
boxes <- rbind(c(5,8,42,47), c(7,47,42,68), c(8,72,42,96),
 c(8,98,42,124), c(8,125,43,151), c(194,10,228,46),
 c(193,46,228,69), c(181,72,229,96), c(195,99,229,125))
legend <- ggdraw()
for (i in seq_along(colors)) {
  b <- boxes[i, ]; icon <- image_crop(ref, geometry_area(b[3]-b[1], b[4]-b[2], b[1], b[2]), repage = TRUE)
  xpos <- if (i <= 5) .055 else .53
  ypos <- .9 - ((i-1) %% 5)*.18
  legend <- legend + draw_image(icon, x = xpos, y = ypos-.075,
      width = .075, height = .15) +
    draw_label(names(colors)[i], x = xpos+.09, y = ypos, hjust = 0,
               fontface = if (i == 1) 'plain' else 'italic', size = 11)
}
key <- ggdraw() + draw_label('Red outline / shading: convex hull     |     Large red point: Rat Hole mean', size = 11)
final <- plot_grid(p, legend, key, ncol = 1, rel_heights = c(1,.32,.065))
print(final)
ggsave('chicago_rat_hole_ggplot.png', final, width = 10, height = 8.5, dpi = 300, bg = 'white')
ggsave('chicago_rat_hole_ggplot.pdf', final, width = 10, height = 8.5, bg = 'white')
