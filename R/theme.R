##----------------------------------------------------------------------------------------------------------##
##  TEMPLATE: Cedric Scherer (cedricphilippscherer@gmail.com)                                               ##
##  Adapted by Anne Lewerentz (anne.lewerentz@uni-wuerzburg.de)                                             ##
##  Function for a custom ggplot theme                                                                      ##
##  2017-05-27                                                                                              ##
##----------------------------------------------------------------------------------------------------------##



if ("extrafont" %in% rownames(installed.packages()))
{
  library(extrafont)
  #extrafont::font_import()
  #extrafont::loadfonts(device = "win")
  base <- "Arial"
} else {
  #base <- "Gadugi"
  base <- "Arial"
}



library(ggplot2)

theme_custom <- function (base_size = 10, base_family = base)
{
  half_line <- base_size/2
  theme(line = element_line(colour = "grey10", size = 0.4, linetype = 1, lineend = "butt"),
        text = element_text(family = base_family, face = "plain", colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                            debug = FALSE),
        axis.line = element_blank(),
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.text = element_text(size = base_size * 1.1, colour = "grey10"),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
        axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
        axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0),
        axis.ticks = element_line(colour = "grey10", size = 0.3),
        axis.ticks.length = unit(half_line/2, "pt"),
        axis.title.x = element_text(margin = unit(c(3.5, 0, 0, 0), "mm"), vjust = 1, size = base_size * 1.3, face = "bold"),
        axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
        axis.title.y = element_text(angle = 90, margin = unit(c(0, 3.5, 0, 0), "mm"), vjust = 1, size = base_size * 1.3, face = "bold"),
        axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),
        legend.background = element_rect(colour = NA),
        legend.spacing = unit(0.4, "cm"),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.key = element_rect(fill = "grey98", colour = "grey98"),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.9)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0, size = rel(1)),
        legend.title.align = NULL,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "center",
        legend.box = NULL,
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.background = element_blank(),
        legend.box.spacing = unit(0.4, "cm"),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_rect(colour = "grey10", fill = NA, size = rel(1)),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.spacing = unit(base_size/2, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        strip.background = element_rect(fill = "grey98", colour = "grey10"),
        strip.text = element_text(colour = "black", size = base_size * 1.1, face = "bold"),
        strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
        strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
        strip.placement = "inside",
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_rect(colour = NA),
        plot.title = element_text(size = base_size * 1.3, hjust = 0, vjust = 1, face = "bold", margin = margin(b = half_line * 1.2)),
        plot.subtitle = element_text(size = base_size, hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)),
        plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9), color = "black"),
        plot.margin = margin(base_size, base_size, base_size, base_size), complete = T,
        plot.tag = element_text(size = rel(1.5), face = "bold", hjust = 0.5, vjust = 0.5),
        plot.tag.position = "topleft")
}

theme_set(theme_custom())
