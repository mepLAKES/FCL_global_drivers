decorate_plot <- function(p,
                          icon_path,
                          # Border
                          border_col = "#2C7FB8",
                          border_lwd = 5,
                          inset = 0.012,        # prevents clipping of thick strokes at edges
                          # Icon placement/size
                          icon_w = 0.08,
                          icon_h = 0.08,
                          pad = 0.01,           # distance from top/left border (keep >= 0)
                          gap_extra = 0.02,     # extra gap around icon where border is interrupted
                          icon_y_nudge = 0.00,  # + moves icon up; - moves it down
                          # Panel label (A/B/C)
                          panel_label = "A",
                          label_size = 10,
                          label_family = "Avenir",
                          label_face = "bold",
                          label_color = NULL,   # NULL -> uses border_col
                          # Top text (title) above the border
                          top_text = NULL,
                          top_text_x = 0.30,    # relative position across panel (0..1)
                          top_text_size = 10,
                          top_text_family = "Arial",
                          top_text_face = "bold",
                          top_text_color = NULL,# NULL -> uses border_col
                          top_text_bg = "white",# NA -> no background box
                          top_box_w = 0.28,     # width of background box (npc units)
                          top_box_h = 0.06,     # height of background box (npc units)
                          # Space above border for top text
                          headroom = 0.10,      # how much extra canvas above the border (0.05–0.15)
                          top_text_gap = 0.05,  # distance ABOVE the border for the top text
                          # Plot inner padding
                          plot_margin = ggplot2::margin(12, 12, 12, 12, unit = "pt")
) {
  
  # Dependencies needed in your script:
  # library(ggplot2)
  # library(cowplot)
  # library(grid)
  
  # Ensure plot content doesn't crash into the border
  p <- p + ggplot2::theme(plot.margin = plot_margin)
  
  # Defaults for colors
  if (is.null(top_text_color)) top_text_color <- border_col
  if (is.null(label_color))    label_color    <- top_text_color
  
  # Create taller canvas so we can draw above the top border without clipping
  canvas_top <- 1 + headroom
  
  # Border is drawn inset to avoid clipping of thick strokes
  x0 <- 0 + inset
  x1 <- 1 - inset
  y0 <- 0 + inset
  y1 <- 1 - inset   # top border y position (inside the plot region)
  
  # Icon placement (top-left corner, tight)
  icon_x <- x0 + pad
  icon_y <- y1 + pad - icon_h/2
  
  # Where to break the border near icon:
  # Top border starts after the icon (plus some gap)
  x_break <- icon_x + icon_w + gap_extra
  # Left border stops below the icon (minus some gap)
  y_break <- icon_y - gap_extra
  
  # Top text position: above top border, within canvas limits
  top_text_y <- min(y1 + top_text_gap, canvas_top - inset)
  
  # Panel label aligned with top text line (same y)
  label_x <- x1
  label_y <- top_text_y
  
  # Plot region placed inside inset border box
  plot_x <- x0
  plot_y <- y0
  plot_w <- x1 - x0
  plot_h <- y1 - y0
  
  # Build panel
  g <- cowplot::ggdraw(xlim = c(0, 1), ylim = c(0, canvas_top)) +
    # Base plot
    cowplot::draw_plot(p, x = plot_x, y = plot_y, width = plot_w, height = plot_h) +
    
    # ---- Border (rounded ends) ----
  # Bottom (full)
  cowplot::draw_line(x = c(x0, x1), y = c(y0, y0),
                     color = border_col, size = border_lwd, lineend = "round") +
    # Right (full)
    cowplot::draw_line(x = c(x1, x1), y = c(y0, y1),
                       color = border_col, size = border_lwd, lineend = "round") +
    # Top (broken near icon)
    cowplot::draw_line(x = c(x_break, x1), y = c(y1, y1),
                       color = border_col, size = border_lwd, lineend = "round") +
    # Left (broken near icon)
    cowplot::draw_line(x = c(x0, x0), y = c(y0, y_break),
                       color = border_col, size = border_lwd, lineend = "round") +
    
    # ---- Icon ----
  cowplot::draw_image(icon_path,
                      x = icon_x, y = icon_y,
                      width = icon_w, height = icon_h)
  
  # ---- Top text above border ----
  if (!is.null(top_text)) {
    top_text <- toupper(top_text)
    
    # Background box behind top text (optional)
    if (!is.na(top_text_bg)) {
      g <- g + cowplot::draw_grob(
        grid::rectGrob(
          x = top_text_x, y = top_text_y,
          width = top_box_w, height = top_box_h,
          gp = grid::gpar(col = NA, fill = top_text_bg)
        )
      )
    }
    
    g <- g + cowplot::draw_text(top_text,
                                x = top_text_x, y = top_text_y,
                                fontfamily = top_text_family,
                                fontface = top_text_face,
                                size = top_text_size,
                                color = top_text_color,
                                hjust = 0.5, vjust = 0.5)
  }
  
  # ---- Panel label aligned with top text ----
  # (Put after top text so it draws on top if boxes overlap)
  # Optionally add a small white box behind label too, for consistency:
  # Uncomment if desired
  # if (!is.na(top_text_bg)) {
  #   g <- g + cowplot::draw_grob(
  #     grid::rectGrob(
  #       x = label_x - 0.02, y = label_y,
  #       width = 0.06, height = top_box_h,
  #       gp = grid::gpar(col = NA, fill = top_text_bg)
  #     )
  #   )
  # }
  
  g <- g + cowplot::draw_text(panel_label,
                              x = label_x, y = label_y,
                              hjust = 1, vjust = 0.5,
                              fontfamily = label_family,
                              fontface = label_face,
                              size = label_size,
                              color = label_color)
  
  g
}
