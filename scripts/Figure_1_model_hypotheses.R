######################################################
# Project : Drivers of food chain length across freshwater ecosystems
# Script: Figure for hypotheses : models outputs
# Date: 2025-10-21
# Version: 1.0
# Purpose: 
# Author: Marie-Elodie Perga, marie-elodie.perga@unil.ch
######################################################

######################################################
# 1. SET UP -------
######################################################
## libraries---------

library(dplyr)
library(ggplot2)
library(ggpubr)
library(rprojroot)
library(patchwork)
library(cowplot)
library(grid)
library(showtext)
library(sysfonts)

##  Set the working directory to the root of the project ------
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'/scripts')
figures.dir = paste0(root.dir,'/figures')
model.dir = paste0(root.dir,'/data/models')

setwd(script.dir)

### choice of color palette---------
col_pal<-c("darkgrey","deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")
col_type<-c("coral","deepskyblue3")
col_type<-c("brown3","goldenrod2")


### data import and load
model_size_temp_path <- paste0(model.dir,'/result_BSmax_Temp.txt')
model_disturbance_path <- paste0(model.dir,'/result_Disturb.txt')
model_productivity_temp_path <- paste0(model.dir,'/result_Nut_Temp.txt')
model_S_temp_path <- paste0(model.dir,'/result_S_Temp.txt')

model_size_temp<-read.table(model_size_temp_path , header = TRUE)
model_disturbance<-read.table(model_disturbance_path  , header = TRUE)
model_productivity_temp<-read.table(model_productivity_temp_path , header = TRUE)
model_richness_temp<-read.table(model_S_temp_path , header = TRUE)

##########################
# 2. FIGURE FOR HYPOTHESES -------
##########################
 ## Generate individual figures

### Figure for hypothesis A : effect of Productivity and temperature on FCL -----
model_productivity_temp<- model_productivity_temp %>%
  filter(!is.na(K))%>%
  mutate(Temp=factor(Temp,levels=c(15,20,25)))%>%
  mutate(Temp_code=recode (Temp, "15" = "Cold", "20"="Temperate","25" = "Warm"))#%>%))
#  mutate(K=factor(K, levels=c(0.1,0.25,0.5,1)))


p_prod_temp<-ggplot(model_productivity_temp, aes(x = K, y = TLmax, colour = Temp_code,fill = Temp_code)) +
  geom_smooth(
    aes(group = Temp_code),
    method = "gam",
    formula = y ~ s(x, k = 3),
    se = TRUE,alpha=0.5
  ) +
  labs(x = "Relative resource availability",
       y = "Food chain length (FCL)",
       colour = NULL, fill = NULL) +
  theme_classic(base_family = "Avenir") +
  scale_fill_manual(values=c("deepskyblue","darkolivegreen2","darkgoldenrod2"), name = "") +
  scale_colour_manual(values=c("deepskyblue","darkolivegreen2","darkgoldenrod2"), name = "") +
  theme(legend.position = c(0.8,0.2),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        size=0.5))+
  guides(colour = guide_legend(title = NULL),
         fill   = guide_legend(title = NULL))

p_prod_temp

figure_hypotheses_path <- paste0(figures.dir,'/fig_hyp_a.png')

ggsave(figure_hypotheses_path, p_prod_temp, width = 7, height = 7, units = "cm")




### Figure for hypothesis B : effect of size and temperature on FCL -----
model_size_temp_2<- model_size_temp %>%
  mutate(Temp=factor(Temp,levels=c(15,20,25)))%>%
  mutate(Temp_code=recode (Temp, "15" = "Cold", "20"="Temperate","25" = "Warm"))#%>%))

#### Predator size versus FCL
p_apex<-ggplot(model_size_temp_2, aes(x=log(maxBM), y=TLmax)) +
  geom_smooth(method = "lm",alpha=0.5,col="grey")+
  labs(x="Apex predator max length (log-scaled)", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  ylim(2,6)+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="white", 
                                        size=0.5))
p_apex


#### Insert: Predator size with temperature
p_apex_hist <- ggplot(
  model_size_temp_3,
  aes(TLmax, #fill = Temp_code,
      col = Temp_code)
) +
  geom_density(alpha = 0.5,size=0.8
               ) +
  labs(x = "Apex predator max length", y = "Density") +
  theme_classic(base_family = "Avenir") +
#  scale_fill_manual(
 #   values = c("deepskyblue", "darkolivegreen2", "darkgoldenrod2"),
 #   name = ""               # ✅ single legend title
 # ) +
  scale_color_manual(
    values = c("deepskyblue", "darkolivegreen2", "darkgoldenrod2"),
    name = ""               # ✅ single legend title
  ) +
  theme (
#    legend.position =  c(0.9,0.95),
    legend.position="none",
#   legend.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    panel.background = element_rect(fill = "transparent", colour = NA),  
    plot.background  = element_rect(fill = "transparent", colour = NA),  
#    legend.background = element_rect(fill = "transparent", colour = NA),
#    legend.key = element_rect(fill = "transparent", colour = NA),
 #   legend.key.size = unit(0.2, "cm")
)
  

#### Combining the two

p_apex_f<-p_apex +
  inset_element(
    p_apex_hist,
    left = 0.01, bottom = 0.40,
    right = 0.55, top = 0.99
  )

p_apex_f

#### saving the figure in ggsave
figure_hypotheses_path <- paste0(figures.dir,'/fig_hyp_b.png')
ggsave(figure_hypotheses_path, p_apex_f, width = 7, height = 7, units = "cm")


model_productivity_temp<- model_productivity_temp %>%
  filter(!is.na(K))%>%
  mutate(Temp=factor(Temp,levels=c(15,20,25)))%>%
  mutate(Temp_code=recode (Temp, "15" = "Cold", "20"="Temperate","25" = "Warm"))#%>%))
#  mutate(K=factor(K, levels=c(0.1,0.25,0.5,1)))




### Figure for hypothesis C : effect of disturbance on FCL -----
summary(model_disturbance) 
model_disturbance<- model_disturbance %>%
  filter(!is.na(omega))%>%
  filter(!is.na(Z))%>%
  mutate(omega=factor(omega, levels=c(0.1,0.5,1)))



p_dist<-ggplot(model_disturbance, aes(x=Z, y=TLmax) )+
  geom_smooth(method = "lm",color="darkgrey",fill="darkgrey",alpha=0.5)+
  labs(x="Disturbance severity", y="Food chain length (FCL)")+
  theme_classic(base_family ="Avenir")+
  theme(legend.position = "none",
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        plot.background = element_rect(fill = "white"))
p_dist

figure_hypotheses_path <- paste0(figures.dir,'/fig_hyp_c.png')

ggsave(figure_hypotheses_path, p_dist, width = 7, height = 7, units = "cm")

###################
## 3. COMBINE ALL
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

    
path_icon_1<-'../figures/energy.png'
path_icon_2<-'../figures/bodysize.png'
path_icon_3<-'../figures/dist.png'

pA <- decorate_plot(p_prod_temp, path_icon_1,
                    border_col = "purple4",
                    top_text_face="bold",
                    top_text = "ENERGY LIMITATION",
                    panel_label = "A",
                    icon_w = 0.2, icon_h = 0.2,label_color="black",
                    border_lwd = 2,pad=0.01,
                    top_text_x = 0.5)

pA

pB <- decorate_plot(p_apex_f, path_icon_2,
                    border_col = "deepskyblue4",
                    top_text_face="bold",
                    panel_label = "B",
                    top_text = "BODY-SIZE CONSTRAINTS",
                    icon_w = 0.2, icon_h = 0.2,label_color="black",
                    border_lwd = 2,pad=0.01,
                    top_text_x = 0.5)

pB
pC <- decorate_plot(p_dist,  path_icon_3,
                    border_col = "chartreuse4",
                    top_text_face="bold",
                    panel_label = "C",
                    top_text = "DISTURBANCE",
                    icon_w = 0.15, icon_h = 0.15,label_color="black",
                    border_lwd = 2,pad=0.01,
                    top_text_x = 0.4)


pC






# Combine into one figure (vertical stack)
final_fig <- cowplot::plot_grid(pA, plot_spacer(),pB, plot_spacer(), pC, ncol = 5, 
                       rel_widths=c(1,0.05,1,0.05,1),
                       align = "v", axis = "lr")


final_fig 
# -----------------------------
# High-resolution export
# -----------------------------


# High-res PNG (excellent raster, crisp text)
ggsave("../figures/Figure_1.png", final_fig,
       width = 280, height =90, units = "mm",
       dpi = 600, bg = "white")


































