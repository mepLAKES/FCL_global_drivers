# Description ------------------------------
# Project : Checks for observed baselines: Drivers of food chain length across freshwater ecosystems- figures
# Script: Analyses of the relationships between FCL and various environmental descriptors-figures
# Date: 2026-03-11
# Version: 2.0
# Description: This script re-performs, for th dataset on observed baslines_ various analyses related:
# - to testing FCL relationships to Ecosystem size X Climate zones,
# - to testing the relationships underpinning the hypothesis of Ecosystem size X Climate zones on FCL
# #
# Purpose: explaining FCL across aquatic ecosystems by a combination of the Ecosystem size and Metabolic theory
# Author: Marie-Elodie Perga
# #

# data preps-------------

##### load datasets---------
getwd()
load ("../data/Env.RData")
load("../data/dataset_with_metrics_community_May25.RData")
load("../data/df_fish_length_weights.RData")
#####libraries---------

library(dplyr)
library(ggplot2)
library(forcats)
library(sjPlot)
library(ggpubr)
library(patchwork)


######## fonctions
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


#####choice of color palette---------
col_pal<-c("deepskyblue1","deepskyblue2","deepskyblue3","darkolivegreen","darkolivegreen2","darkolivegreen3",
           "coral","coral2","brown1","brown2","brown3","goldenrod1","goldenrod2","goldenrod3")

#####filtering dataset for extreme values or outliers ---------
df<- dataset_with_metrics_community_May25 %>% 
  left_join(Env,by="Food web_ID") %>% 
  filter(is.finite(FCL))%>% filter(
    CD<=5,
    FCL >=2,
    FCL<=8,
    NND<=2.5,
    TP>=0)

save(df,file="complete_metrics_env_May25.RData")
df<- df %>% filter( !is.na(size_class))

### Plot----
#### fixing colors for climate zones
fixed_colors=c("Cool and moist" = "darkolivegreen4",
               "Warm temperate" = "brown1",
               "Hot and moist" = "darkgoldenrod1",
               "Hot and dry" = "darkgoldenrod3",
               "Cold and wet/mesic"="deepskyblue1",
               "Cool temperate and dry/xeric"="darkolivegreen2")



#### a little tweak to reorder categories of climates in a logical order
df2<- df 
df2$Climate_zonee3<-factor(df$Climate_zone_e2,
                           levels = c("Cold and wet/mesic","Cool and moist",
                                      "Cool temperate and dry/xeric", 
                                      "Warm temperate",
                                      "Hot and moist","Hot and dry"),
                           ordered = TRUE)



### Ecosystem size XX climate zone
# FCL : testing the Size X Metabolic hypothesis ---------

## Model --------- 
mod_size_2<-lm(FCL ~   Climate_zone_e2 + Climate_zone_e2:size_z_scored,df2)

summary(mod_size_2)
anova(mod_size_2)
tab_model(mod_size_2)

r2<- round(summary(mod_size_2)$r.squared,digits = 3)
p<- round(anova(mod_size_2)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_size_2)

## Plot of the Model --------- 
### the linear model
G_size_2 <- ggplot(df2, aes(y = FCL, x = size_z_scored,
                            col = Climate_zonee3, fill = Climate_zonee3)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Ecosystem size (zscored & log)", y = "FCL") +
  scale_color_manual("Climate zone", values = fixed_colors) +
  scale_fill_manual("Climate zone", values = fixed_colors) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(b = -5),
    plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
  ) +
  annotate("text", x = 2, y = 6,
           label = expression(paste(r^2, "=", 0.059, ", p<", 10^-7)),
           color = "black", size = 4) +
  guides(color = guide_legend(ncol = 1))
G_size_2

### the slopes for insert


#### Extracting slopes values-------------  
X<-summary(mod_size_2)
slopes_mean<-as.vector(X$coefficients[7:12,1])
slopes_names<-c("Cold and wet/mesic","Cool and moist", "Cool temperate and dry/xeric",
                "Warm temperate",
                "Hot and moist",
                "Hot and dry")
slopes_std<-as.vector(X$coefficients[7:12,2])
slopes_p<-as.vector(X$coefficients[7:12,4])
df_slopes<-data.frame(slopes_names,slopes_mean,slopes_std,slopes_p)

#### Plot the slopes values------------- 
G_slope<- df_slopes %>%
  mutate(slopes_names = fct_relevel(slopes_names, 
                                    "Hot and dry","Hot and moist",
                                    "Warm temperate","Cool temperate and dry/xeric",
                                    "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=slopes_mean,x=slopes_names, 
             color=slopes_names )) +
  geom_point(show.legend = FALSE,size=1.5) +
  geom_errorbar(aes(ymin=slopes_mean-slopes_std, ymax=slopes_mean+slopes_std), width=.2,size=1.2,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Slopes",x="Climate zone") +
  theme_classic() +
  ylim(-0.2,0.5)+
  theme(axis.text.x = element_blank(),
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        plot.background = element_rect(fill="transparent",color=NA),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.8), colour = NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) +
  annotate("text", x = c(6,5,4),y = c(0.3,0.4,0.48), 
           label = "*", 
           color = "black", size = 6,fontface="bold")
G_slope

### insert the slopes

G_size_f<-G_size_2 +
  inset_element(
    G_slope,
    left = -0.15, bottom = 0.45,
    right = 0.55, top = 1.1
  )
G_size_f

#figure_test_path_1 <- paste0(figures.dir,'/Figure_test_1.jpeg')
#jpeg(figure_test_path_1,height=15,width=25,units="cm",res=300)
#G_size_f
#dev.off()

### combine the two
#G_size_f<-ggarrange(G_size_2, G_slope,
#                    ncol=2, nrow=1, labels=c("A","B"),
#                    label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
#                    widths=c(3,1))
#
#G_size_f
## energy limitation hypothesis ---------
### Model --------- 

#### linear 
mod_TP<-lm(FCL ~   TP,df2)
mod_TP_2<-lm(FCL ~   Climate_zonee3 + Climate_zonee3:TP,df2)
summary(mod_TP)
summary(mod_TP_2)
anova(mod_TP_2)
tab_model(mod_TP_2)

r2<- round(summary(mod_TP_2)$r.squared,digits = 3)
p<- round(anova(mod_TP_2)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_TP_2)



#### gam degree 3 to fit the hypothesis

library(mgcv)
mod_TP_gam<-gam(FCL ~ Climate_zonee3 +s(TP,by=Climate_zonee3,k=3),data=df2,method="REML")
m0<-gam(FCL ~1,data=df2,method="REML")
m1<-gam(FCL ~Climate_zonee3,data=df2,method="REML")
summary(mod_TP_gam)
anova(m0,mod_TP_gam,test="F")
anova(m1,mod_TP_gam,test="F")


#### a degree three gam model does not explain more than just a univariate model with only climate zone


### plot of the model ---------

G_TP_gam <- ggplot(df2, aes(y = FCL, x = TP,
                            col = Climate_zonee3, fill = Climate_zonee3)) +
  geom_point(alpha = 0.25) +
  geom_smooth(
    aes(group = Climate_zonee3),
    method = "gam",
    formula = y ~ s(x, k = 3),
    se = TRUE,alpha=0.5
  ) +
  labs(x = "Ecosystem size (zscored & log)", y = "FCL") +
  scale_color_manual("Climate zone", values = fixed_colors) +
  scale_fill_manual("Climate zone", values = fixed_colors) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(b = -5),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
  ) +
  annotate("text", x = 2, y = 6,
           label = expression(paste(r^2, "=", 0.059, ", p<", 10^-7)),
           color = "black", size = 4) +
  guides(color = guide_legend(ncol = 1))
G_TP_2











G_P <- ggplot(df2, aes(y = FCL, x = TP,
                       col = Climate_zonee3, fill = Climate_zonee3)) +
  #  geom_point(alpha = 0.5,colour="grey") +
  geom_point(alpha = 0.25) +
  #  geom_smooth(method = "lm", se = TRUE) +
  labs(x=expression(paste("P yield [kg ", ha^-1, yr^-1,"] (log-scale _unit)")),y="FCL")+
  scale_color_manual("Climate zone", values = fixed_colors) +
  scale_fill_manual("Climate zone", values = fixed_colors) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
    #   legend.box = "horizontal",
    #   legend.margin = margin(b = -5),
    #   plot.title = element_text(face = "bold")
  ) +
  annotate("text", x = 6, y = 6,
           label = expression(paste(r^2, "=", 0.002, ", p=", 0.06)),
           color = "black", size = 4) +
  guides(color = guide_legend(ncol = 3))
G_P




## Disturbance hypothesis ---------
### Model ------
mod_disturbance<-lm(FCL ~ hydro_dis_z_scored,df)
summary(mod_disturbance)
anova(mod_disturbance)
tab_model(mod_disturbance)

r2<- round(summary(mod_disturbance)$r.squared,digits = 3)
p<- round(anova(mod_disturbance)$`Pr(>F)`[1],digits=3)

par(mfrow=c(2,2))
plot(mod_disturbance)

### Plot ------
G_disturbance<-ggplot(df,aes(y=FCL,x=hydro_dis_z_scored))+
  geom_point(alpha=0.5,col="grey")+
  #    geom_point(alpha=0.15)+
  #   geom_smooth(method = "lm",se = TRUE, col="goldenrod2")+
  #    ggtitle("Hydrological disturbance hypothesis")+
  labs(x=expression(paste("Flow variability [", 10^-6, s^-1,"] (log-scale _z-scored))")),y="FCL")+
  theme_classic()+
  annotate("text", x = 2, y = 6, 
           label = expression(paste(r^2,"=0.029,", "p=0.09")), 
           color = "black", size = 4)+
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10))


G_disturbance


## body-size hypothesis


# How all of this may be linked to the species body size ? --------- 

### Merging the isotope-env dataset with the apex species body mass dataset 

df_3<- df %>% 
  left_join(df_fish_length_weights, by = c("Food web_ID" = "Food web_ID")) %>%
  filter(Lmax<=250) #one outlier close to 280

df_3$Climate_zonee3<-factor(df_3$Climate_zone_e2,
                            levels = c("Cold and wet/mesic","Cool and moist",
                                       "Cool temperate and dry/xeric", 
                                       "Warm temperate",
                                       "Hot and moist","Hot and dry"),
                            ordered = TRUE)

## Relationships of apex body size to the Ecosystem size across climate zones ------
### Model ---------
mod_Lmax<-lm(FCL ~ Lmax , data = df_3)
summary(mod_Lmax)
tab_model(mod_Lmax)


par(mfrow=c(2,2))
plot(mod_Lmax)

anova(lm(FCL ~ Lmax*Climate_zonee3, data = df_3)) # check for lack of interaction with climate zone

### Plot ---------
G_length_FCL_2<-ggplot(df_3,aes(y=FCL,x=Lmax))+
  geom_point(alpha=0.15,col="grey")+
  geom_smooth(method = "lm",se = TRUE,col="black")+
  labs(x="Length of the top fish predator species (cm)",y="FCL")+
  theme_classic()+
  annotate("text", x = 125, y = 6, 
           label = expression(paste(r^2,"=",0.02,", p<",10^-4)), 
           color = "black", size = 4)+
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10))

G_length_FCL_2

## for insert
G_length_size_2<-ggplot(df_3,aes(y=Lmax,x=size_z_scored,col=Climate_zonee3,fill=Climate_zonee3))+
  geom_point(alpha=0.15)+
  geom_smooth(method = "lm",se = TRUE)+
  labs(x="Ecosystem size (log and z_scored)",y="Length of the top fish predator species (cm)")+
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+theme_classic()+
  annotate("text", x = 2, y = 150, 
           label = expression(paste(r^2,"=",0.15,", p<",10^-16)), 
           color = "black", size = 4)+
  theme(       legend.position = "none",
               plot.title = element_text(face = "bold"),
               axis.title.x = element_text(size = 10),
               axis.title.y = element_text(size = 10),
               axis.text.x  = element_text(size = 10),
               axis.text.y  = element_text(size = 10))+
  guides(color = guide_legend(ncol = 1))
G_length_size_2


### Plot the intercepts ---------
#### Extracting the intercept values
anova(G_mod_fish_size_2)
T<-summary(G_mod_fish_size_2)



intercept_names<-c("Cool and moist",
                   "Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")
intercept_mean<-c(T$coefficients[1,1],
                  T$coefficients[1,1]+T$coefficients[2:6,1])

u<-c(0,(as.vector(T$coefficients[2:6,2]))^2)
v<-rep((T$coefficients[1,2])^2,6)
intercept_std<-sqrt(u+v)

df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std)

#### Plot the intercept values

G_intercept<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=intercept_mean/10,x=intercept_names, 
             color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(intercept_mean-intercept_std)/10, ymax=(intercept_mean+intercept_std)/10), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (cm)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) +
  annotate("text", x = 1:6 ,y = c(70,70,70,100,80,100), 
           label = c("a","a","a","b","c","b"), 
           color = "black", size = 3,fontface="italic")
G_intercept

##figure with insert
G_length_f<-G_length_size_2 +
  inset_element(
    G_intercept,
    left = -0.15, bottom = 0.4,
    right = 0.7, top = 1.1
  )
G_length_f

G_body_size<-ggarrange(G_length_FCL_2,G_length_f,
                       ncol=2, nrow=1, labels=c("B","C"),
                       label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                       widths=c(1.2,1.5),align="h")



### final figure

G_P<-ggarrange(G_P,labels=c("D"),
               label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"))

G_energy<-decorate_plot(G_P, path_icon_1,
                        border_col = "purple4",
                        top_text_face="bold",
                        top_text = "ENERGY LIMITATION",
                        panel_label = "",
                        icon_w = 0.2, icon_h = 0.2,label_color="black",
                        border_lwd = 2,pad=0.01,
                        top_text_x = 0.5)

G_energy

G_disturbance<-ggarrange(G_disturbance,labels=c("E"),
                         label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"))

G_dist<-decorate_plot(G_disturbance, path_icon_3,
                      border_col = "chartreuse4",
                      top_text_face="bold",
                      panel_label = "",
                      top_text = "DISTURBANCE",
                      icon_w = 0.2, icon_h = 0.2,label_color="black",
                      border_lwd = 2,pad=0.01,
                      top_text_x = 0.4)

G_dist

GDE<-ggarrange(G_energy,G_dist,align="h")

G_bodysize<-decorate_plot(G_body_size, path_icon_2,
                          border_col = "deepskyblue4",
                          top_text_face="bold",
                          panel_label = "",
                          top_text = "BODY-SIZE CONSTRAINTS",
                          icon_w = 0.15, icon_h = 0.2,label_color="black",
                          border_lwd = 2,pad=0.01,
                          top_text_x = 0.4)

G_bodysize



final_figure<-ggarrange(
  ggarrange(G_size_f,labels=c("A"),label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold")),
  G_bodysize,GDE,
  ncol=1, nrow=3,
  heights=c(1.8,1.8,1))


final_figure

ggsave("../figures/Figure_3.png", final_figure,
       width = 220, height =300, units = "mm",
       dpi = 300, bg = "white")



#### additional test for Richness effect
load(file = "../data/df_rich.RData")

colnames(df_rich)[1]<-"Food web_ID"

df_4<-df_3 %>% 
  left_join(df_rich, by="Food web_ID") %>%
  filter(richness<=40) # one outlier with 60 species, but it is a very small stream, so I think it is a mistake in the data and I remove it

df_4$Climate_zonee3<-factor(df_4$Climate_zone_e2,
                            levels = c("Cold and wet/mesic","Cool and moist",
                                       "Cool temperate and dry/xeric", 
                                       "Warm temperate",
                                       "Hot and moist","Hot and dry"),
                            ordered = TRUE)
##  Model of Richness versus Ecosystem size across climate zones-----

mod_richness<-lm(richness ~ Climate_zone_e2+Climate_zone_e2:size_z_scored, data = df_4)
summary(mod_richness)
anova(mod_richness)
tab_model(mod_richness)



##  Figure : Richness versus Ecosystem size across climate zones-----


### Richness vs size and cliamte
G_Richness_2<-ggplot(df_4,aes(size_z_scored, richness,col=Climate_zonee3,fill=Climate_zonee3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+
  labs(x = "Ecosystem Size (z-scored)", y = "Fish richness") +
  theme_classic() +theme(legend.position = "right",
                         legend.title = element_text(size = 10),
                         legend.text = element_text(size = 8),
                         axis.title.x = element_text(size = 10),
                         axis.title.y = element_text(size = 10),
                         axis.text.x = element_text(size = 8),
                         axis.text.y = element_text(size = 8))+
  ylim(0,40)

G_Richness_2




### Plot the intercepts ---------
#### Extracting the intercept values
T<-summary(mod_richness)



intercept_names<-c("Cool and moist",
                   "Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")

intercept_mean<-c(T$coefficients[1,1],
                  T$coefficients[1,1]+T$coefficients[2:6,1])

u<-c(0,(as.vector(T$coefficients[2:6,2]))^2)
v<-rep((T$coefficients[1,2])^2,6)
intercept_std<-sqrt(u+v)

df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std)

#### Plot the intercept values

G_intercept<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=intercept_mean/10,x=intercept_names, 
             color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(intercept_mean-intercept_std)/10, ymax=(intercept_mean+intercept_std)/10), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (Richness)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) #+
#  annotate("text", x = 1:6 ,y = c(70,70,70,100,80,100), 
#           label = c("a","a","a","b","c","b"), 
#          color = "black", size = 3,fontface="italic")
G_intercept

##figure with insert
G_Richness_2_f<-G_Richness_2 +
  inset_element(
    G_intercept,
    left = 0.0, bottom = 0.4,
    right = 0.7, top = 1.1
  )
G_Richness_2_f



# Richness and FCL
mod_richness2<-lm(FCL ~ Climate_zone_e2+Climate_zone_e2:richness, data = df_4)
summary(mod_richness2)
anova(mod_richness2)
tab_model(mod_richness2)

##  Figure : Richness versus Ecosystem size across climate zones-----


### Richness vs size and cliamte
G_Richness_3<-ggplot(df_4,aes(richness, FCL,col=Climate_zonee3,fill=Climate_zonee3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual("Climate zone",values = fixed_colors)+
  scale_fill_manual("Climate zone",values = fixed_colors)+
  labs(x = "Richness", y = "FCL") +
  theme_classic() +theme(legend.position = "right",
                         legend.title = element_text(size = 10),
                         legend.text = element_text(size = 8),
                         axis.title.x = element_text(size = 10),
                         axis.title.y = element_text(size = 10),
                         axis.text.x = element_text(size = 8),
                         axis.text.y = element_text(size = 8))

G_Richness_3

### Plot the intercepts ---------
#### Extracting the intercept values
T<-summary(mod_richness2)



intercept_names<-c("Cool and moist",
                   "Warm temperate",
                   "Hot and moist",
                   "Hot and dry",
                   "Cold and wet/mesic",
                   "Cool temperate and dry/xeric")

intercept_mean<-c(T$coefficients[1,1],
                  T$coefficients[1,1]+T$coefficients[2:6,1])

u<-c(0,(as.vector(T$coefficients[2:6,2]))^2)
v<-rep((T$coefficients[1,2])^2,6)
intercept_std<-sqrt(u+v)

df_intercept<-data.frame(intercept_names,intercept_mean,intercept_std)


## plot the slopes values
slopes_names<-c("Cool and moist",
                "Warm temperate",
                "Hot and moist",
                "Hot and dry",
                "Cold and wet/mesic",
                "Cool temperate and dry/xeric")

slope_mean<-as.vector(T$coefficients[7:12,1])
slope_std<-as.vector(T$coefficients[7:12,2])

df_slopes<-data.frame(slopes_names,slope_mean,slope_std)

#### Plot the intercept values

G_intercept<- df_intercept %>%     
  mutate(intercept_names = fct_relevel(intercept_names,  "Hot and dry","Hot and moist",
                                       "Warm temperate","Cool temperate and dry/xeric",
                                       "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=intercept_mean,x=intercept_names, 
             color=intercept_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(intercept_mean-intercept_std), ymax=(intercept_mean+intercept_std)), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Intercept (FCL)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) #+
#  annotate("text", x = 1:6 ,y = c(70,70,70,100,80,100), 
#           label = c("a","a","a","b","c","b"), 
#          color = "black", size = 3,fontface="italic")
G_intercept



G_slopes<- df_slopes %>%     
  mutate(slopes_names = fct_relevel(slopes_names,  "Hot and dry","Hot and moist",
                                    "Warm temperate","Cool temperate and dry/xeric",
                                    "Cool and moist","Cold and wet/mesic")) %>%
  ggplot(aes(y=slope_mean,x=slopes_names, 
             color=slopes_names )) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin=(slope_mean-slope_std), ymax=(slope_mean+slope_std)), width=.2,size=1,show.legend = FALSE)+
  scale_color_manual(values = fixed_colors) +
  labs(y="Slopes (FCL)",x="Climate zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 2, unit = "cm"),
        panel.background = element_rect(fill = rgb(1, 1, 1, alpha = 0.7), colour = NA),
        plot.background = element_rect(fill="transparent",color=NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        #       axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 8)) +
  annotate("text", x = c(1,3,4,6) ,y = c(0.06,0.06,0.12,0.12), 
           label = c("*","*","*","*"), 
           color = "black", size = 5,fontface="italic")
G_slopes



##figure with insert
G_Richness_3_f<-G_Richness_3 +
  inset_element(
    G_slopes,
    left = 0.35, bottom = 0.5,
    right = 1, top = 1.1
  )
G_Richness_3_f


G_richness<-ggarrange(G_Richness_2_f,G_Richness_3_f,
                      ncol=1, nrow=2, labels=c("A","B"),
                      label.x=0.95,label.y=0.95, font.label = list(size = 12, face = "bold"),
                      widths=c(1.5,1.5),align="h",common.legend = TRUE)

ggsave("../figures/Figure_richness.png", G_richness,
       width = 200, height =200, units = "mm",
       dpi = 300, bg = "white")

