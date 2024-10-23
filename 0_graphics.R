
library(pacman)
p_load(hms,
       patchwork,
       rio,
       sf,
       # terra,
       # geodata,
       tidyverse,
       biscale)


#=SET UP=======================================================================

#import cleaned data
aa_data <- import("data/aa_data_clean_2023.rds")


#=FUNCTIONS====================================================================

#univariate plot function

univar_plot <- function(data, var_name, var_label, hgrid=TRUE, val_labels=TRUE, legend=TRUE, y_pct=TRUE){
  
  require(ggplot2)
  require(dplyr)
  
  plot_data <- data |> 
    drop_na({{ var_name }}) |> 
    count({{ var_name }}, sort = FALSE)
  
  # Dynamically determine breaks based on range of n
  max_n <- max(plot_data$n, na.rm = TRUE)
  min_n <- min(plot_data$n, na.rm = TRUE)
  
  #get sum
  sum_n <- sum(plot_data$n, na.rm = TRUE)
  
  if(y_pct){
    plot <- plot_data |> 
      ggplot(aes(x={{ var_name }}, y=n/sum_n)) +
      scale_y_continuous(labels = scales::label_percent(),
                         limits = c(0, .7))
  } else {
    plot <- plot_data |> 
      ggplot(aes(x={{ var_name }}, y=n))
  }
  
  if(hgrid){
    plot <- plot +
      theme_minimal_hgrid() 
  } else {
    plot <- plot +
      theme_minimal_vgrid() 
  }
  
  plot <- plot +
    geom_col(aes(fill=n)) +
    theme(legend.position = "bottom",
          legend.text = element_text(face = "bold", size = 12),
          legend.ticks = element_blank(),
          legend.direction = "horizontal",
          legend.key.width = unit(3, "line"),
          axis.title.y = element_text(size = 12, face = "bold", angle = 0, vjust = .5),
          plot.title = element_text(size = 18, face = "bold", hjust = 0),
          plot.margin = margin(2,.5,.5,.5, unit = "line"),
          axis.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
    ) +
    labs(title=var_label,
         x=element_blank(),
         y="Active\nAttack\nCount") +
    scale_fill_viridis_c(name = NULL,
                         breaks = c(min_n, max_n),   # Set breaks based on range of n
                         labels = c("Fewer\nAttacks", "More\nAttacks")) +
    coord_cartesian(expand = F, clip = "off")
  
  
  if(val_labels){
    plot <- plot +
      geom_text(aes(label=n), vjust=0, fontface="bold")
  }
  
  if(!legend) {   # Simplified condition for hiding legend
    plot <- plot +
      guides(fill="none")
  }
  
  
  return(plot)
}


# #3d map labeling function
# 
# label_state_attacks <- function(data=event_map_data,
#                                 height_mat=height_mat,
#                                 state_name,
#                                 x,
#                                 y,
#                                 z,
#                                 clear_prev=TRUE){ 
#   
#   n_attacks <- data |> 
#     filter(state == state_name) |> 
#     pull(n)
#   lab <- glue("{state_name}: {n_attacks} Attacks") # Update the label to use state_name
#   
#   if(clear_prev){
#     render_label(heightmap = height_mat,
#                  x=x,
#                  y=y,
#                  z=z,
#                  text = lab, 
#                  textcolor = "red",
#                  linecolor = "red", 
#                  dashed = TRUE, 
#                  dashlength = 15,
#                  clear_previous = TRUE)
#   } else {
#     render_label(heightmap = height_mat,
#                  x=x,
#                  y=y,
#                  z=z,
#                  text = lab, 
#                  textcolor = "red",
#                  linecolor = "red", 
#                  dashed = TRUE, 
#                  dashlength = 15,
#                  clear_previous = FALSE)
#   }
# }

#=UNIVARIATE PLOTS=============================================================

#year
p_yr <- univar_plot(data = aa_data, var_name = year, var_label = "Year",
                    hgrid = TRUE, val_labels = F, legend = T, y_pct = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,70)) +
  geom_text(aes(label = n, colour = ifelse(n>35, "black", "white")), show.legend = F, vjust=1.1, fontface="bold") +
  scale_color_identity()
p_yr

#hour of day
p_hr <-  univar_plot(data=aa_data, var_name = hour_24hr, var_label = "Hour of Day",
                     hgrid = TRUE, val_labels = F, legend = TRUE, y_pct = FALSE) +
  scale_x_time(breaks = scales::date_breaks("1 hour"),
               labels = scales::time_format(format = "%H")) +
  scale_y_continuous(limits = c(0,70)) +
  geom_vline(xintercept = as_hms("12:00:00"), linetype="dashed", color="black") +
  annotate("label", x=as_hms("12:00:00"), y=50, label="Midday", size=5, label.size=NA, fontface="bold") +
  geom_text(aes(label = n, colour = ifelse(n>30, "black", "white")), show.legend = F, vjust=1.1, fontface="bold")  +
  scale_color_identity()

p_hr

#location type
p_loc <- univar_plot(data = aa_data, var_name = location_primary, var_label = "Location Type",
                     hgrid = F, val_labels = F, legend = T, y_pct = TRUE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip(expand = F) +
  geom_text(aes(label = n, colour = ifelse(n>170, "black", "white")), show.legend = F, hjust=1.1, fontface="bold")  +
  scale_color_identity()

#weapons
p_weapon <- univar_plot(data = aa_data, var_name = weapon_primary, var_label = "Weapon Type",
                        hgrid = F, val_labels = F, legend = T, y_pct = TRUE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip(expand = F) +
  geom_text(aes(label = n, colour = ifelse(n>170, "black", "white")), show.legend = F, hjust=1.1, fontface="bold")  +
  scale_color_identity()


#relationship
p_rel <- univar_plot(data = aa_data, var_name = relationship, var_label = "Relationship to Location",
                     hgrid = F, val_labels = F, legend = T, y_pct = TRUE) +
  theme(axis.title.x = element_text(size = 14, face = "bold")) +
  labs(y="Percentage of\nAll Active Attacks") +
  coord_flip(expand = F) +
  geom_text(aes(label = n, colour = ifelse(n>150, "black", "white")), show.legend = F, hjust=1.1, fontface="bold")  +
  scale_color_identity()

#save individual plots
ggsave("www/univar_yr.png", p_yr, height=7, width=10, bg="white", dpi=300)
ggsave("www/univar_hr.png", p_hr, height=7, width=10, bg="white", dpi=300)
ggsave("www/univar_loc.png", p_loc, height=7, width=10, bg="white", dpi=300)
ggsave("www/univar_weapon.png", p_weapon, height=7, width=10, bg="white", dpi=300)
ggsave("www/univar_rel.png", p_rel, height=7, width=10, bg="white", dpi=300)



#set design layout
design = "
AAAACC
AAAACC
AAAADD
AAAADD
BBBBEE
BBBBEE
BBBB##
BBBBFF
"

#combine plots
p_all <- p_yr + p_hr + p_loc + p_weapon + p_rel + guide_area() +
  plot_layout(design = design, guides = "collect")
p_all

#save
ggsave("www/univar_plots_all.png", p_all, height = 10, width = 14, bg="white", dpi=300)




#=BIVARIATE PLOTS==============================================================

#=====Events_killed pyramid====================================================
event_killed_data <- aa_data |> 
  drop_na(year) |> 
  group_by(year) |> 
  summarise(attacks=n(),
            killed_tot = sum(kill_tot)) |> 
  pivot_longer(-year,
               names_to = "vars",
               values_to = "vals")

#attacks
p_yr_event <- event_killed_data |> 
  filter(vars=="attacks") |> 
  ggplot(aes(y=vals, x=year)) +
  geom_bar(stat = "identity", fill="grey69") +
  geom_text(aes(label=vals), color="white",
            fontface="bold", hjust=0) +
  coord_flip(expand = F, clip = "off") +
  guides(fill="none") +
  theme_minimal_vgrid() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(2,.5,0,0, unit = "line")) +
  labs(x=element_blank(),
       y=element_blank()) +
  scale_x_reverse(breaks = seq(2000, 2023, 1)) +
  scale_y_reverse(limits = c(150, 0)) +
  annotate("text", x=1998, y=0, label="Number of Events", hjust=1, fontface="bold", color="grey69", size=5)
p_yr_event

#killed
p_yr_killed <- event_killed_data |> 
  filter(vars!="attacks") |> 
  ggplot(aes(y=vals, x=year)) +
  geom_bar(stat = "identity", fill="red4") +
  geom_text(aes(label=vals), color="white", hjust=1,
            fontface="bold") +
  coord_flip(expand = F, clip = "off") +
  guides(fill="none") +
  theme_minimal_vgrid() +
  theme(axis.text.y = element_text(size = 12, face = "bold", margin = margin(0,1,0,0, "line")),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(2,.5,0,0, unit = "line")) +
  labs(x=element_blank(),
       y=element_blank()) +
  scale_x_reverse(breaks = seq(2000, 2023, 1), expand = expansion(add=2)) +
  scale_y_continuous(limits = c(0, 150)) +
  annotate("text", x=1998, y=0, label="Number of Fatalities", hjust=0, fontface="bold", color="red4", size=5) 

#combine sides of pyramid
p_pyramid <- p_yr_event+p_yr_killed


ggsave("www/bivar_eventkilled.png", p_pyramid, height = 7, width = 10, bg="white")

# location x relationship
# location x time

# year x total killed
# year x attacks




#=GEOGRAPHIC PLOTS=============================================================



# rolling map
# interactive map

#=Bicolor chloropleth map======================================================

bicol_map_data <- aa_data |>
  group_by(state) |> 
  mutate(events = n(),
         killed = sum(kill_tot, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct(state, .keep_all = TRUE) |> 
  select(state, events, killed, geometry)


bicol_map <- bicol_map_data |> 
  biscale::bi_class(x=events,
                    y=killed,
                    style = "quantile", 
                    dim = 4) |> 
  ggplot(aes(geometry=geometry)) +
  geom_sf(aes(fill=bi_class), show.legend = F) +
  bi_scale_fill(pal = "GrPink2", dim = 4) +
  theme_void() +
  theme(plot.margin = margin(1,1,1,1, "line")) +
  bi_theme()

#create legend
bicol_map_legend <- bi_legend(pal = "GrPink2",
                              dim = 4,
                              xlab = "More Events",
                              ylab = "More Fatalities",
                              size = 8)
#bind plot and legend
design <- "
AAAAA##
AAAAAB#
AAAAA##
"

bicol_map_final <- bicol_map + bicol_map_legend +
  plot_layout(design = design) +
  theme(axis.title = element_text(size = 12, face = "bold"))
bicol_map_final

ggsave("www/bicol_map_eventkill.png", height = 5, width = 10, dpi = 300, bg="white")

# # 3d map
# 
# p_load(rayshader)
# 
# event_map_data <- aa_data |> 
#   group_by(state) |> 
#   count(events = n(),
#         state,
#         geometry,
#         state_centroid_lat,
#         state_centroid_lon)
# 
# #base map
# event_map <- event_map_data |> 
#   ggplot(aes(geometry=geometry)) +
#   geom_sf(aes(fill=n), color="black", show.legend = F) +
#   theme_void() +
#   scale_fill_viridis_c() 
# 
# #convert ggobj to 3dobj and get height matrix
# height_mat <- plot_gg(event_map,
#                       multicore = TRUE,
#                       width=5,height=5,scale=150,windowsize=c(1400,866),
#                       offset_edges = 1,
#                       zoom = 0.55, phi = 30,
#                       shadow = FALSE, 
#                       save_height_matrix = TRUE)
# 
# #change camera angle
# render_camera(zoom=0.75,theta=0,phi=55)
# 
# 
# # Now, call the function with the corrected argument name:
# label_state_attacks(data=event_map_data,
#                     height_mat=height_mat,
#                     state_name = "CA", 
#                     x=340,
#                     y=680, 
#                     z=700,
#                     clear_prev = TRUE)
# 
# label_state_attacks(data = event_map_data,
#                     height_mat = height_mat,
#                     state_name = "TX",
#                     x=780,
#                     y=900, 
#                     z=800,
#                     clear_prev = FALSE)
# 
# label_state_attacks(data = event_map_data,
#                     height_mat = height_mat,
#                     state_name = "FL",
#                     x=1195,
#                     y=960, 
#                     z=800,
#                     clear_prev = FALSE)
# 
# #save
# render_snapshot("temp/event_map.png")



