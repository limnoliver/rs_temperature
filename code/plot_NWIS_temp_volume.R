library("ggplot2")
theme_set(theme_bw())
library("sf")
library("maps")#provides maps of the USA, with state and county borders.

#retrieve USA state map then converting as sf objects.
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

sites_sf <- lake_temp_inventory %>%
  filter(!(is.na(dec_lat_va) & is.na(dec_long_va))) %>%
  st_as_sf(coords = c("dec_long_va" ,"dec_lat_va"), crs = 4326)

p_sites_inventory <- ggplot() +
  geom_sf(data = states, color="black", fill=NA) + # plot state outlines
  geom_sf(data = sites_sf, aes(geometry = geometry,
                               size = count_nu)) +
  #scale_color_gradient(low="blue", high="red") +
  scale_color_viridis_c(trans = "sqrt", alpha = .4) +
  # setting long and lat limit to grpah contiguous states.  
  coord_sf(xlim = c(-127, -66), ylim = c(24, 50), expand = FALSE) +
  
  xlab("Longitude") + ylab("Latitude") +
  #classic dark-on-light theme: changing gray background to lighter them
  theme_bw() +
  ggtitle("NWIS Temperature Data Inventory") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        legend.title = element_blank(), # to remove legend box title.
        ## for legend box position
        legend.position = "bottom")

p_sites_inventory


