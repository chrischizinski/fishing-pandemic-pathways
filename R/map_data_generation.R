library(tidyverse)
library(sf)
library(here)
library(urbnmapr)
library(tmap)

sf::sf_use_s2(FALSE)

NE_zips <- read_csv("/Volumes/Envoy Pro/data/NebraskaZipCodes.csv")

zip_shape <- here("/Volumes/Envoy Pro/data/gis/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") %>%
  st_read() %>% 
  select(ZCTA5CE10, geometry)



st_intersection(zip_shape, contig_sf) %>% 
  st_make_valid() %>% 
  st_transform(5070) -> trimmed_zips

st_simplify(trimmed_zips, preserveTopology = FALSE, dTolerance = 1000) -> trimmed_zips2

data_snap <- feather::read_feather("/Volumes/Envoy Pro/data/PermitStampsCustomer202220201.feather")



good_permitz <- c("Hunt Fish Combo", "Fish")

data_snap %>% 
  filter(Year %in% c(2010:2021),
         Type %in% good_permitz,
         !is.na(resident)) %>% 
  mutate(lifeRange2 = ifelse(grepl("Day", lifeRange),"Daily","Annual(+)"),
         resident = ifelse(resident == "F","Non-resident", "Resident"),
         zip = trimws(zip)) %>% 
  group_by(zip) %>% 
  summarise(N = n_distinct(customerUID)) -> n_per_zip


trimmed_zips2 %>% 
  left_join(n_per_zip, by = c("ZCTA5CE10" = "zip")) %>% 
  # mutate(N = ifelse(is.na(N),0,N)) %>% 
  filter(!st_is_empty(geometry)) -> anglers_zip

st_write(anglers_zip, "data/gis/angler_zip.shp")

 ggplot(data = anglers_zip, aes()) + 
  geom_sf(aes(fill = N), size = 0) + 
   geom_sf(data = contig_sf, color = "black", fill = NA, size = 0.5) + 
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits =c(1,18000)) + 
  theme_void() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())
 
 anglers_zip <- st_read( "data/gis/angler_zip.shp", quiet = TRUE)
 contig_sf <- st_read("data/gis/contig_sf.shp", quiet = TRUE,)
 
 ggplot(data = anglers_zip, aes()) + 
   geom_sf(aes(fill = N), size = 0.01, color = "#f9f4f1") + 
   geom_sf(data = contig_sf, color = "black", fill = NA, size = 0.5) + 
   scale_fill_distiller(palette = "Spectral", na.value = NA, limits =c(1,16000)) + 
   coord_sf() + 
   theme_void() + 
   theme(rect = element_rect(fill = "transparent", color = NA),
         legend.position = "right",
         legend.title = element_blank(),
         legend.text = element_text(size = 16, color = "#f9f4f1"),
         panel.grid = element_blank(),
         axis.line = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         axis.title =  element_blank(),
         plot.margin = margin(1,1,1,1, "cm")
         ) -> zip_map
 
 ggsave(zip_map, 
        filename = "figs/angler_zips2.png", 
        bg = "transparent", 
        width = 3840 , 
        height = 2160,
        units = "px", 
        dpi = 320, 
        device = "png")
 
 828/664
 
 1300/1.246988
 
 library(magick)
 image<- magick::image_read("figs/angler_zips2.png")
 print(image) 
 image<- image_trim(image)
 
 3292/1797
 1500*1.831942
 image_write(image, path = "figs/angler_zips2.png", format = "png")
# btb::kernelSmoothing(anglers_zip, 
#                      iCellSize = 10000L,
#                      iBandwidth = 20000L,
#                      vQuantiles = c(0.1, 0.5, 0.9))
# 
# # st_write(trimmed_zips2, "data/gis/trimmed_zips.shp")
# library(metR)
# object.size(anglers_zip)/ 1000000 
# 
# st_bbox(contig_sf)
# st_bbox(zip_shape)
# st_bbox(trimmed_zips)
# 
# qtm(trimmed_zips2)
# 
# sf::st_make_valid(trimmed_zips) 
# 
# trimmed_zips$geometry %>%
#   s2::s2_rebuild() %>%
#   sf::st_as_sfc() -> trimmed_zips$geometry
# 
# qtm(contig_sf)
# plot(trimmed_zips)
# 
# zip_shape$geometry %>%
#   s2::s2_rebuild() %>%
#   sf::st_as_sfc() -> zip_shape$geometry
# 
# plot(zip_shape)
# 
# 
# 
# ggplot(data = ne_anglers_zip) + 
#   geom_sf(mapping = aes(fill = N),
#           color = "black", size = 0.05) + 
#   scale_fill_viridis_c(option = "D", limits = c(0,20000))
# 
# 
# 
# 
# 
# 
# zip_shape %>% 
#   filter(ZCTA5CE10 %in% NE_zips$zip) %>% 
#   select(ZCTA5CE10,geometry)-> NE_zip_shape
# 
# 
# sf::sf_use_s2(FALSE)
# 
# 
# plot(NE_zip_shape)
# plot(nebraska_sf)
# ?sf::st_intersection()
# 
# 
# 
# 
# 
# n_per_state$state
# 
# 
# 
# 
# 
# 
# 
# 
# 
# summary(anglers_zip$N)
# 
# ggplot(data = anglers_zip) + 
#   geom_sf(mapping = aes(fill = N),
#           color = "#ffffff", size = 0.05) + 
#   scale_fill_viridis_c(option = "D", limits = c(0,20000))

 
 states_sf <- get_urbn_map("states", sf = TRUE) %>% 
   filter(state_abbv == "NE") %>% 
   st_transform(5070) 
 
 
 NE_zips <- read_csv("/Volumes/Envoy Pro/data/NebraskaZipCodes.csv")
 
 ne_zip_shape <- here("/Volumes/Envoy Pro/data/gis/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") %>%
   st_read() %>% 
   select(ZCTA5CE10, geometry) %>% 
   filter(ZCTA5CE10 %in% NE_zips$zip) %>% 
   st_transform(5070) 
 
 
 
 st_intersection(ne_zip_shape, states_sf) %>% 
   st_make_valid()  -> trimmed_zips
 
 st_simplify(trimmed_zips, preserveTopology = FALSE, dTolerance = 1000) -> trimmed_zips2
 
 R3_types_fish %>%
   filter(permitYear %in% c(2019,2020),
          r3_type != "retained") %>%
   select(permitYear, customerUID, r3_type) %>% 
   left_join(data_snap %>% 
               distinct(customerUID,Year,zip),
             by = c("customerUID"= "customerUID",
                    "permitYear" = "Year"))  %>% 
   filter(zip %in% NE_zips$zip) %>% 
   group_by(permitYear, zip) %>% 
   summarise(N = n_distinct(customerUID)) %>% 
   pivot_wider(names_from =permitYear, values_from = N, values_fill = list(N =0)) %>% 
   ungroup() %>% 
   mutate(pct_change = ((`2020`-`2019`)/`2019`)) %>% 
   select(zip, pct_change) -> zip_pct_chg
 
 trimmed_zips %>% 
   left_join(zip_pct_chg, by = c("ZCTA5CE10" = "zip")) -> zip_chg
   
 
 ggplot(data = zip_chg, aes()) + 
   geom_sf(aes(fill = pct_change), size = 0.5, color = "#f9f4f1") + 
   geom_sf(data = states_sf, color = "black", fill = NA, size = 0.5) + 
   scale_fill_distiller(palette = "Spectral", na.value = NA, limits =c(-1.5,5), labels = scales::percent) + 
   coord_sf() + 
   theme_void() + 
   theme(rect = element_rect(fill = "transparent", color = NA),
         legend.position = "right",
         legend.title = element_blank(),
         legend.text = element_text(size = 16, color = "#f9f4f1"),
         panel.grid = element_blank(),
         axis.line = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         axis.title =  element_blank(),
         plot.margin = margin(1,1,1,1, "cm")
   ) -> ne_zip_chg_map
 
 
 ggsave(ne_zip_chg_map, 
        filename = "figs/ne_angler_zips_chg.png", 
        bg = "transparent", 
        width = 3840 , 
        height = 2160,
        units = "px", 
        dpi = 320, 
        device = "png")
 