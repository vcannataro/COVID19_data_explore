library(rayrender)
library(tidyverse)


data_subset <- county_data_sf %>% 
  filter(date == as.Date("2020-04-05")) 

data_subset <- data_subset[grep(x = data_subset$NAME,pattern = "New York"),]

data_subset <- sf::st_set_geometry(data_subset,"geometry")

if("spData" %in% rownames(utils::installed.packages())) {
  us_states = spData::us_states
  texas = us_states[us_states$NAME == "Texas",]
  #Fix no sfc class in us_states geometry data
  class(texas$geometry) = c("list","sfc")
}

generate_ground(depth=0,
                material = diffuse(color="grey50",checkercolor="grey20",sigma=90)) %>%
  add_object(extruded_polygon(us_states, x=-96,z=-45, data_column_top = "total_pop_15",
                              scale_data = 1/max(us_states$total_pop_15)*5,
                              material=diffuse(color="#ff2222",sigma=90))) %>%
  add_object(sphere(y=30,x=-100,z=60,radius=10,
                    material=light(color="lightblue",intensity=250))) %>%
  add_object(sphere(y=30,x=100,z=-60,radius=10,
                    material=light(color="orange",intensity=250))) %>%
  render_scene(parallel=TRUE,lookfrom = c(-60,50,-40),lookat=c(0,-5,0),samples=400,fov=30)

counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))

generate_ground(depth=0,
                material = diffuse(color="grey50",checkercolor="grey20",sigma=90)) %>%
  add_object(extruded_polygon(counties, x=-96,z=-45,
                              material=diffuse(color="#ff2222",sigma=90))) %>%
  add_object(sphere(y=30,x=-100,z=60,radius=10,
                    material=light(color="lightblue",intensity=250))) %>%
  add_object(sphere(y=30,x=100,z=-60,radius=10,
                    material=light(color="orange",intensity=250))) %>%
  render_scene(parallel=TRUE,lookfrom = c(-60,50,-40),lookat=c(0,-5,0),samples=400,fov=30)



ggplot(data=data_subset) + 
  geom_sf()

scene = generate_ground(depth=-0.5, material = diffuse(color="white", checkercolor="darkgreen"))
render_scene(scene,parallel=TRUE,samples=500)

scene = generate_ground() %>%
  add_object(sphere(material = diffuse(color="#ff5555")))
render_scene(scene, parallel = TRUE, width = 800, height = 800, samples = 1000)



generate_ground(depth=-0.01,
                material = diffuse(color="grey50",checkercolor="grey20")) %>%
  add_object(extruded_polygon(data_subset, center = T,
                              material=diffuse(color="#ff2222",sigma=90))) %>%
  render_scene(parallel=TRUE,lookfrom = c(0,10,-10),samples=40,fov=60)

data_subset



generate_ground(depth=-0.01,
                material = diffuse(color="grey50",checkercolor="grey20")) %>%
  add_object(extruded_polygon(data_subset, center = T,
                              material=diffuse(color="#ff2222",sigma=90))) %>%
  render_scene(parallel=TRUE,samples=4)



generate_ground(depth=-0.01,
                material = diffuse(color="grey50",checkercolor="grey20")) %>%
  add_object(extruded_polygon(data_subset, center = TRUE,data_column_top = "roll_mean",
                              material=diffuse(color="#ff2222",sigma=90))) %>%
  add_object(sphere(y=30,x=-30,radius=10,
                    material=light(color="lightblue",intensity=40))) %>%
  render_scene(parallel=TRUE,lookfrom = c(0,10,-10),samples=400,fov=60)




ny_county_polygons <- county_polygons[grep(pattern = "new york",x = county_polygons$ID),]

data_subset_just_params <- data_subset %>%  
  as.data.frame() %>%
  select(ID.x,cases,lag_cases,roll_mean,roll_new_cases)

data_subset_just_params <- data_subset_just_params %>%
  mutate(ID=ID.x)

# ny_county_polygons <- sf::st_join(ny_county_polygons,data_subset_just_params,join="ID")
ny_county_polygons <- left_join(ny_county_polygons,as.data.frame(data_subset_just_params),"ID")


class(ny_county_polygons$geom) = c("list","sfc")

# county_polygons
scene <- generate_ground(depth=0,spheresize=1000, 
                material=diffuse(color="#000000",
                                 noise=1/10,
                                 noisecolor = "#654321")) %>%
  add_object(extruded_polygon(ny_county_polygons, center = T,
                              material= metal(color="orange"))) 

render_scene(scene, parallel=TRUE,samples=400,lookfrom = c(0,7,-9),fov=60)



ny_county_polygons <- ny_county_polygons %>%
  filter(!is.na(roll_mean))


generate_ground(depth=0,spheresize=1000, 
                material=diffuse(color="#000000",
                                    noise=1/10,
                                    noisecolor = "#654321")) %>%
  add_object(extruded_polygon(ny_county_polygons, center = T,data_column_top = "roll_mean",
                              scale_data = 1/max(ny_county_polygons$roll_mean)*5,
                              material= metal(color="orange"))) %>%
  render_scene(parallel=TRUE,samples=400,lookfrom = c(0,7,-9),fov=60)



scene <- generate_ground(depth=0,spheresize=1000, 
                material=diffuse(color="#000000",
                                 noise=1/10,
                                 noisecolor = "#654321")) %>%
  add_object(extruded_polygon(ny_county_polygons, center = T,data_column_top = "roll_mean",
                              scale_data = 1/max(ny_county_polygons$roll_mean)*5,
                              material= metal(color="gold"))) 


scene <- generate_ground(depth=0,spheresize=1000, 
                         material=diffuse(color="#000000",
                                          noise=1/10,
                                          noisecolor = "#654321")) %>%
  add_object(extruded_polygon(ny_county_polygons, center = T,data_column_top = "roll_mean",
                              scale_data = 1/max(ny_county_polygons$roll_mean)*5,
                              material= dielectric(color="darkgreen",attenuation = c(1,1,0.3)/200),material_id = 1)) %>%
  add_object(sphere(y=30,x=-30,radius=10,
                    material=light(color="lightblue",intensity=50)))


# original nice scene
render_scene(scene = scene, parallel=TRUE,samples=400,
             lookfrom = c(0,7,-9),fov=60,width=500, height=500)

# playing with camera angles below
render_scene(scene = scene, parallel=TRUE,samples=600,
             lookfrom = c(camerax[1], 8, cameraz[1]),fov=60,width=500, height=500)


# render_scene(scene = scene, parallel=TRUE,samples=400,
#              lookfrom = c(camerax[1], 25, cameraz[1]),fov=35,lookat = c(0,0,0))


frames = 360

camerax=-10*cos(seq(0,360,length.out = frames+1)[-frames-1]*pi/180)
cameraz=10*sin(seq(0,360,length.out = frames+1)[-frames-1]*pi/180)



av::av_capture_graphics(expr = {
  for(i in 1:frames) {
    render_scene(scene, width=500, height=500, fov=60,
                 lookfrom = c(camerax[i], 7, cameraz[i]),samples = 400, parallel = TRUE)
    print(i)
  }
}, width=500,height=500, framerate = 60, output = "output_data/figures/tests/NYS_rotate.mp4")


# av::av_capture_graphics(expr = {
#   for(i in 1:frames) {
#     render_scene(scene, width=500, height=500, fov=35,
#                  lookfrom = c(camerax[i], 25, cameraz[i]),
#                  lookat = c(0,9,0),samples = 400, parallel = TRUE)
#   }
# }, width=500,height=500, framerate = 60, output = "output_data/figures/tests/NYS_rotate.mp4")
# 

# 
# %>%
#   render_scene(parallel=TRUE,samples=400,lookfrom = c(0,7,-9),fov=60)
# 




data_subset_all <- county_data_sf %>% 
  dplyr::filter(date == as.Date("2020-06-22")) 



# ny_county_polygons <- county_polygons[grep(pattern = "new york",x = county_polygons$ID),]

data_subset_just_params <- data_subset_all[,c("ID.x","cases","lag_cases","roll_mean","roll_new_cases")]

data_subset_just_params <- data_subset_just_params %>%
  mutate(ID=ID.x)

# ny_county_polygons <- sf::st_join(ny_county_polygons,data_subset_just_params,join="ID")
polygons <- left_join(county_polygons,as.data.frame(data_subset_just_params),"ID")


polygons[which(is.na(polygons$roll_mean)),]

polygons <- polygons %>%
  filter(!is.na(roll_mean))


scene <- generate_ground(depth=0,spheresize=1000, 
                         material=diffuse(color="#000000",
                                          noise=1/10,
                                          noisecolor = "#654321")) %>%
  add_object(extruded_polygon(polygons, center = T,data_column_top = "roll_mean",
                              scale_data = 1/max(polygons$roll_mean,na.rm = T)*5,
                              material= dielectric(color="darkgreen"),
                                                   # attenuation = c(1,1,0.3)/200),
                              material_id = 1)) %>%
  add_object(sphere(y=20,x=0,z = 0,radius=7,
                    material=light(color="lightblue",intensity=60)))

render_scene(scene = scene, parallel=TRUE,samples=400,
             lookfrom = c(0,15,-15),fov=40,width=500, height=500)

# 
# scene = generate_ground(depth=-0.5, material = diffuse(color="white", checkercolor="darkgreen"))
# scene = scene %>%
#   add_object(sphere(x=0,y=1,z=2,radius=0.5,material = diffuse(color=c(1,0,1))))
# render_scene(scene,fov=20,parallel=TRUE,samples=5)



