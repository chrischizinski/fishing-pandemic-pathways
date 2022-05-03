#Image fill 


library(echarts4r)
library(echarts4r.assets)

  
  gender <- data.frame(gender = c("Females in 2019","Females in 2020", "Females in 2021"), value = c(29.2, 31.8, 29.5), path = c("path://M11.3,89.8c7.2,0,12.4,5.9,13.4,9.2l8.8,29c1.8,6.2-6.2,8.8-8.1,2.7l-8-26.6H13l13.3,47.3H13.6v36.9c0,6.3-9.5,6.3-9.5,0
	v-37.2h-4.8v37.1c0,6.4-9.5,6.4-9.5,0v-36.8h-12.7l13.2-47.3h-4.4l-7.9,26.7c-1.9,5.8-9.9,3.4-8.1-2.8l8.8-29
	c0.9-3.3,5.1-9.2,12.4-9.2C-8.9,89.8,11.3,89.8,11.3,89.8z",
                                                                                                                                 "path://M11.3,89.8c7.2,0,12.4,5.9,13.4,9.2l8.8,29c1.8,6.2-6.2,8.8-8.1,2.7l-8-26.6H13l13.3,47.3H13.6v36.9c0,6.3-9.5,6.3-9.5,0
	v-37.2h-4.8v37.1c0,6.4-9.5,6.4-9.5,0v-36.8h-12.7l13.2-47.3h-4.4l-7.9,26.7c-1.9,5.8-9.9,3.4-8.1-2.8l8.8-29
	c0.9-3.3,5.1-9.2,12.4-9.2C-8.9,89.8,11.3,89.8,11.3,89.8z",
                                                                                                                                 "path://M11.3,89.8c7.2,0,12.4,5.9,13.4,9.2l8.8,29c1.8,6.2-6.2,8.8-8.1,2.7l-8-26.6H13l13.3,47.3H13.6v36.9c0,6.3-9.5,6.3-9.5,0
	v-37.2h-4.8v37.1c0,6.4-9.5,6.4-9.5,0v-36.8h-12.7l13.2-47.3h-4.4l-7.9,26.7c-1.9,5.8-9.9,3.4-8.1-2.8l8.8-29
	c0.9-3.3,5.1-9.2,12.4-9.2C-8.9,89.8,11.3,89.8,11.3,89.8z"))

gender %>% 
  e_charts(gender) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel= list(show=FALSE)) %>%
  e_y_axis(max=100, 
           splitLine=list(show = FALSE),
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel=list(show=FALSE)) %>%
  e_color(color = c('#69cce6','#eee'),"#1f1f22") %>%
  e_pictorial(value, symbol = path, z=10, name= 'realValue', 
              symbolBoundingData= 100, symbolClip= TRUE) %>% 
  e_pictorial(value, symbol = path, name= 'background', 
              symbolBoundingData= 100) %>% 
  e_labels(position = "bottom", offset= c(0, 10), 
           textStyle =list(fontSize= 20, fontFamily= 'Arial', 
                           fontWeight ='bold', 
                           color= '#69cce6'),
           formatter="{@[1]}% {@[0]}") %>%
  e_legend(show = FALSE) %>%
  e_theme("westeros")

# R3_types_fish %>% 
#   filter(permitYear %in% c(2019,2020,2021),
#          r3_type != "retained",
#          !is.na(sex)) %>% 
#   group_by(permitYear, sex) %>% 
#   count() %>% 
#   group_by(permitYear) %>% 
#   mutate(prct = (n/sum(n)*100)) %>% 
#   filter(sex == "Female") %>% 
#   select(-sex,-n) %>% 
#   pivot_wider(names_from = permitYear, values_from = prct) 


data_snap <- feather::read_feather("/Volumes/Envoy Pro/data/PermitStampsCustomer202220201.feather")



R3_types_fish %>%
  filter(permitYear %in% c(2019,2020,2021),
         r3_type != "retained") %>%
  left_join(data_snap %>% 
              distinct(customerUID,Year,dob),
            by = c("customerUID"= "customerUID",
                   "permitYear" = "Year")) %>% 
  mutate(greater40 = ifelse(permitYear - lubridate::year(dob) >= 40,1,0)) %>% 
  group_by(permitYear, greater40) %>%
  count() %>% 
  group_by(permitYear) %>%
  mutate(prct = (n/sum(n)*100))  %>% 
  filter(greater40 == 1) %>%
  select(-greater40,-n) %>%
  pivot_wider(names_from = permitYear, values_from = prct)

age <- data.frame(gender = c(">40 yo in 2019",">40 yo in 2020", ">40 yo in 2021"), value = c(48.4, 47.8, 51.0), path = c("path://M256,8C119,8,8,119,8,256S119,504,256,504,504,393,504,256,393,8,256,8Zm92.49,313h0l-20,25a16,16,0,0,1-22.49,2.5h0l-67-49.72a40,40,0,0,1-15-31.23V112a16,16,0,0,1,16-16h32a16,16,0,0,1,16,16V256l58,42.5A16,16,0,0,1,348.49,321Z",
                                                                                                                               "path://M256,8C119,8,8,119,8,256S119,504,256,504,504,393,504,256,393,8,256,8Zm92.49,313h0l-20,25a16,16,0,0,1-22.49,2.5h0l-67-49.72a40,40,0,0,1-15-31.23V112a16,16,0,0,1,16-16h32a16,16,0,0,1,16,16V256l58,42.5A16,16,0,0,1,348.49,321Z",
                                                                                                                               "path://M256,8C119,8,8,119,8,256S119,504,256,504,504,393,504,256,393,8,256,8Zm92.49,313h0l-20,25a16,16,0,0,1-22.49,2.5h0l-67-49.72a40,40,0,0,1-15-31.23V112a16,16,0,0,1,16-16h32a16,16,0,0,1,16,16V256l58,42.5A16,16,0,0,1,348.49,321Z"))

age %>% 
  e_charts(gender) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel= list(show=FALSE)) %>%
  e_y_axis(max=100, 
           splitLine=list(show = FALSE),
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel=list(show=FALSE)) %>%
  e_color(color = c('#91a5a4','#eee'),"#1f1f22") %>%
  e_pictorial(value, symbol = path, z=10, name= 'realValue', 
              symbolBoundingData= 100, symbolClip= TRUE) %>% 
  e_pictorial(value, symbol = path, name= 'background', 
              symbolBoundingData= 100) %>% 
  e_labels(position = "bottom", offset= c(0, 10), 
           textStyle =list(fontSize= 20, fontFamily= 'Arial', 
                           fontWeight ='bold', 
                           color= '#69cce6'),
           formatter="{@[1]}% {@[0]}") %>%
  e_legend(show = FALSE) %>%
  e_theme("westeros")  


R3_types_fish %>%
  filter(permitYear %in% c(2019,2020,2021),
         r3_type== "reactivated") %>%
  mutate(lag3yr = ifelse(yr_diff >=3,1,0)) %>% 
  group_by(permitYear, lag3yr) %>%
  count() %>% 
  group_by(permitYear) %>%
  mutate(prct = (n/sum(n)*100))  %>% 
  filter(lag3yr == 1) %>%
  select(-lag3yr,-n) %>%
  pivot_wider(names_from = permitYear, values_from = prct) 


lagyrs <- data.frame(gender = c(">3y lag in 2019",">40 yo in 2020", ">40 yo in 2021"), value = c(56.1, 62.4, 67.2), path = c("path://M343.1 25.7c.5 31.23 14.5 58.24 38.1 88.1l4.1 5.2-43.9 64H402l12.6-52.9 6.4-.7c18.7-1.9 41-10.2 60-19.9 7.1-3.7 13.6-7.5 19.6-11.3-4.2 0-7.4.13-12.2 0-22.4-.61-48.7-2.52-67.4-10.26-20.4-8.51-42.6-27.79-60.7-45.05-6.6-6.31-12.3-12.04-17.2-17.19zM74.93 26.9C55.54 74.74 74.44 140.8 98.75 183H119c-.8-2.4-1.2-4.9-1.2-7.5 0-13.7 11.3-25 25-25s25 11.3 25 25c0 2.6-.4 5.1-1.2 7.5h60.7c-16-37.2-41.8-82.7-82.8-116.96 5.1 19.17 12.2 37.76 23.8 55.56l-15.2 9.8c-17.2-26.6-25.3-54.39-30.9-81.78C108 40.49 92.3 32.73 74.93 26.9zm67.87 141.6c-4 0-7 3-7 7s3 7 7 7 7-3 7-7-3-7-7-7zM89 201v30h196.5c1.7-5.8 3.3-11.7 5-17.6l17.4 4.8c-1.2 4.3-2.4 8.5-3.6 12.8H423v-30H89zm17.8 48 31.5 167.5c47.8-19.6 78.6-46.8 100.7-78.7 18.5-26.7 30.8-57 40.9-88.8H106.8zm192.1 0c-10.7 34.6-24 68.5-45.2 99.1-24.4 35.1-59.5 65.5-112.1 86.4l9.9 52.5h209l44.7-238H298.9z",
                                                                                                                         "path://M343.1 25.7c.5 31.23 14.5 58.24 38.1 88.1l4.1 5.2-43.9 64H402l12.6-52.9 6.4-.7c18.7-1.9 41-10.2 60-19.9 7.1-3.7 13.6-7.5 19.6-11.3-4.2 0-7.4.13-12.2 0-22.4-.61-48.7-2.52-67.4-10.26-20.4-8.51-42.6-27.79-60.7-45.05-6.6-6.31-12.3-12.04-17.2-17.19zM74.93 26.9C55.54 74.74 74.44 140.8 98.75 183H119c-.8-2.4-1.2-4.9-1.2-7.5 0-13.7 11.3-25 25-25s25 11.3 25 25c0 2.6-.4 5.1-1.2 7.5h60.7c-16-37.2-41.8-82.7-82.8-116.96 5.1 19.17 12.2 37.76 23.8 55.56l-15.2 9.8c-17.2-26.6-25.3-54.39-30.9-81.78C108 40.49 92.3 32.73 74.93 26.9zm67.87 141.6c-4 0-7 3-7 7s3 7 7 7 7-3 7-7-3-7-7-7zM89 201v30h196.5c1.7-5.8 3.3-11.7 5-17.6l17.4 4.8c-1.2 4.3-2.4 8.5-3.6 12.8H423v-30H89zm17.8 48 31.5 167.5c47.8-19.6 78.6-46.8 100.7-78.7 18.5-26.7 30.8-57 40.9-88.8H106.8zm192.1 0c-10.7 34.6-24 68.5-45.2 99.1-24.4 35.1-59.5 65.5-112.1 86.4l9.9 52.5h209l44.7-238H298.9z",
                                                                                                                         "path://M343.1 25.7c.5 31.23 14.5 58.24 38.1 88.1l4.1 5.2-43.9 64H402l12.6-52.9 6.4-.7c18.7-1.9 41-10.2 60-19.9 7.1-3.7 13.6-7.5 19.6-11.3-4.2 0-7.4.13-12.2 0-22.4-.61-48.7-2.52-67.4-10.26-20.4-8.51-42.6-27.79-60.7-45.05-6.6-6.31-12.3-12.04-17.2-17.19zM74.93 26.9C55.54 74.74 74.44 140.8 98.75 183H119c-.8-2.4-1.2-4.9-1.2-7.5 0-13.7 11.3-25 25-25s25 11.3 25 25c0 2.6-.4 5.1-1.2 7.5h60.7c-16-37.2-41.8-82.7-82.8-116.96 5.1 19.17 12.2 37.76 23.8 55.56l-15.2 9.8c-17.2-26.6-25.3-54.39-30.9-81.78C108 40.49 92.3 32.73 74.93 26.9zm67.87 141.6c-4 0-7 3-7 7s3 7 7 7 7-3 7-7-3-7-7-7zM89 201v30h196.5c1.7-5.8 3.3-11.7 5-17.6l17.4 4.8c-1.2 4.3-2.4 8.5-3.6 12.8H423v-30H89zm17.8 48 31.5 167.5c47.8-19.6 78.6-46.8 100.7-78.7 18.5-26.7 30.8-57 40.9-88.8H106.8zm192.1 0c-10.7 34.6-24 68.5-45.2 99.1-24.4 35.1-59.5 65.5-112.1 86.4l9.9 52.5h209l44.7-238H298.9z"))

lagyrs %>% 
  e_charts(gender) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel= list(show=FALSE)) %>%
  e_y_axis(max=100, 
           splitLine=list(show = FALSE),
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel=list(show=FALSE)) %>%
  e_color(color = c('#b3a975','#eee'),"#1f1f22") %>%
  e_pictorial(value, symbol = path, z=10, name= 'realValue', 
              symbolBoundingData= 100, symbolClip= TRUE) %>% 
  e_pictorial(value, symbol = path, name= 'background', 
              symbolBoundingData= 100) %>% 
  e_labels(position = "bottom", offset= c(0, 10), 
           textStyle =list(fontSize= 20, fontFamily= 'Arial', 
                           fontWeight ='bold', 
                           color= '#f9f4f1'),
           formatter="{@[1]}% {@[0]}") %>%
  e_legend(show = FALSE) %>%
  e_theme("westeros") 

library(ggplot2)
library(cowplot)
library(magick)

age1 <- image_read("img/age.png")  

age1 <- age1 %>%
  image_scale("100") %>% 
  image_border("grey") %>%
  image_annotate("Powered By R", color = "white", size = 30, 
                 location = "+10+50", gravity = "northeast")

# Stack them on top of each other
final_plot <- image_append(image_scale(c(age1, age1), "500"), stack = TRUE)
# And overwrite the plot without a logo
image_write(final_plot, "figs/test.png")
