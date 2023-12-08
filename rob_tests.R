data1 %>% 
  as_tibble %>% 
  dplyr::select(dt=datetime, vial, tot_rib, fjord, temperature) %>% 
  group_by(vial, fjord, tot_rib) %>% 
  dplyr::summarise(t=mean(temperature)) %>% 
  ggplot(aes(tot_rib, t, colour=fjord))+
  geom_point()+
  theme_classic()

data1 %>% 
  as_tibble %>% 
  dplyr::select(dt=datetime, vial, tot_rib, fjord, temperature) %>% 
  ggplot(aes(dt, temperature))+
  geom_point()
