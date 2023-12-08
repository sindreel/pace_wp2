require(tidyverse)
require(vegan)

meta<-gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0")

sel<-gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1kTBdcz_luBNkyh0wuKUK3fmVckBsaZMt/edit#gid=1110154350") %>%
  dplyr::select(3:4) %>%
  dplyr::filter(Thermal...4=="x")


rd<-gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1kTBdcz_luBNkyh0wuKUK3fmVckBsaZMt/edit#gid=2122083308") %>%
  # dplyr::filter(grepl("Satr_S", BiomarkTBL) |
  #                grepl("Satr_1", BiomarkTBL) |
  #               grepl("PA", BiomarkTBL) |
  #              grepl("AUR", BiomarkTBL) |
  #             grepl("PA", BiomarkTBL)) %>%
  dplyr::select(2:47) %>%
  dplyr::select(one_of(sel$biomarker)) %>%
  mutate_if(is.numeric, function(x) replace_na(x, 0)) %>%
  rda(.) %>%
  plot()

rd$species %>% view

gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1kTBdcz_luBNkyh0wuKUK3fmVckBsaZMt/edit#gid=2122083308") %>%
  dplyr::select(1) %>%
  dplyr::mutate(
    river = case_when(
      grepl("PA", BiomarkTBL) |
        grepl("BF", BiomarkTBL) ~ "vosso",
      grepl("AUR", BiomarkTBL) ~ "aurland",
      grepl("Satr_1", BiomarkTBL) ~ "Stjordal",
      grepl("Satr_S", BiomarkTBL) ~ "Beiarn",
      nchar(BiomarkTBL)==3 | grepl("SL", BiomarkTBL) ~"Stjordal",
      T~"CONTROL"
    )) %>%
  bind_cols(rd$sites) %>%
  dplyr::rename(vial=BiomarkTBL) %>%
  mutate(vial=gsub("Satr_", "", vial)) %>%
  left_join(meta %>% distinct(vial, Spp, TL)) %>%
  mutate(Spp=case_when(is.na(Spp) ~ "CONTROL", T~Spp)) %>%
  ggplot(aes(PC1, PC2, colour=river, group=river, pch=Spp))+
  geom_point()+
  theme_classic()+
  stat_ellipse(geom="polygon", alpha=0)

