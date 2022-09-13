
library(tidyverse)
library(rsdmx)

link_graph =  Sys.getenv("HOME")

sdmx <- readSDMX(providerId = "ECB", resource = "data", flowRef = "BLS",
                 key = "Q.U2+FR+DE+IT+ES.ALL.O+MTY.E.Z.B3+F3.ST+TC.S.BWFNET+FNET+BFNET", verbose = F)
df <- as.data.frame(sdmx)

n = str_count(df[1,"TITLE_COMPL"], pattern = "-")

list_variable = df %>%
  distinct(TITLE_COMPL) %>%
  separate(TITLE_COMPL, into = paste0("col",c(1:n)), sep = "-", remove = FALSE)

df_ = df %>%
  mutate(time = yq(obsTime)) %>%
  separate(TITLE_COMPL, into = paste0("col",c(1:n)), sep = "-", remove = FALSE) %>%
  mutate(label = case_when(str_detect(col5, "Forward looking") ~ "Offre anticipée",
                           str_detect(col3, "Maturity") ~ "Maturité",
                           str_detect(col5, "Backward") & str_detect(col6, "standards") ~ "Offre",
                           str_detect(col5, "Backward") & str_detect(col6, "terms") ~ "Conditions Effectives"
         )) %>%
  mutate(area_label =  case_when(REF_AREA == "U2" ~ "Zone Euro",
                                 REF_AREA == "FR" ~ "France",
                                 REF_AREA == "IT" ~ "Italie",
                                 REF_AREA == "ES" ~ "Espagne",
                                 REF_AREA == "DE" ~ "Allemagne"
                                 ))

list_variable_ = df_ %>%
  distinct(TITLE_COMPL) %>%
  separate(TITLE_COMPL, into = paste0("col",c(1:n)), sep = "-", remove = FALSE)


list_area = df_ %>%
  distinct(REF_AREA) %>%
  pull(REF_AREA) %>%
  as.character()

for(area in list_area){

  area_label = ""
  if(area == "U2"){area_label = "de la zone euro"}
  if(area == "FR"){area_label = "françaises"}
  if(area == "DE"){area_label = "allemandes"}
  if(area == "IT"){area_label = "italiennes"}
  if(area == "ES"){area_label = "espagnoles"}

  name_gg = sprintf("gg_BLS_%s_offre", area)
  file_name = paste0(name_gg, ".pdf")

  title_ =  sprintf("Offre de crédit aux entreprises %s", area_label)

  df2 = df_ %>% filter(REF_AREA == area)

  last_values = df2 %>%
    group_by(label) %>%
    filter(time == max(time)) %>%
    select(label, time, obsValue) %>%
    mutate(value = round(obsValue, 2)) %>%
    as.data.frame()

  subtt = paste("Source: Bank Lending Survey, BCE \n Dernier point:", max(df_$time), "\n")

  for(i in 1:nrow(last_values)){
    subtt = paste(subtt, sprintf("%s : %s, ", last_values[i,"label"], last_values[i,"value"]))
  }

  gg_BLS_offre =
    ggplot(data = df2,
           aes(x = time, y = obsValue, color = label, group = label)) +
    geom_line(size = 1.2) +
    facet_wrap(~label) +
    ggtitle(title_) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggthemes::theme_stata() +
    guides(color = guide_legend("")) +
    theme(axis.text.y = element_text(angle = 0 ),
          text = element_text(size = 18),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_line(colour = "black", size = 0.5, linetype = 1),
          plot.caption = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face="italic", color="black")) +
    labs(subtitle = subtt) 
  
  gg_BLS_offre %>% ggsave(file=file.path(link_graph, file_name), width = 15, height = 10)


  assign(name_gg, gg_BLS_offre, envir = .GlobalEnv)

}

df_tot = df_ %>%
  filter(time >= "208-01-01")

name_gg_agg = sprintf("gg_BLS_%s_offre", "tot")

file_name_agg = paste0(name_gg_agg, ".pdf")

gg_BLS_offre_agg =
  ggplot(data = df_tot,
         aes(x = time, y = obsValue, color = area_label, group = label)) +
  geom_line(size = 1.2) +
  facet_grid(label ~ area_label, scales = "free") +
  ggtitle("Offre de crédit aux entreprises") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggthemes::theme_stata() +
  guides(color = guide_legend("")) +
  theme(axis.text.y = element_text(angle = 0 ),
        text = element_text(size = 16),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", size = 0.5, linetype = 1),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face="italic", color="black")) +
  labs(subtitle = sprintf("Dernier point : %s", max(df_tot$time))) 

gg_BLS_offre_agg %>% ggsave(file=file.path(link_graph, file_name_agg ), width = 15, height = 10)

