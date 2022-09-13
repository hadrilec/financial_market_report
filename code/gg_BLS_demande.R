
library(rsdmx)
library(tidyverse)

link_graph =  Sys.getenv("HOME")


sdmx <- readSDMX(providerId = "ECB", resource = "data", flowRef = "BLS",
                 key = "Q.U2+DE+FR+IT+ES.ALL.O+LE+SME.E.Z.B3+F3.ZZ.D.BWFNET+FNET+BFNET", verbose = F)
df <- as.data.frame(sdmx)

n = str_count(df[1,"TITLE_COMPL"], pattern = " - ") + 1

list_variable = df %>%
  distinct(TITLE_COMPL) %>% 
  separate(TITLE_COMPL, into = paste0("col",c(1:n)), sep = " - ", remove = FALSE)

df_ = df %>%
  mutate(time = yq(obsTime)) %>% 
  separate(TITLE_COMPL, into = paste0("col", c(1:n)), sep = " - ", remove = FALSE) %>% 
  mutate(label = case_when(str_detect(col5, "Forward")  & str_detect(col3, "Small") ~ "PME (anticipée)",
                           str_detect(col5, "Forward")  & str_detect(col3, "Large") ~ "Grands groupes (anticipée)",
                           str_detect(col5, "Backward") & str_detect(col3, "Overall") ~ "Demande",
                           str_detect(col5, "Forward")  & str_detect(col3, "Overall") ~ "Demande anticipée",
                           TRUE ~ ""
         )) %>%
  filter(!(str_detect(TITLE_COMPL, "Small.*Backward"))) %>% 
  filter(!(str_detect(TITLE_COMPL, "Large.*Backward"))) %>% 
  mutate(area_label =  case_when(REF_AREA == "U2" ~ "Zone Euro",
                                 REF_AREA == "FR" ~ "France",
                                 REF_AREA == "IT" ~ "Italie",
                                 REF_AREA == "ES" ~ "Espagne",
                                 REF_AREA == "DE" ~ "Allemagne"
  ))


list_variable_ = df_ %>%
  distinct(TITLE_COMPL) 

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
  
  name_gg = sprintf("gg_BLS_%s_demande", area)
  file_name = paste0(name_gg, ".pdf")

  title_ =  sprintf("Demande de crédit des entreprises %s", area_label)
  
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
  
  gg_BLS_demande = 
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
  
    gg_BLS_demande %>% ggsave(file=file.path(link_graph, file_name ), width = 15, height = 10)


  assign(name_gg, gg_BLS_demande, envir = .GlobalEnv)
}


name_gg_agg = sprintf("gg_BLS_%s_demande", "tot")

file_name_agg = paste0(name_gg_agg, ".pdf")

df_tot = df_ %>% 
  filter(time >= "2008-01-01") %>% 
  mutate(value = obsValue)

gg_BLS_demande_agg = 
  ggplot(data = df_tot, aes(x = time, y = value, color = area_label, group = label)) + 
  geom_line(size = 1.2) + 
  facet_grid(label ~ area_label, scales = "free") + 
  ggtitle("Demande de crédit des entreprises") + 
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
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black")) +
  labs(subtitle = sprintf("Dernier point : %s", max(df_tot$time))) 

gg_BLS_demande_agg %>% ggsave(file=file.path(link_graph, file_name_agg ), width = 15, height = 10)


