
library(tidyverse)
library(rwebstat)
library(lubridate)

webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'
# client_ID = webstat_client_ID
# tmo = rwebstat::w_data(dataset_name = "FM", series_name = "M.FR.EUR.FR2.MM.TMO.HSTA")

# base_url = "https://api.webstat.banque-france.fr/webstat-"
# 
# url_ = rwebstat:::make_url(dataset_name = "BSI1", format = "json",
#                     series_name = "M.FR.Y.V.L21.A.4.U6.2300.Z01.F")
# 
# req <- rwebstat:::get_data(url_, client_ID)

# BSI1.M.FR.Y.V.L21.A.4.U6.2300.Z01.F
# Contribution de M1 au taux de croissance annuel
# de la composante française à l'agrégat M3 zone euro CVS
M1 = rwebstat::w_data(dataset_name = "BSI1",
                      series_name = "M.FR.Y.V.L21.A.4.U6.2300.Z01.F")

# BSI1.M.FR.Y.V.L2A.M.4.U6.2300.Z01.F
# Contribution de M2-M1 au taux de croissance annuel
# de la composante française à l'agrégat M3 zone euro CVS
M2_M1 = rwebstat::w_data(dataset_name = "BSI1", series_name = "M.FR.Y.V.L2A.M.4.U6.2300.Z01.F")

# BSI1.M.FR.Y.V.LT3.L.4.U6.2300.Z01.F
# Contribution de M3-M2 au taux de croissance annuel
# de la composante française à l'agrégat M3 zone euro CVS
M3_M2 = rwebstat::w_data(dataset_name = "BSI1", series_name = "M.FR.Y.V.LT3.L.4.U6.2300.Z01.F")

# BSI1.M.FR.Y.V.LXG.X.4.U5.2300.Z01.F
# Contribution des engagements monétaires extérieurs nets au taux de croissance annuel
# de la composante française à l'agrégat M3 zone euro CVS
eng_ext = rwebstat::w_data(dataset_name = "BSI1", series_name = "M.FR.Y.V.LXG.X.4.U5.2300.Z01.F")

# BSI1.M.FR.Y.V.N30.X.4.U2.2300.Z01.F
# Contribution de M3 au taux de croissance annuel
# de la composante française à l'agrégat M3 zone euro CVS
tot = rwebstat::w_data(dataset_name = "BSI1", series_name = "M.FR.Y.V.N30.X.4.U2.2300.Z01.F")

date_start = today() %m-% months(38)

data = M1 %>% 
  left_join(M2_M1) %>% 
  left_join(M3_M2) %>% 
  left_join(eng_ext) %>% 
  pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
  drop_na() %>% 
  filter(date >= date_start) %>% 
  mutate(label = case_when(variable == "BSI1.M.FR.Y.V.L21.A.4.U6.2300.Z01.F" ~ "dépôts à vue M1",
                           variable == "BSI1.M.FR.Y.V.L2A.M.4.U6.2300.Z01.F" ~ "M2 - M1",
                           variable == "BSI1.M.FR.Y.V.LT3.L.4.U6.2300.Z01.F" ~ "M3 - M2",
                           variable == "BSI1.M.FR.Y.V.LXG.X.4.U5.2300.Z01.F" ~ "Engagements extérieurs")) 

ggplot(data, aes(x = date, y = value, fill = label)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Contribution à la variation de M3")



