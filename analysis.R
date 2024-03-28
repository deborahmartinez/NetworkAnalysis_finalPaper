# load libraries ----------------------------------------------------------
library(dplyr)
library(magrittr)
library(tidyverse)
library(igraph)
library(highcharter)
library(viridis)
library(hrbrthemes)
library(ggsankey)
library(highcharter)

# data reading ------------------------------------------------------------
### World Trade Organization data: Bilateral imports
# link: https://stats.wto.org/ (you need to selec "Bilateral imports and download)
# in case of download from the WTO website, run the following commented lines (18-33),
# otherwise, decompress zip file and just run line 38, which is the filtered data

# bd <- read.csv("~/Desktop/WtoData_20240312173556.csv", encoding = "UTF-8") %>%  as_tibble()
# bd <- bd %>% janitor::clean_names()
# 
# bd %>%  select(-indicator_category)
# 
# bd <- bd %>%  filter(  partner_economy != "World" , 
#                        # year change
#                        year == 2020,
#                        product_sector == "Petroleum") %>% 
#   select(reporting_economy_iso3a_code, partner_economy_iso3a_code, reporting_economy, partner_economy, product_sector_classification,  year, value)  %>%  
#   left_join(atr, by= c("reporting_economy_iso3a_code" = "country_code")) %>% 
#   rename(rep_region = region, rep_income_group = income_group)%>% 
#   left_join(atr, by= c("partner_economy_iso3a_code" = "country_code")) %>% 
#   rename(par_region = region, par_income_group = income_group)%>% 
#   na.omit() %>% 
#   select(- reporting_economy_iso3a_code, -partner_economy_iso3a_code)

# Only run in case of sample
# samp <- sample(x = 1:2748, size = 100, replace = F) %>%  as_tibble()

bd <- read_csv("data/peroleum2020_imports.csv")

### World Bank metadata for region and income group
# link: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?view=chart
#  this csv comes with every dataset from the world bank,
# in case of download from the WB site, run the following commented lines,
# and adjust the directory
# otherwise, run line 49
# atr <- read_csv("~/Desktop/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_184/Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_184.csv")
# atr <- atr %>%  janitor::clean_names() %>% select(country_code, region, income_group)

atr <- read_csv("data/worldbank_metadata.csv")

# data wrangling ---------------------------------------------------------

aux <- bd %>%  
  # In case of filtering the region
  # filter(rep_region %in% c("North America", "Europe & Central Asia"),
  #        par_region %in% c("North America", "Europe & Central Asia") ) %>%
  mutate(    weight= value) %>% mutate(id = row_number()) %>% 
  # Just run in case of sample
  # inner_join(samp, by = c("id" = "value")) %>%
  select(partner_economy, reporting_economy, year, value, rep_region,                  
         rep_income_group, par_region, par_income_group  )


# sizes
aux_size <-aux %>%  select(name =partner_economy, income = par_income_group) %>% 
  bind_rows(aux %>%  select(name = reporting_economy, income = rep_income_group)) %>%  
  unique() %>% 
  mutate(size = case_when(income == "High income" ~5^2,
                          income == "Upper middle income" ~4^2,
                          income == "Lower middle income"~3^2,
                          income == "Low income" ~2^2))

aux_color <-aux %>%  select(names = partner_economy, region = par_region) %>% 
  bind_rows(aux %>%  select(names =reporting_economy, region = rep_region) ) %>%  
  unique() %>%
  mutate(color = case_when(region == "East Asia & Pacific" ~"#0077b6" ,       
                           region =="Europe & Central Asia"~"#00afb9" ,    
                           region =="Latin America & Caribbean" ~"#f3a712",
                           region =="Middle East & North Africa"~"#ed553b",
                           region =="North America"~"#7d3c98"  ,        
                           region =="South Asia"~"#5f9ea0" ,              
                           region =="Sub-Saharan Africa"~"#00a651" ) )

aux_edge <-  aux %>%  mutate(width =value %>%sqrt()/10000  ) %>% select(width) %>% 
  arrange(width)


# Network viz -------------------------------------------------------------

# .................... 
net<-graph.data.frame(aux)
net <- simplify(net, remove.multiple = T, remove.loops = T)

# network centrality & closeness
centr_degree(net, mode = "all")
degree.cent <- centr_degree(net, mode = "all")
degree.cent$res
closeness.cent <- closeness(net, mode="all")
closeness.cent
# .....................

node.size<-setNames(aux_size$size %>%  as_vector() , aux_size$name %>% as_vector())
node.color<-setNames(aux_color$color %>%  as_vector() , aux_color$names %>% as_vector())

# Arrangement selection
lo<- layout_nicely(net)
# lo<- layout_as_star(net)
lo<- layout_on_sphere(net)
# lo<- layout_in_circle(net)

#  Legends
plot(net,
     # vertex.label= NA,
     layout = lo,
     # layout= layout_randomly,
     edge.arrow.size=.2, 
     vertex.size=node.size,
     vertex.label.color="black",
     vertex.frame.color="transparent",
     vertex.color = node.color,
     # edge
     edge.curved=0.1,
     edge.width = aux_edge$width)

legend(
  "topleft",
  legend = c("East Asia & Pacific" ,"Europe & Central Asia" ,
             "Latin America & Caribbean" ,"Middle East & North Africa",
             "North America","South Asia","Sub-Saharan Africa"),
  pt.bg  = c("#0077b6", "#00afb9", "#f3a712", "#ed553b", "#7d3c98", "#5f9ea0", "#00a651"),
  pch    = 21,
  cex    = .6,
  pt.cex= 2.5,
  bty    = "n",
  title  = "Region"
)

# scaled <- 1 + ((2-1) * (size_vec - min(size_vec) ) / (  max(size_vec) - min(size_vec) ) )
legend('bottomleft',
       legend= c("High income" , 
                 "Upper middle income", 
                 "Lower middle income","Low income" ),
       pt.cex=c(4,3,2,1),
       cex    = .6,
       col='black',pch=21, 
       bty    = "n",
       pt.bg='white',
       title  = "Income group")


# Node degree centrality --------------------------------------------------

degree_centrality <-   net %>% degree() %>%  as.matrix() %>%
  as.tibble(rownames = "country") %>%  rename(conexiones = V1)

degree <- degree_centrality %>%  arrange(desc(conexiones)) %>% 
  left_join(aux %>%  select(partner_economy,income_group= par_income_group,region= par_region) %>% 
              unique(), 
            by = c("country"="partner_economy")) %>% 
  left_join(aux %>% filter(reporting_economy %in% c("Montenegro", "Madagascar")) %>% 
              select(reporting_economy,rep_income_group, rep_region) %>% 
              unique(), by = c("country"="reporting_economy")) %>% 
  mutate(income_group = case_when(is.na(income_group)~rep_income_group,
                                  T~income_group),
         region = case_when(is.na(region)~rep_region,
                            T~region)) %>%  
  select(-rep_income_group, -rep_region) 

sample_size <- degree %>% group_by(income_group) %>% summarize(num=n())

degree <- degree %>% left_join(sample_size) %>% 
  mutate(wighted_degree = conexiones/num,
         income = case_when(income_group == "High income"~4,
                            income_group == "Upper middle income"~3,
                            income_group == "Lower middle income"~2,
                            income_group == "Low income"~1)) 

# Correlaation
cor(degree$income, degree$conexiones)

# DC chart

degree %>% filter(conexiones!= 1) %>% 
  ggplot(aes(x =fct_reorder(country, conexiones), y = conexiones,
             color = income_group, group = income_group))+
  geom_point()+
  labs(x = "Countries", y = "Degree centrality", color = "Income group")+
  theme_minimal()+
  # scale_color_discrete(breaks=c("High income" , 
  #                               "Upper middle income", 
  #                               "Lower middle income","Low income"))+
  scale_color_manual(values = c("High income" = "#34450C", 
                                "Upper middle income" = "#567314", 
                                "Lower middle income" = "#8FB339",
                                "Low income" = "#C7D59F" ),
                     breaks = c("High income" , 
                                "Upper middle income",
                                "Lower middle income","Low income")) +
  theme( legend.position = "bottom",
         panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.text.x = element_text(angle = 45, size = 4)) 

# Degree centrality density distribution
ggplot(degree, aes(x=conexiones)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  labs(y = "Degree centrality density", x = "Degree centrality")

# Degree centrality by income group
degree %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(income_group, "\n", "n=", num)) %>%
  ggplot( aes(x=fct_reorder(myaxis, conexiones), y= conexiones, fill= income_group)) +
  geom_violin(width=1.4, alpha = .9, color = "transparent") +
  # geom_boxplot(width=0.1, color="white", outlier.colour = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  coord_flip()+
  labs(x = "Degree centrality", y = "Income group")+
  theme( panel.grid.major.y = element_blank(),
         legend.position="none"  ) 



# Trade value -------------------------------------------------------------
aux %>%  group_by(par_income_group, rep_income_group) %>% 
  summarise(n =sum(value))  %>% arrange(desc(n)) %>% 
  mutate(par_income_group = paste("Partner: ", par_income_group),
         rep_income_group = paste("Reporting: ", rep_income_group)) %>% 
  hchart(hcaes(to = rep_income_group, from =par_income_group, weight = n), 
         type = "sankey")

aux %>%  group_by(rep_income_group, par_income_group) %>% 
  summarise(n =sum(value)) %>%  mutate(n= n/sum(n))   %>% arrange(desc(n)) %>% 
  mutate(par_income_group = paste("Partner: ", par_income_group),
         rep_income_group = paste("Reporting: ", rep_income_group)) %>% 
  hchart(hcaes(to = rep_income_group, from =par_income_group, weight = n), 
         type = "sankey")

# ggplot( aes(x = 1, 
#              next_x = rep_income_group, 
#              node = n,
#              next_node = n,
#              fill = factor(rep_income_group))) +
# geom_sankey() +
# theme_sankey(base_size = 16)

aux %>%  group_by(rep_income_group, par_income_group) %>% 
  summarise(n =sum(value)) %>%  mutate(n=n/sum(n))  %>%
  mutate(rep_income_group = factor( rep_income_group,rev( c("High income" , 
                                                            "Upper middle income", 
                                                            "Lower middle income","Low income")) ),
         par_income_group = factor( par_income_group, rev( c("High income" , 
                                                             "Upper middle income", 
                                                             "Lower middle income",
                                                             "Low income")) )  )%>%
  ggplot( aes(fct_reorder(str_wrap(par_income_group, 10),n), 
              rep_income_group, fill= n)) + 
  geom_tile() +
  # labs(y = "Reporting country", x= "Partner", fill = "Edges")+
  geom_text(aes(label = round(n, 2)%>%  scales::comma() ), size=5) + 
  scale_fill_gradient(low="#E1EBE8", high="#0D5C63") +
  theme_ipsum()+
  scale_x_discrete(position = "top") +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank())



# Homophily ---------------------------------------------------------------


aux %>%  count(rep_income_group, par_income_group) %>%
  mutate(rep_income_group = factor( rep_income_group,rev( c("High income" , 
                                                            "Upper middle income", 
                                                            "Lower middle income","Low income")) ),
         par_income_group = factor( par_income_group, rev( c("High income" , 
                                                             "Upper middle income", 
                                                             "Lower middle income",
                                                             "Low income")) )  )%>% 
  ggplot( aes(fct_reorder(str_wrap(par_income_group, 10),n), 
              rep_income_group, fill= n)) + 
  geom_tile() +
  labs(y = "Reporting country", x= "Partner", fill = "Edges")+
  geom_text(aes(label = round(n, 2)%>%  scales::comma() ), size=5) + 
  scale_fill_gradient(low="#E1EBE8", high="#0D5C63") +
  theme_ipsum()+
  scale_x_discrete(position = "top") +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major  = element_blank())
