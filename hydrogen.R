##################################
# HYDROGEN PROJECTS DATABASE
# Laura A.
##################################

main_dir <- getwd()
dir.create("Elab")
res_dir_path <- paste0(main_dir, "/", "Elab")

# 0. INSTALL - LOAD LIBRARIES
#----------------------------
libs <- c(
  "readxl", "tidyverse", "dplyr",
  "ggplot2", "maps", "sp", "maptools"
)
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)){
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = T))

# 1. READ DATA
#-------------
head1 <- readxl::read_excel("Hydrogen projects database public version.xlsx",
                            sheet = "Projects", skip = 1, col_names = T) %>%
  names() %>%
  str_replace("\\..\\d*", NA_character_)
head2 <- readxl::read_excel("Hydrogen projects database public version.xlsx",
                            sheet = "Projects", skip = 2, col_names = T) %>%
  names() %>%
  str_replace("\\..\\d*", NA_character_)

for (n in seq_len(length(head1))) {
  if (is.na(head2[n])) {
    head2[n] <- head1[n]
    head1[n] <- NA_character_
  }
}

head1 <- tibble(head1) %>% 
  mutate(head1 = zoo::na.locf0(head1)) %>% pull()
head1[c(26, 31, 32)] <- NA

headers <- map_chr(seq_len(length(head1)), ~{
                     case_when(
                       !is.na(head1[.x]) & !is.na(head2[.x]) ~ paste(head1[.x], head2[.x], sep = "_"),
                       TRUE ~ head2[.x]
                     )
                   })

data <- readxl::read_excel("Hydrogen projects database public version.xlsx", 
                           sheet = "Projects", skip = 4, col_names = headers)

# 2. PREPARE DATA
#----------------
cols_excluded <- c("Project name", "Technology Comments", "Announced Size", "Refs")

data_v1 <- data %>%
  select(headers[!(c(headers %in% cols_excluded))]) %>%
  mutate(Country = strsplit(Country, "\\r\\n")) %>%
  unnest(Country) %>%
  filter(!is.na(`Type of electricity (for electrolysis projects)`),
         !is.na(Country),
         `Date online` > 1900) %>%
  mutate_if(is.character, as.factor)
levels(data_v1$`Type of electricity (for electrolysis projects)`) <- list(
  "Dedicated renewable" = "Dedicated renewable",
  "Grid" = "Grid",
  "Grid (excess renewable)" = "Grid (excess renewable)",
  "Other/Unknown" = "N/A",
  "Nuclear" = "Nuclear",
  "Other/Unknown" = "Other/unknown",
  "Other/Unknown" = "Other/Unknown"
)
colnames(data_v1)[7:8] <- c("Type_electricity", "Type_renewable")

# 3. QUESTIONS
#-------------

## Most used technologies
data_v1 %>%
  group_by(Type_electricity) %>%
  summarize(n = n()) %>%
  ggplot(aes(x =Type_electricity, y = n)) +
    geom_bar(stat = "identity")

data_v1 %>%
  group_by(Technology) %>%
  summarize(n = n()) %>%
  ggplot(aes(x =Technology, y = n)) +
  geom_bar(stat = "identity")

data_v1 %>%
  group_by(Status) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Status, y = n)) +
  geom_bar(stat = "identity")

# What technologies are being developed most
data_v2 <- data_v1 %>%
  filter(!(Type_renewable %in% c("Unknown", NA))) %>%
  mutate(Type_electricity = as.factor(ifelse(Type_electricity == "Dedicated renewable", as.character(Type_renewable), as.character(Type_electricity)))) %>%
  group_by(Technology, Type_electricity) %>%
  summarize(n = n())

levels(data_v2$Technology) <- list(
  "Alkaline electrolysis" = "ALK",
  "Biomass" = "Biomass",
  "Biomass" = "Biomass w CCUS",
  "Fossil Fuel & CCUS" = "Coal w CCUS",
  "Fossil Fuel & CCUS" = "NG w CCUS",
  "Fossil Fuel & CCUS" = "Oil w CCUS",
  "Other" = "Other",
  "Other Electrolysis" = "Other Electrolysis",
  "PEM" = "PEM",
  "SOEC" = "SOEC"
  )

## Sankey plot
nodes <- data.frame(
  name = c(as.character(data_v2$Type_electricity), 
           as.character(data_v2$Technology))
) %>% unique()

data_v2$IDsource = match(data_v2$Type_electricity, nodes$name) - 1
data_v2$IDtarget = match(data_v2$Technology, nodes$name) -1

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

sankeyNetwork(Links = data_v2, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=12, nodePadding=5)

## Bubble map
data_v3 <- data_v1 %>%
  filter(!(Type_renewable %in% c("Unknown", NA))) %>%
  mutate(Type_electricity = as.factor(ifelse(Type_electricity == "Dedicated renewable", as.character(Type_renewable), as.character(Type_electricity)))) %>%
  group_by(Country, Type_electricity) %>%
  summarize(n = n())

world <- map('world', fill = T, col = "transparent", plot = F)
IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
world_sp <- map2SpatialPolygons(world, IDs=IDs,
                                proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
world.label <- data.frame(
  country = names(world_sp),
  coordinates(world_sp)) %>%
  mutate(country = case_when(
    country == "China" ~"People's Republic of China",
    country == "Czech Republic" ~"Czechia",
    country == "Russia" ~"Russian Federation",
    country == "Slovakia" ~"Slovak Republic",
    country == "USA" ~"United States",
    TRUE ~country
  ))
names(world.label) <- c("country", "Long", "Lat")





countries <- readxl::read_excel("Hydrogen projects database public version.xlsx", 
                                sheet = "Countries") %>%
  left_join(y = world, by = c("Country" = "region"))

data_v3 <- left_join(x = data_v3, y = countries,
                     by = c("Country" = "ISO-3 Code")) %>%
  arrange(desc(n))


ggplot() +
  geom_polygon(data = countries, aes(x = long, 
                                     y = lat, 
                                     group = group),
               fill = "grey") +
  geom_point(data = data_v3, aes(x = long, 
                                 y = lat, 
                                 size = n, 
                                 color = Type_electricity)) +
  scale_size_continuous() +
  scale_color_viridis_c() +
  theme_void() +
  coord_map(xlim = c(-180, 180))

#End uses categorized by country
data_v3 <- data_v1 %>%
  select(Country, Technology, Product, starts_with("End use")) %>%
  pivot_longer(cols = -c("Country", "Technology", "Product"),
               names_to = "End use",
               names_transform = list("End use" = as.factor),
               values_to = "values") %>%
  mutate("End use" = gsub("End use_", "", `End use`)) %>%
  filter(!is.na(values)) %>%
  group_by(Country, Technology, Product, `End use`) %>%
  summarize(Total = sum(values, na.rm = T))

# How much capacity is asset to each use
data_v4 <- data_v1 %>%
  select(`Project name`, Country, Status, Technology, `Normalised capacity_MWel`)


