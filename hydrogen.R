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
  "ggplot2"
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

cols_excluded <- c("Project name", "Technology Comments", "Announced Size", "Refs")

data_v1 <- data %>%
  select(headers[!(c(headers %in% cols_excluded))]) %>%
  mutate(Country = strsplit(Country, "\\r\\n")) %>%
  unnest(Country) %>%
  filter(!is.na(`Type of electricity (for electrolysis projects)`),
         !is.na(Country)) %>%
  mutate_if(is.character, as.factor)
levels(data_v1$`Type of electricity (for electrolysis projects)`) <- list(
  "Dedicated renewable" = "Dedicated renewable",
  "Grid" = "Grid",
  "Grid (excess renewable)" = "Grid (excess renewable)",
  "N/A" = "N/A",
  "Nuclear" = "Nuclear",
  "Other/Unknown" = "Other/unknown",
  "Other/Unknown" = "Other/Unknown"
)

data_v1 %>%
  group_by(`Type of electricity (for electrolysis projects)`) %>%
  summarize(n = n())

data_v2 <- data_v1 %>%
  filter(`Type of electricity (for electrolysis projects)` == "Dedicated renewable",
         !(`If dedicated renewables, type of renewable` %in% c("Unknown", NA))) %>%
  group_by(Country, `If dedicated renewables, type of renewable`) %>%
  summarize(n = n())

data_v3 <- data_v1 %>%
  select(Country, Technology, Product, starts_with("End use")) %>%
  pivot_longer(cols = -c("Country", "Technology", "Product"),
               names_to = "End use",
               names_transform = list("End use" = as.factor),
               values_to = "values") %>%
  mutate("End use" = gsub("End use_", "", `End use`))



