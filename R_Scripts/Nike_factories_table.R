
# Clear the Rstudio environment (memory management)
rm(list = ls())

# Cleaning from garbage memory
gc()

# Loading the pacman library
if (!require(pacman)) install.packages('pacman')
library(pacman)
# Importing the required Packages using pacman
pacman::p_load(tidyverse, vroom, xts, glue, visdat, naniar, rmarkdown, dlookr, DT, gt, lubridate, scales, webshot, htmlwidgets, tidytext, reshape2, textdata, readr, data.table, xlsx, parallel, janitor)

# Nike color pallete

#    #000000	- Black
#    #ffffff	- White
#    #005147	- Dark Green
#    #ba2b2b	- Red
#    #ea553b	- Deep Orange

# Loading the data
setwd("C:/...") # set your working directory
factory <- readxl::read_excel("nike_factory.xlsx")

# Cleaning the feature names with janitor
factory <- clean_names(factory)

# Drop the factories that have no information about total workers and line workers
factory <- factory %>% drop_na(c(total_workers, line_workers))

# Transform names from full uppercase to only having the first letter of each word as uppercase and rest lowercase
factory$factory_name <- tolower(factory$factory_name) %>% str_to_title(factory$factory_name)

factory$factory_type <- tolower(factory$factory_type) %>% str_to_title(factory$factory_type)

factory$product_type <- tolower(factory$product_type) %>% str_to_title(factory$product_type)

factory$supplier_group <- tolower(factory$supplier_group) %>% str_to_title(factory$supplier_group)

factory$city <- tolower(factory$city) %>% str_to_title(factory$city)

factory$country <- tolower(factory$country) %>% str_to_title(factory$country)

factory$line_workers_perc <- round((factory$line_workers/factory$total_workers),3)*100



# Select the top 4 
factory_female_perc_top <- factory %>% select(factory_name, factory_type, product_type, nike_inc_brand_s, supplier_group, address, city, country, total_workers, line_workers, percent_female_workers) %>% 
  arrange(-percent_female_workers) %>% 
  dplyr::slice_max(percent_female_workers, n = 4)

factory_female_perc_top$Group <- "Factories with Highest Female Workers %"

# Select the bottom 4 
factory_female_perc_bottom <- factory %>% select(factory_name, factory_type, product_type, nike_inc_brand_s, supplier_group, address, city, country, total_workers, line_workers, percent_female_workers) %>% 
  arrange(-percent_female_workers) %>% 
  dplyr::slice_min(percent_female_workers, n = 4)

factory_female_perc_bottom$Group <- "Factories with Lowest Female Workers %"

# Merge the top and bottom frame
factory_female_perc <- rbind.data.frame(factory_female_perc_top, factory_female_perc_bottom)

# Load Flag images
factory_female_perc <- factory_female_perc %>%
  mutate(flag = case_when(
    str_detect(country,'Brazil') ~ 'https://hatscripts.github.io/circle-flags/flags/br.svg',
    str_detect(country,'China') ~ 'https://hatscripts.github.io/circle-flags/flags/cn.svg',
    str_detect(country,'Italy') ~ 'https://hatscripts.github.io/circle-flags/flags/it.svg',
    str_detect(country,'Vietnam') ~ 'https://hatscripts.github.io/circle-flags/flags/vn.svg',
    str_detect(country,'Georgia') ~ 'https://hatscripts.github.io/circle-flags/flags/ga.svg',
    str_detect(country,'Sri Lanka') ~ 'https://hatscripts.github.io/circle-flags/flags/lk.svg',
    str_detect(country,'Pakistan') ~ 'https://hatscripts.github.io/circle-flags/flags/pk.svg',
    
  ))

# Custom function to combine the text
combine_word <- function(factory_name, nike_inc_brand_s, factory_type, product_type){
  glue::glue(
    "<div style='line-height:16px; text-align:left'><span style='font-weight:bold;font-size:14px'>{factory_name}</span><span style='font-size:13px'> ({nike_inc_brand_s})</span></div>
     <div style='line-height:14px; text-align:left'><span style ='color:#2f2f2f;font-size:12px'>Factory Type: {factory_type}</span></div>
    <div style='line-height:14px; text-align:left'><span style ='color:#2f2f2f;font-size:12px'>Product Type: {product_type}</span></div>"
  )
}

# Correcting some foreign character names
factory_female_perc[factory_female_perc == "Calçados Hammes Ltda."] <- "Calcados Hammes Ltda."	
factory_female_perc[factory_female_perc == "Thomas E Goergen Atelier De Calçados Ltda Me"] <- "Thomas E Goergen Atelier De Calcados Ltda Me"

# Creating percentage for line workeers and formatting it
factory_female_perc$line_workers_perc <- round((factory_female_perc$line_workers/factory_female_perc$total_workers),3)*100


# Creation of the Table Report ------

tb_factory_female_perc <- factory_female_perc %>%
  # Merge all info for each Factory
  mutate(
    combo = combine_word(factory_name, nike_inc_brand_s, factory_type, product_type),
    combo = purrr::map(combo, gt::html)
  )%>%
  # Select and order columns of interest
  select(combo, -factory_name, -nike_inc_brand_s, -factory_type, -product_type, flag, supplier_group, total_workers, line_workers_perc, Group)%>%
  group_by(Group) %>% 
  gt(groupname_col = "Group")%>%
  # Add title and subtitle
  tab_header(
    title = "Highest and Lowest Nike Factories by Female Workers Percentage",
    subtitle = "Summary information of the top and bottom Nike factories by female workers proportion - 2018 MFG data"
  )%>%
  tab_source_note(
    source_note = md("")
  )%>%
  # Add source note
  tab_source_note(
    source_note = md("**Data:** 2018/W36: The Nike Manufacturing Map - data.world | **Table:** Nike factories by female percentage")
  )%>%
  tab_source_note(
    source_note = glue("Last Updated: { as.Date(as.POSIXlt(Sys.time())) }")  # Put automated date generation every time you recreate the table for version control
  )%>% 
  # Change column names
  cols_label(
    combo = "Factories",
    flag = "",
    supplier_group = md("Supplier"),
    total_workers = md("Total Workers"),
    line_workers_perc = md("Percentage % of <br>Line workers"),
  )%>%
  # Style
  # Set columns size
  cols_width(
    combo ~ px(300),
    flag ~ px(100),
    supplier_group ~ px(150),
    total_workers ~ px(100),
    line_workers_perc ~ px(150)
    
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(everything())
  ) %>%
  # Turn line_workers percentage into bar
  gt_plt_bar_pct(column = line_workers_perc, scaled = TRUE,
                 fill = "#ea553b", background = "#f7d8d2") %>% 
  # Title
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font=google_font(name = 'Bebas Neue'), 
        size='xx-large',weight='bold',align='left',
        color='#005147'
      )))%>%
  # Subtitle
  tab_style(
    locations = cells_title(groups = 'subtitle'),
    style = list(
      cell_text(
        font=google_font(name = 'Oswald'), 
        size='medium',align='left'
      )))%>%
  #Apply new style to all column headers
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)))) %>% 
  # Footnote
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ),style = "italic",size='small')),
    locations = cells_footnotes()
  )%>%
  # Source note
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Oswald"
      ),size= 'small')),
    locations = cells_source_notes()
  )%>%
  # Highlight one row each two rows
  # to improve readability
  tab_style(
    style = list(cell_fill(color = "#f2f2fc")),
    locations = cells_body(rows = seq(1,nrow(factory_female_perc),2))
  ) %>%
  #Apply new style to all column headers
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)))) %>% 
  # Header
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Anton"), 
        align = "center",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(c(total_workers, line_workers_perc, supplier_group))
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Anton"), 
        align = "left",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(combo)
  )%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(total_workers, supplier_group)
    )
  ) %>%
  data_color(
    columns =  total_workers,
    colors = scales::col_numeric(
      palette = 'Blues',
      reverse = FALSE,
      domain = c(0, 25000))
  ) %>% 
  # General table options
  tab_options(
    # data_row.padding = px(0),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.style = "hidden",
    table_body.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    row_group.font.size = px(16),                # Row group labels - font size
    row_group.font.weight = "800",               # Row group labels - font weight
    row_group.border.top.style = "solid",
    row_group.border.bottom.style = "solid",
    source_notes.border.bottom.style = "solid",
  ) %>% 
  gtExtras::gt_img_rows(columns = flag, img_source = "web", height = 40)

# gtsave(tb_factory_female_perc, "Nike_Factories_table.html")          # generate html report of the table
# gtsave(tb_factory_female_perc, 'Nike_Factories_table.pdf', zoom = 1) # generate pdf report of the table
# gtsave(tb_factory_female_perc, "Nike_Factories_table.png")           # generate png report of the table
tb_factory_female_perc
