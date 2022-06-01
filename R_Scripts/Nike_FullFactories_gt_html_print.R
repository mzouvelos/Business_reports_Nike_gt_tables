
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
# Load data
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





# Load Flag images
factory <- factory %>%
  mutate(flag = case_when(
    str_detect(country,'Brazil') ~ 'https://hatscripts.github.io/circle-flags/flags/br.svg',
    str_detect(country,'China') ~ 'https://hatscripts.github.io/circle-flags/flags/cn.svg',
    str_detect(country,'Italy') ~ 'https://hatscripts.github.io/circle-flags/flags/it.svg',
    str_detect(country,'Vietnam') ~ 'https://hatscripts.github.io/circle-flags/flags/vn.svg',
    str_detect(country,'Georgia') ~ 'https://hatscripts.github.io/circle-flags/flags/ga.svg',
    str_detect(country,'Sri Lanka') ~ 'https://hatscripts.github.io/circle-flags/flags/lk.svg',
    str_detect(country,'Pakistan') ~ 'https://hatscripts.github.io/circle-flags/flags/pk.svg',
    str_detect(country,'Usa') ~ 'https://hatscripts.github.io/circle-flags/flags/us.svg',
    str_detect(country,'Guatemala') ~ 'https://hatscripts.github.io/circle-flags/flags/gt.svg',
    str_detect(country,'Argentina') ~ 'https://hatscripts.github.io/circle-flags/flags/ar.svg',
    str_detect(country,'Nicaragua') ~ 'https://hatscripts.github.io/circle-flags/flags/ni.svg',
    str_detect(country,'Moldova') ~ 'https://hatscripts.github.io/circle-flags/flags/md.svg',
    str_detect(country,'Japan') ~ 'https://hatscripts.github.io/circle-flags/flags/jp.svg',
    str_detect(country,'Netherlands') ~ 'https://hatscripts.github.io/circle-flags/flags/nl.svg',
    str_detect(country,'South Korea') ~ 'https://hatscripts.github.io/circle-flags/flags/kr.svg',
    str_detect(country,'Mexico') ~ 'https://hatscripts.github.io/circle-flags/flags/mx.svg',
    str_detect(country,'Indonesia') ~ 'https://hatscripts.github.io/circle-flags/flags/id.svg',
    str_detect(country,'Taiwan') ~ 'https://hatscripts.github.io/circle-flags/flags/tw.svg',
    str_detect(country,'Cambodia') ~ 'https://hatscripts.github.io/circle-flags/flags/kh.svg',
    str_detect(country,'El Salvador') ~ 'https://hatscripts.github.io/circle-flags/flags/sv.svg',
    str_detect(country,'Turkey') ~ 'https://hatscripts.github.io/circle-flags/flags/tr.svg',
    str_detect(country,'Israel') ~ 'https://hatscripts.github.io/circle-flags/flags/il.svg',
    str_detect(country,'Bulgaria') ~ 'https://hatscripts.github.io/circle-flags/flags/bg.svg',
    str_detect(country,'Bangladesh') ~ 'https://hatscripts.github.io/circle-flags/flags/bd.svg',
    str_detect(country,'Canada') ~ 'https://hatscripts.github.io/circle-flags/flags/ca.svg',
    str_detect(country,'India') ~ 'https://hatscripts.github.io/circle-flags/flags/in.svg',
    str_detect(country,'Indonesia') ~ 'https://hatscripts.github.io/circle-flags/flags/id.svg',
    str_detect(country,'Honduras') ~ 'https://hatscripts.github.io/circle-flags/flags/hn.svg',
    str_detect(country,'Thailand') ~ 'https://hatscripts.github.io/circle-flags/flags/th.svg',
    str_detect(country,'Croatia') ~ 'https://hatscripts.github.io/circle-flags/flags/hr.svg',
    str_detect(country,'Ecuador') ~ 'https://hatscripts.github.io/circle-flags/flags/ec.svg',
    str_detect(country,'Germany') ~ 'https://hatscripts.github.io/circle-flags/flags/de.svg',
    str_detect(country,'Spain') ~ 'https://hatscripts.github.io/circle-flags/flags/es.svg',
    str_detect(country,'United Kingdom') ~ 'https://hatscripts.github.io/circle-flags/flags/uk.svg',
    str_detect(country,'Greece') ~ 'https://hatscripts.github.io/circle-flags/flags/gr.svg',
    str_detect(country,'Malaysia') ~ 'https://hatscripts.github.io/circle-flags/flags/my.svg',
    str_detect(country,'Egypt') ~ 'https://hatscripts.github.io/circle-flags/flags/eg.svg',
    str_detect(country,'Poland') ~ 'https://hatscripts.github.io/circle-flags/flags/pl.svg',
    str_detect(country,'Jordan') ~ 'https://hatscripts.github.io/circle-flags/flags/jo.svg',
    str_detect(country,'Australia') ~ 'https://hatscripts.github.io/circle-flags/flags/au.svg',
    str_detect(country,'Bosnia') ~ 'https://hatscripts.github.io/circle-flags/flags/ba.svg',
    str_detect(country,'France') ~ 'https://hatscripts.github.io/circle-flags/flags/fr.svg',
    str_detect(country,'South Africa') ~ 'https://hatscripts.github.io/circle-flags/flags/za.svg',
    
    
    
  ))


factory <- factory %>% 
  # Merge all info for each Factory
  mutate(
    combo = combine_word(factory_name, nike_inc_brand_s, factory_type, product_type),
    combo = purrr::map(combo, gt::html)
  )%>%
  # Select and order columns of interest
  select(combo, -factory_name, -nike_inc_brand_s, -factory_type, -product_type, flag, supplier_group, total_workers, line_workers_perc)




# The gt table
factory_gt <-
  factory %>%
  gt()%>%
  opt_all_caps() %>%
  opt_table_font(font = google_font(name = "IBM Plex Sans")) %>%
  
  # Turn line_workers percentage into bar
  gt_plt_bar_pct(column = line_workers_perc, scaled = TRUE,
                 fill = "#ea553b", background = "#f7d8d2") %>% 
  
  # Highlight one row each two rows
  # to improve readability
  tab_style(
    style = list(cell_fill(color = "#f2f2fc")),
    locations = cells_body(rows = seq(1,nrow(factory),2))
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
  data_color(
    columns =  total_workers,
    colors = scales::col_numeric(
      palette = 'Blues',
      reverse = FALSE,
      domain = c(0, 25000))
  ) %>% 
  # Style
  # Set columns size
  cols_width(
    combo ~ px(300),
    flag ~ px(100),
    supplier_group ~ px(150),
    total_workers ~ px(100),
    line_workers_perc ~ px(150),
    everything() ~ px(75)) %>% 
  cols_align(
    align = c("center"),
    columns = c(total_workers, line_workers_perc, supplier_group)
  ) %>%
  # General table options
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.style = "hidden",
    table_body.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    row_group.font.size = px(19),                # Row group labels - font size
    row_group.font.weight = "800",               # Row group labels - font weight
    row_group.border.top.style = "solid",
    source_notes.border.bottom.style = "solid",
  ) %>%
  gtExtras::gt_img_rows(columns = flag, img_source = "web", height = 40) %>% 
  opt_css(
    css = "
    #pubs-table a {
      color: DarkCyan;
      text-underline-position: under;
    }
    "
  )





# The gt table top part
factory_gt_topp <-
  factory[0, ] %>%
  gt() %>%
  # Change column names
  cols_label(
    combo = "Factories",
    flag = "",
    supplier_group = md("Supplier"),
    total_workers = md("Total Workers"),
    line_workers_perc = md("Percentage % of <br>Line workers"),
  )%>%
  opt_all_caps() %>%
  opt_table_font(font = google_font(name = "IBM Plex Sans")) %>%
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
  ) %>% 
  cols_width(
    combo ~ px(300),
    flag ~ px(100),
    supplier_group ~ px(150),
    total_workers ~ px(100),
    line_workers_perc ~ px(150),
    everything() ~ px(75)) %>% 
  cols_align(
    align = c("center"),
    columns = c(total_workers, line_workers_perc, supplier_group)) 




library(htmltools)

table_gt_layout <-
  htmltools::div(
    # This div has the header and the column labels
    htmltools::tags$div(
      factory_gt_topp %>%
        # Add title and subtitle
        # Add title and subtitle
        tab_header(
          title = "Nike Factories information",
          subtitle = "Summary information of Nike factories - 2018 MFG data"
        )%>%
        opt_align_table_header(align = "left") %>%
        as_raw_html()
    ),
    # This div has the table body (hiding the column labels)
    htmltools::tags$div(
      factory_gt %>%
        tab_options(column_labels.hidden = TRUE) %>%
        as_raw_html(inline_css = FALSE),
      style = htmltools::css(
        `overflow-x` = "hidden",
        `overflow-y` = "auto",
        height = "500px"
      )
    ),
    # This div has the table footer (reuses the heading code and adds source notes)
    htmltools::tags$div(
      factory_gt_topp %>%
        tab_options(column_labels.hidden = TRUE) %>%
        tab_source_note(
          source_note = md("")
        )%>%
        # Add source note
        tab_source_note(
          source_note = md("**Data:** 2018/W36: The Nike Manufacturing Map - data.world | **Table:** Nike factories")
        )%>%
        tab_source_note(
          source_note = glue("Last Updated: { as.Date(as.POSIXlt(Sys.time())) }")  # Put automated date generation every time you recreate the table for version control
        )%>% 
        as_raw_html(inline_css = FALSE)
    ),
    style = htmltools::css(
      `padding` = "10px",
      border = "solid",
      `border-color` = gt::adjust_luminance("DarkTurquoise", steps = -0.5),
      `border-width` = "2px",
      `background-color` = gt::adjust_luminance("Snow", steps = -1)
    )
  )

# Preview the finalized table
table_gt_layout %>% html_print()
