
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
product <- read.csv("nike_product.csv")

# Cleaning the feature names with janitor
product <- clean_names(product)

# Format the price to appropriate level
product$listing_price <- product$listing_price/100
product$sale_price <- product$sale_price/100

# Filtering and saving in a variable the top 10 products by amount of reviews received
top_10_reviews <- product %>% 
  dplyr::slice_max(reviews, n = 10)


# Cleaning the image url link from regex (regular expressions) and keeping only the main url for the main look of each product
top_10_reviews$images <- sapply(top_10_reviews$images, FUN = function(x) {strsplit(x, split = '["]')[[1]][2]})

# Note: there are some products with NA on the image feature, we can fill them manually with case when

# Mutating a new Pic column with the product images to apply custom styles for the table

top_10_reviews <- top_10_reviews %>% 
  mutate(Pic=case_when(
    product_name =='Air Jordan 10 Retro'~"<img src='https://static.nike.com/a/images/t_PDP_1728_v1/ccsubyw6lzx10virtjdu/air-jordan-10-retro-shoe-f3jBkN.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =='Nike Zoom Fly'~"<img src='https://c.static-nike.com/a/images/t_PDP_1728_v1/x6jwtxaf3brhu6jisuf5/zoom-fly-running-shoe-OZEAxq.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =='Nike Air Monarch IV'~"<img src='https://static.nike.com/a/images/t_PDP_864_v1/f_auto,b_rgb:f5f5f5/qmmwhyelq5qegzvh8qu4/air-monarch-iv-mens-training-shoes-lPtRrS.png' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =='Nike Air Max 270'~"<img src='https://c.static-nike.com/a/images/t_PDP_1728_v1/awjogtdnqxniqqk0wpgf/air-max-270-shoe-2V5C4p.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =="Nike Air Force 1 '07"~"<img src='https://c.static-nike.com/a/images/t_PDP_1728_v1/oplkqwyf7nwnj98f8agj/air-force-1-07-shoe-PATZxx4V.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =='Nike Epic React Flyknit 2'~"<img src='https://static.nike.com/a/images/t_PDP_1728_v1/akktdiniehnrjqoibfaw/epic-react-flyknit-2-running-shoe-ShRZnm.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =='Nike Air Max 2017'~"<img src='https://static.nike.com/a/images/t_PDP_1728_v1/bbbwgncnxexhwgbz8qbp/air-max-2017-shoe-MkTmxxOd.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =='Nike React Infinity Run Flyknit'~"<img src='https://static.nike.com/a/images/t_PDP_1728_v1/ddb4c566-fcb0-47c0-8184-323ad9edff37/react-infinity-run-flyknit-running-shoe-ZjGHFz.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>",
    product_name =="Nike Air Force 1 '07"~"<img src='https://static.nike.com/a/images/t_PDP_1728_v1/i1-512bfa8a-01a0-4971-bd34-9cef18a159e0/air-force-1-07-shoe-AKTdww3y.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50%;height:80px;'>",
    product_name =='Nike Joyride Run Flyknit'~"<img src='https://static.nike.com/a/images/t_PDP_1728_v1/i1-b714d0a4-53ed-4919-9761-1bddc5dff48f/joyride-run-flyknit-running-shoe-sqfqGQ.jpg' style='border-radius: 11% 50% 11% 50% / 11% 50% 11% 50% ;height:80px;'>"
  ))


# Function for combining the words into a list for the gt table
combine_word <- function(Number, product_name, product_id){
  glue::glue(
    "<div style='line-height:16px; text-align:left'><span style='font-weight:bold; font-size:16px'>{Number}. </span><span style='font-weight:bold;font-size:13px'>{product_name}</span></div>
    <div><span style ='color:#2f2f2f;font-size:12px'>Product ID: {product_id}</span></div>"
  )
}


# Creation of the Table Report ------

tb_reviews <- top_10_reviews %>%
  # Add empty columns to fill with images and plots
  mutate(Pic = NA) %>%
  # Add number for ranking 
  mutate(Number=seq(1,10,1))%>%
  # Merge all info for each focus area description
  mutate(
    combo = combine_word(Number, product_name, product_id),
    combo = purrr::map(combo, gt::html)
  )%>%
  # Select and order columns of interest
  select(Pic, combo, -product_name, -product_id, reviews, rating, sale_price)%>%
  gt()%>%
  # Add pictures
  text_transform(
    locations = cells_body(columns=Pic),
    fn = function(x){
      purrr::map(
        top_10_reviews$Pic, gt::html
      )
    }
  )%>%
  # Add title and subtitle
  tab_header(
    title = "Top 10 Nike Products by Reviews",
    subtitle = "Summary information of the top 10 reviewed Nike products - 2020 product data"
  )%>%
  tab_footnote(
    footnote = md("Highlighted are products with a rating below 3 stars"),
    locations = cells_column_labels(columns = rating)
  )%>%
  # Add source note
  tab_source_note(
    source_note = md("**Data:** Product data from Nike - datahut.co | **Table:** Top 10 Reviewed Products")
  )%>%
  tab_source_note(
    source_note = glue("Last Update: { as.Date(as.POSIXlt(Sys.time())) }")  # Put automated date generation every time you recreate the table for version control
  )%>% 
  # Change column names
  cols_label(
    combo = "Product",
    Pic = md(""),
    reviews = md("Number of <br>Reviews"),
    rating = md("Rating"),
    sale_price = md("Price")
  )%>%
  fmt_currency(columns =  sale_price, currency = "EUR") %>% 
  # Style
  # Set columns size
  cols_width(
    combo ~ px(240),
    Pic ~ px(100),
    reviews ~ px(80),
    rating ~ px(80),
    sale_price ~ px(80)
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(everything())
  ) %>%
  # Title
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font=google_font(name = 'Bebas Neue'), 
        size='xx-large',weight='bold',align='left',
        color='#ba2b2b'
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
    style = list(cell_fill(color = "#f0f0f0")),
    locations = cells_body(rows = seq(1,nrow(top_10_reviews),2))
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
    locations = cells_column_labels(c(reviews, rating, sale_price))
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Anton"), 
        align = "left",v_align = "middle",size='small',color='black')),
    locations = cells_column_labels(combo)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = sale_price
    )
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#E68C79") # highlighting the low reviews row < 3
    ),
    locations = cells_body(rows = rating < 3.0)
  )%>%
  data_color(
    columns = rating ,
    colors = scales::col_numeric(
      palette = 'RdYlGn',
      reverse = FALSE,
      domain = c(1.0, 5.0))
  ) %>%
  # General table options
  tab_options(
    data_row.padding = px(0),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.style = "hidden",
    table_body.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    row_group.font.size = px(19),                # Row group labels - font size
    row_group.font.weight = "800",               # Row group labels - font weight
    row_group.border.top.style = "solid",
    source_notes.border.bottom.style = "solid",
  ) 

# gtsave(tb_reviews, "Nike_Reviews_table.html")          # generate html report of the table
# gtsave(tb_reviews, 'Nike_Reviews_table.pdf', zoom = 1) # generate pdf report of the table
# gtsave(tb_reviews, "Nike_Reviews_table.png")           # generate png report of the table
tb_reviews