# generate recipe RMD files for website
make_script <- function(df){

  name <- df$recipe

  # file dir & name
  name_edit <- name %>%
    stringr::str_replace_all(" ", "-") %>%
    stringr::str_remove("-[(].*$") %>%
    stringr::str_to_lower()
  file_dir <- glue::glue("content/post/{name_edit}")
  dir.create(file_dir, showWarnings = FALSE)
  file_name <- glue::glue("content/post/{name_edit}/index.Rmd")

  # recipe pictures - format for markdown/html
  # pictures <<- df$pictures[[1]]
  pictures <- list.files(glue::glue("content/post/{name_edit}/"), pattern = "JPG")
  # file.copy(paste0("data/food/", pictures), file_dir) todo
  use_image <- pictures %>%
    paste0("![](", ., ')') %>%
    paste(collapse = "\n\n")
  use_image <- ifelse(length(pictures) == 0, "", use_image)

  # recipe youtube - format for markdown/html
  youtube <- df$youtube
  use_video <- ""
  if( !is.na(youtube) ) use_video <- paste0("[![youtube](http://img.youtube.com/vi/", youtube, "/0.jpg)](http://www.youtube.com/watch?v=", youtube, ")")

  # recipe script
  script <- c("---
title: \"", name, "\"
tags: ['", stringr::str_to_title(df$meal_type), "'", ifelse(is.na(df$tag), "", paste0(", ", df$tag)), "]
---

```{r, echo = FALSE, warning = FALSE}
library(tidyverse)
library(RSQLite)
library(knitr)

recipe_name <- '", name, "'
connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = '../../../data/recipes.db')
ingredients_query <- glue::glue('SELECT * FROM ingredients WHERE recipe = \"{recipe_name}\"')
instructions_query <- glue::glue('SELECT * FROM instructions WHERE recipe = \"{recipe_name}\"')
ingredients <- RSQLite::dbGetQuery(conn = connect, statement = ingredients_query)
instructions <- RSQLite::dbGetQuery(conn = connect, statement = instructions_query)
RSQLite::dbDisconnect(conn = connect)

display_ingredients <- ingredients %>%
  dplyr::mutate(idx = ifelse(ingredients == 'any', 1, 0)) %>%
  dplyr::arrange(idx, ingredients) %>%
  dplyr::group_by(recipe, type) %>%
  dplyr::mutate(n = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-idx) %>%
  tidyr::spread(type, ingredients)
add_cols <- purrr::discard(c('other', 'meat', 'veggie', 'fruit'), ~ .x %in% colnames(display_ingredients))
for(c in add_cols) display_ingredients[,c] <- NA
display_ingredients <- display_ingredients %>%
  dplyr::select(other, meat, veggie, fruit) %>%
  dplyr::mutate_all(~ ifelse(is.na(.x), '', .x)) %>%
  dplyr::rename(Other = other, Meat = meat, Veggie = veggie, Fruit = fruit)

display_instructions <- instructions %>%
  dplyr::arrange(idx) %>%
  dplyr::select(instructions)
```

", use_image,"

", use_video, "


### Ingredients

```{r, echo = FALSE}
display_ingredients %>% knitr::kable(format = \"html\", table.attr = \"class = \\\"presenttab\\\"\")
```

<br>

### Instructions

```{r, echo = FALSE}
display_instructions %>% knitr::kable(format = \"html\", table.attr = \"class = \\\"presenttabnoh\\\"\")
```
") %>% paste(collapse = "")

  # write to file
  write(script, file = file_name)
  return(script)
}

run_recipes <- function(){
  "Regenerate recipes files"

  connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "data/recipes.db")
  recipes <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM recipes")
  tags <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM tags") %>%
    dplyr::mutate(tags = paste0("'", tag, "'")) %>%
    dplyr::group_by(recipe) %>%
    dplyr::summarise(tag = paste(tags, collapse = ", "))
  recipes <- dplyr::left_join(recipes, tags, "recipe") %>%
    dplyr::arrange(recipe)
  RSQLite::dbDisconnect(conn = connect)

  # extract recipe pics todo
  recipe_info <- recipes
  # recipe_info$pictures <- apply(recipes, 1, function(r){
  #   pat <- r['recipe'] %>%
  #     stringr::str_replace(" \\(.*", "") %>%
  #     stringr::str_replace_all(" ", "_")
  #   pics <- list.files("data/food/", pattern = pat)
  #   stringr::str_subset(pics, glue::glue("^{pat}\\d*.JPG"))
  # })

  purrr::walk(1:nrow(recipe_info), ~ recipe_info %>% dplyr::slice(.x) %>% make_script())
}
