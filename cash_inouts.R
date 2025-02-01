library(tidyverse)
library(jsonlite)
library(httr)
library(extrafont)
library(extrafontdb)
library(showtext)
library(tidyjson)


font_add(
  "BPG SSP Crystal",
  regular = "C:/Windows/Fonts/BPG_SSP_Crystal.ttf",
  italic = "C:/Windows/Fonts/BPG_SSP_Crystal_Italic.ttf",
  bold = "C:/Windows/Fonts/BPG_SSP_Crystal_Bold.ttf",
  bolditalic = "C:/Windows/Fonts/BPG_SSP_Crystal_BoldItalic.ttf",
)

font_add(
  "BPG SSP Crystal Caps",
  regular = "C:/Windows/Fonts/BPG_SSP_Crystal_Caps.ttf",
  italic = "C:/Windows/Fonts/BPG_SSP_Crystal_Italic_Caps.ttf",
  bold = "C:/Windows/Fonts/BPG_SSP_Crystal_Bold_Caps.ttf",
  bolditalic = "C:/Windows/Fonts/BPG_SSP_Crystal_BoldItalic_Caps.ttf",
)

font_add(
  "Enciklopediuri _Uni MT",
  regular = "C:\\Windows\\Fonts\\Enciklopediuri _Uni MT.ttf",
  italic = "C:\\Windows\\Fonts\\Enciklopediuri _Uni MT Italic.ttf",
  bold = "C:\\Windows\\Fonts\\Enciklopediuri _Uni MT Bold.ttf",
  bolditalic = "C:\\Windows\\Fonts\\Enciklopediuri _Uni MT Bold Italic.ttf",
)

font_add(
  "Enciklopediuri _Uni Nus",
  regular = "C:\\Windows\\Fonts\\Enciklopediuri _Uni Nus.ttf",
  italic = "C:\\Windows\\Fonts\\Enciklopediuri _Uni Nus Italic.ttf",
  bold = "C:\\Windows\\Fonts\\Enciklopediuri _Uni Nus Bold.ttf",
  bolditalic = "C:\\Windows\\Fonts\\Enciklopediuri _Uni Nus Bold Italic.ttf",
)

## Setyup themes

theme_ds <- function () { 
  theme_minimal(base_size = 14) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(),
      title = element_blank(),
      # axis.text.x = element_blank(),
      # panel.grid = element_blank(),
      strip.text = element_text(family = "Enciklopediuri _Uni Nus", face="bold", size = 28),
      text = element_text(family= "Enciklopediuri _Uni Nus"),
      plot.title = element_text(size=28, face="bold", family="Enciklopediuri _Uni MT", hjust = 0),
      plot.subtitle = element_text(size=24, family="Enciklopediuri _Uni Nus", hjust=0),
      plot.caption = element_text(size=12, family="Enciklopediuri _Uni Nus", hjust=0),
      axis.text = element_text(size=18, family="Enciklopediuri _Uni Nus", color = "black"),
      legend.position = "none"
    )
}

### Read data

file_list <- dir("raw_json", pattern = "*.json", full.names = TRUE)

jsonlite::fromJSON(file_list[402]) |>
  tidyjson::as_tibble() |> 
  select(Id:BirthDate, DeclarationSubmitDate, InOuts)%>%
  # mutate(
  #   InOuts = map(InOuts, ~if(is.data.frame(.x)) .x else data.frame(.x))
  # ) |> 
  unnest_longer(col = InOuts) |> 
  tibble() |> View()
  unnest_wider(col = InOuts, names_sep = "_inout_") |> View()
  select(
    Id:DeclarationSubmitDate, InOuts_inout_InOutKind:InOuts_inout_RelationName
  ) |>
  mutate(
    income_type = "inout"
  ) |> 
  rename(
    "value" = "InOuts_inout_Amount",
    "currency" = "InOuts_inout_Currency",
    "income_date" = "DeclarationSubmitDate",
  ) -> inouts

# სასამართლო 347.json
# პარლამენტი 441.json
# პროკურატურა


  


content(req) |> write_json('parliament_apr18_2024.json')

parliament <- content(req)

