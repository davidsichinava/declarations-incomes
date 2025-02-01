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

### CPI

readxl::read_excel("datasets/samomxmareblo-fasebis-indeqsebi-2010-wlis-sashualo=100.xlsx", 1) |> 
  slice(3:n()) |> 
  set_names("year", paste0("m", 1:12)) |> 
  pivot_longer(-year, names_to = "month", values_to = "cpi") |> 
  mutate(
    cpi = as.numeric(cpi)
  ) |> 
  group_by(year) |> 
  summarize(
    cpi = mean(cpi, na.rm=T)/100
  ) -> cpi

## Download parliament

url <- "https://declaration.acb.gov.ge/Api/Declarations?&OrganisationIds=441"

req <- POST(url = url,
            add_headers(.headers =
                          c('User-Agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0',
                            'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
                            'Accept-Language' = 'en-CA,en-US;q=0.7,en;q=0.3',
                            'Accept-Encoding' = 'gzip, deflate, br',
                            'Content-Type' = 'application/json;charset=utf-8',
                            'Connection' = 'keep-alive',
                            'Upgrade-Insecure-Requests' = '1',
                            'Sec-Fetch-Dest' = 'document',
                            'Sec-Fetch-Mode' = 'navigate',
                            'Sec-Fetch-Site' = 'cross-site'
                          )))

content(req) |> write_json('parliament_apr18_2024.json')

parliament <-read_json("parliament_apr18_2024.json")

parliament <- content(req)

# Data processing

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |> 
  select(Id:BirthDate, DeclarationSubmitDate, InOuts) |>
  unnest_longer(col = InOuts) |> 
  tibble() |> 
  unnest_wider(col = InOuts, names_sep = "_inout_") |>
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


inouts |>
  mutate(
    currency = case_when(
      is.na(currency) ~ "GEL",
      currency == "FJD" ~ "CHF",
      T ~ as.character(currency)
    )
  ) |> 
  filter(currency != "GEL") |>
  select(currency, income_date) |> 
  distinct() |> 
  mutate(
    income_date = format(lubridate::dmy(income_date), "%Y-%m-%d")
  ) |> 
  filter(!is.na(income_date)) -> currencies

currencies |> 
  select(income_date) |> 
  distinct() |> unname() |> unlist() -> dates


get_historical_rates <- function(x){
  day_url <- paste0("https://nbg.gov.ge/gw/api/ct/monetarypolicy/currencies/ka/json/?date=", x)
  
  GET(url = day_url,
      add_headers(.headers =
                    c('User-Agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0',
                      'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
                      'Accept-Language' = 'en-CA,en-US;q=0.7,en;q=0.3',
                      'Accept-Encoding' = 'gzip, deflate, br',
                      'Content-Type' = 'application/json; charset=utf-8',
                      'Connection' = 'keep-alive',
                      'Upgrade-Insecure-Requests' = '1',
                      'Sec-Fetch-Dest' = 'document',
                      'Sec-Fetch-Mode' = 'navigate',
                      'Sec-Fetch-Site' = 'none'
                    ))) |> 
    content()
}

map(dates, get_historical_rates) -> exchange_rates


exchange_rates |> 
  tibble() |> 
  unnest_longer("exchange_rates") |> 
  unnest_longer("exchange_rates") |> 
  filter(exchange_rates_id == "currencies") |> 
  select(exchange_rates) |> 
  unnest_longer("exchange_rates") |> 
  unnest_wider("exchange_rates") |>
  mutate(
    validFromDate = format(as.Date(validFromDate),  "%Y-%m-%d")
  ) -> exchange_rates_processed


inouts |> 
  mutate(
    currency = case_when(
      is.na(currency) ~ "GEL",
      currency == "FJD" ~ "CHF",
      T ~ as.character(currency)
    )
  ) |> 
  mutate(
    income_date = format(lubridate::dmy(income_date), "%Y-%m-%d")
  ) |> 
  left_join(
    exchange_rates_processed |> select(code, validFromDate, rateFormated, quantity), by = c(
      "income_date" = "validFromDate",
      "currency" = "code"
    )
  ) |> 
  type_convert() |> 
  mutate(
    income = case_when(
      currency != "GEL" ~ rateFormated*value*quantity,
      T ~ as.double(value)
    )
  ) |>
  mutate(
    declaration_year = lubridate::year(lubridate::ymd(income_date))-1,
  ) -> inout_all

inout_all |> 
  filter(str_detect(InOuts_inout_InOutKind, "სწავ")) |> 
  filter(FirstName == "ნინო" & LastName == "წილოსანი") 

inout_all |> 
  filter(str_detect(InOuts_inout_InOutKind, "სწავ")) |>
  group_by(FirstName, LastName, declaration_year) |>
  summarize(
    value = sum(value, na.rm = T)
  ) |>
  ungroup() |> 
  group_by(declaration_year) |> 
  mutate(
    records = n()
  ) -> payment_education


parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |>
  select(Id:BirthDate, DeclarationSubmitDate, FamilyMembers) |>
  unnest_longer(col = FamilyMembers) |>
  tibble() |> 
  unnest_wider(col = FamilyMembers, names_sep = "_family_") |>
  mutate(
    FamilyMembers_family_BirthDate = format(lubridate::dmy(FamilyMembers_family_BirthDate), "%Y-%m-%d"),
    income_date = format(lubridate::dmy(DeclarationSubmitDate), "%Y-%m-%d"),
    declaration_year = lubridate::year(lubridate::ymd(income_date))-1,
  ) |>
  filter(
    FamilyMembers_family_Relationship == "შვილი"
  ) |>
  group_by(FirstName, LastName, declaration_year) |>
  summarize(
    children = n()
  ) |>
  ungroup() |> 
  group_by(declaration_year) |> 
  mutate(
    mps_with_children = n()
  ) |> 
  ungroup() |> 
  left_join(
    payment_education, by = c(
      "declaration_year",
      "FirstName",
      "LastName"
    )
  ) -> mps_with_children



comment_2023 <- data.frame(
  label = "2023 წელს, ",
  x = as.Date(c("2021-05-01")),
  y = 50000,
  color = "#172325",
  hjust = 1,
  halign = 1
)

children_plot_title <- paste0(
  "2023 წელს, 24-მა პარლამენტარმა შვილების განათლებაში სულ **<span style='color:#ca562c'>270 896 ლარი </span>** დახარჯა, რაც საშუალოდ, 11 287 ლარს შეადგენს."
)

round(1075.8159*12, 0)

children_plot_subtitle <- paste0(
  "2022 წელს, საქართველოში ოჯახის წლიური მედიანური შემოსავალი  **<span style='color:#ca562c'>12 910 ლარი </span>** იყო"
)

mps_with_children |> 
  group_by(declaration_year) |> 
  arrange(value) |>
  summarize(
    top_spender_first_name = first(FirstName[order(-value)]),  # Get first name of top spender
    top_spender_last_name = first(LastName[order(-value)]),  # Get last name of top spender
    top_spender_spent = first(value[order(-value)]),  # Get last name of top spender
    spent_on_education = sum(value, na.rm = T),
    records = first(na.omit(records)),
    mps_with_children = first(na.omit(mps_with_children)),
  ) |> 
  filter(declaration_year >= 2011) |>
  mutate(
    declaration_year = as.Date(lubridate::my(paste0("01-", declaration_year))),
    fill = 1
  ) |> 
  mutate(
    label = paste0(top_spender_first_name, " ", top_spender_last_name, ": ", format(top_spender_spent, big.mark = " ")),
    x = declaration_year,
    y = top_spender_spent*2,
    color = "#c95c3b", # rep("#c95c3b", 13),
    hjust = 0, #rep(0, 13)
    width = unit(3, "cm"),
    halign = 0 #rep(0, 13)
  ) |> 
  select(label, x, y, color, hjust, halign)-> top_spenders

annotate_top_spender = data.frame(
  label  = "ტოპ-მხარჯველი და თანხა, ლარი",
  x = as.Date(c("2023-01-01")),
  y = 60000,
  color = "black", # rep("#c95c3b", 13),
  hjust = 0.5, #rep(0, 13),
  halign = 0.5 #rep(0, 13)
)

annotate_all_edu = data.frame(
  label  = "განათლებაზე დახარჯული თანხა, სულ (ლარი)",
  x = as.Date(c("2023-01-01")),
  y = 400000,
  color = "black", # rep("#c95c3b", 13),
  hjust = 0.5, #rep(0, 13),
  halign = 0.5 #rep(0, 13)
)

mps_with_children |> View()
  group_by(declaration_year) |> 
  arrange(value) |>
  summarize(
    top_spender_first_name = first(FirstName[order(-value)]),  # Get first name of top spender
    top_spender_last_name = first(LastName[order(-value)]),  # Get last name of top spender
    top_spender_spent = first(value[order(-value)]),  # Get last name of top spender
    spent_on_education = sum(value, na.rm = T),
    records = first(na.omit(records)),
    mps_with_children = first(na.omit(mps_with_children)),
  ) |> 
  mutate(
    declaration_year = as.Date(lubridate::my(paste0("01-", declaration_year))),
    fill = 1
  ) |> 
  ggplot(aes(x = declaration_year, y = spent_on_education, label = format(round(spent_on_education, 0), big.mark = " "), color = as.factor(fill), size = 2)) +
  geom_point()+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_date(date_labels = "%Y", breaks = "1 year") +
  scale_color_manual(values = c("#3d5941"))+
  geom_text(position = position_dodge(width = 2), vjust = -0.4, family = "Enciklopediuri _Uni Nus")+
  ggtext::geom_textbox(
    data = top_spenders,
    aes(
      label = label,
    #  alpha = rep(0.5, 13),
      x = x,
      y = y,
      hjust = hjust,
      halign = halign
    ), family = "Enciklopediuri _Uni MT", size = 3, fill = NA, box.colour = NA, color="#c95c3b", inherit.aes = FALSE, fontface = "bold"
  ) +
  ggtext::geom_textbox(
    data = annotate_top_spender,
    aes(
      label = label,
      x = x,
      y = y,
      hjust = hjust,
      halign = halign
    ), family = "Enciklopediuri _Uni MT", size = 3, fill = NA, box.colour = NA, inherit.aes = FALSE, color = "#c95c3b"
  ) +
  ggtext::geom_textbox(
    data = annotate_all_edu,
    aes(
      label = label,
      x = x,
      y = y,
      hjust = hjust,
      halign = halign
    ), family = "Enciklopediuri _Uni MT", size = 3, fill = NA, box.colour = NA, inherit.aes = FALSE, color = "#3d5941"
  ) +
  geom_curve(
    aes(
      x = as.Date(c("2023-01-01")), y = 120000,
      xend = as.Date(c("2023-01-01")), yend = 80000,
    ),
    # arrow = arrow(length = unit(0.1, "cm")),
    curvature = list(0.15),
    size = 0.1,
    alpha = 0.5,
    color="#c95c3b"
  ) +
  geom_curve(
    aes(
      x = as.Date(c("2023-01-01")), y = 270895.6,
      xend = as.Date(c("2023-01-01")), yend = 380000,
    ),
    # arrow = arrow(length = unit(0.1, "cm")),
    curvature = list(0.15),
    size = 0.1,
    alpha = 0.5,
    color="#3d5941"
  ) +
  labs(
    title = children_plot_title,
    subtitle = children_plot_subtitle,
    caption = "\nწყარო: Declaration.gov.ge API; საქართველოს ეროვნული ბანკის API, საქსტატი (შინამეურნეობების ხარჯების და შემოსავლების კვლევა)"
  )+
  theme_ds()+
  coord_flip()+
  theme(
    plot.title = ggtext::element_textbox_simple(
      colour = "#172325", 
      size = rel(1.5), 
      face = "bold",
      lineheight = 1.2,
      margin = margin(0.5, 0, 1, 0, "lines")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      colour = "#172325", 
      size = rel(1.2),
      lineheight = 1.2,
      margin = margin(0.5, 0, 1, 0, "lines")
    )
  )

ggsave("children_education.pdf", width = 40, height = 25, units = "cm", device = cairo_pdf)

