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
      strip.text = element_text(family = "BPG SSP Crystal", face="bold", size = 28),
      text = element_text(family= "BPG SSP Crystal"),
      plot.title = element_text(size=28, face="bold", family="BPG SSP Crystal Caps", hjust = 0),
      plot.subtitle = element_text(size=24, family="BPG SSP Crystal", hjust=0),
      plot.caption = element_text(size=12, family="BPG SSP Crystal", hjust=0),
      axis.text = element_text(size=18, family="BPG SSP Crystal", color = "black"),
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

content(req) |> write_json('parliament_sep22_09.json')

parliament <- content(req)

# Data processing

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |> 
  select(Id:BirthDate, DeclarationSubmitDate, Jobs) |>
  unnest_longer(col = Jobs) |> 
  tibble() |> 
  unnest_wider(col = Jobs, names_sep = "_job_") |>
  select(
    Id:DeclarationSubmitDate, Jobs_job_Amount, Jobs_job_Currency, Jobs_job_EndDate
  ) |> 
  mutate(
    income_type = "salary"
  ) |> 
  rename(
    "value" = "Jobs_job_Amount",
    "currency" = "Jobs_job_Currency",
    "income_date" = "Jobs_job_EndDate"
  ) |> 
  mutate(
    income_date = case_when(
      income_date == "" ~ DeclarationSubmitDate,
      T ~ as.character(income_date)
    )
  )-> household_income

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |>
  select(Id:BirthDate, DeclarationSubmitDate, Securities) |> 
  unnest_longer(col = Securities) |> 
  tibble() |> 
  unnest_wider(col = Securities, names_sep = "_sec_") |> 
  select(
    Id:DeclarationSubmitDate, Securities_sec_Amount, Securities_sec_AmountCurrency, Securities_sec_PurchaseYear
  )|>
  mutate(
    income_type = "securities"
  ) |> 
  rename(
    "value" = "Securities_sec_Amount",
    "currency" = "Securities_sec_AmountCurrency",
    "income_date" = "Securities_sec_PurchaseYear"
  ) |> 
  mutate(
    income_date = paste0("31.12.", income_date)
  )-> securities

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |> 
  select(Id:BirthDate, DeclarationSubmitDate, Cashes) |> 
  unnest_longer(col = Cashes) |> 
  tibble() |> 
  unnest_wider(col = Cashes, names_sep = "_cash_") |>
  select(Id:Cashes_cash_Currency) |> 
  mutate(
    income_type = "cash_income"
  ) |> 
  rename(
    "value" = "Cashes_cash_Amount",
    "currency" = "Cashes_cash_Currency", # Add declaration year
  ) |> 
  mutate(
    income_date = paste0("31.12.", lubridate::year(lubridate::dmy(DeclarationSubmitDate))-1)
  ) -> cash

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |>
  select(Id:BirthDate, DeclarationSubmitDate, Gifts) |> 
  unnest_longer(col = Gifts) |> 
  tibble() |> 
  unnest_wider(col = Gifts, names_sep = "_gift_") |> 
  select(
    Id:DeclarationSubmitDate, Gifts_gift_Amount, Gifts_gift_Currency
  ) |> 
  mutate(
    income_type = "gift"
  ) |> 
  rename(
    "value" = "Gifts_gift_Amount",
    "currency" = "Gifts_gift_Currency", # Add declaration year
  )|> 
  mutate(
    income_date = paste0("31.12.", lubridate::year(lubridate::dmy(DeclarationSubmitDate))-1)
  ) -> gift

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |>
  select(Id:BirthDate, DeclarationSubmitDate, InOuts) |> 
  unnest_longer(col = InOuts) |> 
  tibble() |> 
  unnest_wider(col = InOuts, names_sep = "_inout_") |> 
  filter(InOuts_inout_Type == 1) |>
  select(
    Id:DeclarationSubmitDate, InOuts_inout_Amount, InOuts_inout_Currency
  )|> 
  mutate(
    income_type = "in_out"
  ) |> 
  rename(
    "value" = "InOuts_inout_Amount",
    "currency" = "InOuts_inout_Currency", # Add declaration year
  )|> 
  mutate(
    income_date = paste0("31.12.", lubridate::year(lubridate::dmy(DeclarationSubmitDate))-1)
  ) -> inouts

parliament |> 
  tibble() |> 
  unnest_wider(col = parliament) |>
  select(Id:BirthDate, DeclarationSubmitDate, Enterprice) |> 
  unnest_longer(col = Enterprice) |> 
  tibble() |> 
  unnest_wider(col = Enterprice, names_sep = "_entr_")|>
  select(
    Id:DeclarationSubmitDate, Enterprice_entr_Income, Enterprice_entr_IncomeCurrencyName
  )|> 
  mutate(
    income_type = "entrepreneurial_activities"
  ) |> 
  rename(
    "value" = "Enterprice_entr_Income",
    "currency" = "Enterprice_entr_IncomeCurrencyName", # Add declaration year
  )|> 
  mutate(
    income_date = paste0("31.12.", lubridate::year(lubridate::dmy(DeclarationSubmitDate))-1)
  )  -> entrepreneurial


## Currencies



household_income |>
  bind_rows(
    gift, inouts, entrepreneurial # securities cash
  ) |> 
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



household_income |>
  bind_rows(
    gift, inouts, entrepreneurial# securities, cash, 
  ) |> 
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
    declaration_year = lubridate::year(lubridate::dmy(DeclarationSubmitDate))-1,
    exclude_securities = case_when(
      declaration_year != lubridate::year(income_date) & income_type == "securities" ~ 1,
      T ~ 0
    )
  ) |>
  filter(exclude_securities == 0) -> incomes_all
  
incomes_all |>   
  group_by(Id, FirstName, LastName, BirthDate,  declaration_year) |> 
  summarize(
    income = sum(income, na.rm = T)
  ) |>
  ungroup() |> 
  arrange(
    FirstName, LastName, BirthDate, declaration_year
  ) |>
  mutate(
    monthly_income = income/12
  ) |> 
  group_by(declaration_year) |>
  summarize(
    mean = mean(monthly_income, na.rm = T),
    median = median(monthly_income, na.rm = T),
    total = sum(monthly_income, na.rm = T)
  ) |> 
  left_join(
    cpi, by = c(
      "declaration_year" = "year"
    )
  ) |>
  mutate(
    across(
      mean:total, ~ . / cpi, .names = "{col}_adj"
    )
  ) -> monthly_incomes


### Total incomes per convocation

incomes_all |>   
  group_by(Id, FirstName, LastName, BirthDate,  declaration_year) |> 
  summarize(
    income = sum(income, na.rm = T)
  ) |>
  ungroup() |> 
  arrange(
    FirstName, LastName, BirthDate, declaration_year
  ) |>
  group_by(declaration_year) |>
  summarize(
    mean = mean(income, na.rm = T),
    median = median(income, na.rm = T),
    total = sum(income, na.rm = T)
  ) |> 
  left_join(
    cpi, by = c(
      "declaration_year" = "year"
    )
  ) |>
  mutate(
    across(
      mean:total, ~ . / cpi, .names = "{col}_adj"
    )
  ) -> annual_incomes


### SHINDA household incomes, Geostat

hh_incomes <- read_csv("datasets/mean_median.csv")

### 

hh_incomes |> 
  filter(year > 2008 & measure == "Median") |>
  left_join(
    monthly_incomes, by = c(
      "year" = "declaration_year"
    )
  ) |>
  rename(
    "Households" = `Nominal values`,
    "Parliament" = "median"
  ) |> 
  select(-c(`Adjusted to inflation (base: 2010 prices)`, mean, measure)) |> 
  mutate(
    across(
      Households:total, ~ . / cpi, .names = "{col}_adj"
    )
  ) |> 
  mutate(
    diff = Parliament_adj / Households_adj
  ) |>
  filter(
    diff == max(diff) | diff == min(diff)
  ) -> min_max
    
  

### Chart

hh_incomes |> 
  filter(year > 2008 & measure == "Median") |>
  left_join(
    monthly_incomes, by = c(
      "year" = "declaration_year"
    )
  ) |> 
  rename(
    "Households" = `Adjusted to inflation (base: 2010 prices)`,
    "Parliament" = "median_adj"
  ) |> 
  select(year, Households, Parliament, cpi) |>
  pivot_longer(-c(year, cpi), names_to = "measure", values_to = "income") |>
  mutate(
    year = as.Date(lubridate::my(paste0("01-", year))),
  ) |> 
  filter(measure != "total") |> 
  ggplot(aes(year, income, fill = measure, color = measure))+
  # geom_area(stat = "identity", position = "identity")+
  # geom_point() +
  geom_line(linewidth = 1.5) +
  scale_x_date(date_labels = "%Y", breaks = "1 year")+
  scale_y_continuous(limits = c(0, 6000))+
  scale_color_manual(values=c("#036D9C", "#A1331C")) +
  # scale_alpha_manual(values=c(1, .3))+
  ## Annotate
  ## Max. divergence
  annotate(
    "point",
    x=as.Date("2009-01-01"),
    y = round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2009 & hh_incomes$measure == "Median"], 0),
    size = 3, color = "#036D9C"
  ) +
  annotate(
    "text",
    x = as.Date("2009-01-01"),
    y = round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2009 & hh_incomes$measure == "Median"], 0),
    label = paste0(round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2009 & hh_incomes$measure == "Median"], 0), " ₾"),
    color = "#036D9C", family = "BPG SSP Crystal", size = 4, vjust = -1, hjust = -0.2, fontface = "bold"
  ) +
  annotate(
    "point",
    x=as.Date("2009-01-01"),
    y = round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2009], 0),
    size = 3, color = "#A1331C"
  ) +
  annotate(
    "text",
    x = as.Date("2009-01-01"),
    y = round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2009], 0),
    label = paste0(round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2009], 0), " ₾"),
    color = "#A1331C", family = "BPG SSP Crystal", size = 4, vjust = -1, hjust = -0.2, fontface = "bold"
  ) +
  annotate("segment", x=as.Date("2009-01-01"), xend=as.Date("2009-01-01"),
           y = min_max$Households_adj[min_max$year == 2009] + 100,
           yend = min_max$Parliament_adj[min_max$year == 2009] - 100,
           arrow = arrow(length = unit(0.05, "inches"), ends = "both"),
           linewidth = 0.6,
           lineend = "butt",
           linejoin = "round")+
  annotate("text", x =as.Date("2009-02-01"), y=min_max$Parliament_adj[min_max$year == 2009]/2,
           label = "←",
           hjust = 0,
           family = "FiraGO",
           size = 6)+
  annotate("text", x =as.Date("2009-06-01"), y=min_max$Parliament_adj[min_max$year == 2009]/2, lineheight = 0.8,
           label=paste0("2009 წელს, პარლამენტის წევრის\nშინამეურნეობისწლიური მედიანური შემოსავალი\nქვეყნის იმავე მაჩვენებელს\n",
                        round(min_max$diff[min_max$year == 2009], 1), "-ჯერ აღემატებოდა"
                        ),
           hjust = 0,
           family = "BPG SSP Crystal", size = 5)+
  ## Min. divergence
  annotate(
    "point",
    x=as.Date("2020-01-01"),
    y = round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2020 & hh_incomes$measure == "Median"], 0),
    size = 3, color = "#036D9C"
  ) +
  annotate(
    "text",
    x = as.Date("2020-01-01"),
    y = round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2020 & hh_incomes$measure == "Median"], 0),
    label = paste0(round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2020 & hh_incomes$measure == "Median"], 0), " ₾"),
    color = "#036D9C", family = "BPG SSP Crystal", size = 4, vjust = 2, hjust = 0.5, fontface = "bold"
  ) +
  annotate(
    "point",
    x=as.Date("2020-01-01"),
    y = round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2020], 0),
    size = 3, color = "#A1331C"
  ) +
  annotate(
    "text",
    x = as.Date("2020-01-01"),
    y = round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2020], 0),
    label = paste0(round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2020], 0), " ₾"),
    color = "#A1331C", family = "BPG SSP Crystal", size = 4, vjust = -1.5, hjust = 0.5, fontface = "bold"
  ) +
  annotate("segment", x=as.Date("2020-01-01"), xend=as.Date("2020-01-01"),
           y = min_max$Households_adj[min_max$year == 2020] + 100,
           yend = min_max$Parliament_adj[min_max$year == 2020] - 100,
           arrow = arrow(length = unit(0.05, "inches"), ends = "both"),
           linewidth = 0.6,
           lineend = "butt",
           linejoin = "round")+
  annotate("text", x =as.Date("2019-07-31"), y=min_max$Parliament_adj[min_max$year == 2020]/2,
           label = "→",
           hjust = 0,
           family = "FiraGO", size = 6)+
  annotate("text", x =as.Date("2019-06-30"), y=min_max$Parliament_adj[min_max$year == 2020]/2, lineheight = 0.8,
           label=paste0("2020 წელს, შემოსავლებს შორის ფარდობა\nმინიმუმამდე შემცირდა,თუმცა\nპარლამენტარის შინამეურნეობის შემოსავალი\nქვეყნის მედიანას მაინც\n",
                        round(min_max$diff[min_max$year == 2020], 1), "-ჯერ მეტი იყო"
           ),
           hjust = 1,
           family = "BPG SSP Crystal", size = 5)+
  ## 2022
  annotate(
    "point",
    x=as.Date("2022-01-01"),
    y = round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2022 & hh_incomes$measure == "Median"], 0),
    size = 3, color = "#036D9C"
  ) +
  annotate(
    "text",
    x = as.Date("2022-01-01"),
    y = round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2022 & hh_incomes$measure == "Median"], 0),
    label = paste0(round(hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2022 & hh_incomes$measure == "Median"], 0), " ₾"),
    color = "#036D9C", family = "BPG SSP Crystal", size = 4, vjust = 2, hjust = 0.5, fontface = "bold"
  ) +
  annotate(
    "point",
    x=as.Date("2022-01-01"),
    y = round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2022], 0),
    size = 3, color = "#A1331C"
  ) +
  annotate(
    "text",
    x = as.Date("2022-01-01"),
    y = round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2022], 0),
    label = paste0(round(monthly_incomes$median_adj[monthly_incomes$declaration_year == 2022], 0), " ₾"),
    color = "#A1331C", family = "BPG SSP Crystal", size = 4, vjust = -1.5, hjust = 0.5, fontface = "bold"
  ) +
  annotate("segment", x=as.Date("2022-01-01"), xend=as.Date("2022-01-01"),
           y = hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2022 & hh_incomes$measure == "Median"] + 100,
           yend = monthly_incomes$median_adj[monthly_incomes$declaration_year == 2022] - 100,
           arrow = arrow(length = unit(0.05, "inches"), ends = "both"),
           linewidth = 0.6,
           lineend = "butt",
           linejoin = "round")+
  annotate("text", x = as.Date("2021-06-30"), y=2500, lineheight = 0.8,
           label=paste0("2022 წელს,\nპარლამენტარის\nშინამეურნეობის\nშემოსავალი\nქვეყნის მედიანას\n",
                        round(
                          monthly_incomes$median_adj[monthly_incomes$declaration_year == 2022]/
                            hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2022 & hh_incomes$measure == "Median"], 1
                        ), "-ჯერ აღემატებოდა"
           ),
           hjust = 1,
           family = "BPG SSP Crystal", size = 5)+
  annotate("text", x =as.Date("2021-07-31"),
           y = 2500,
           label = "→",
           hjust = 0,
           family = "FiraGO", size = 6)+
  ## Category names
  annotate(
    "text",
    x = as.Date("2016-01-01"),
    y = 5500,
    label = "პარლამენტის წევრის შინამეურნეობის მედიანური შემოსავალი, 2010 წლის ფასებში",
    color = "#A1331C", family = "BPG SSP Crystal", size = 4, hjust = 0, fontface = "bold"
  ) +
  annotate("segment", x = as.Date("2016-01-01"), xend = as.Date("2015-01-01"),
           y = 5500, yend = monthly_incomes$median_adj[monthly_incomes$declaration_year == 2015],
           color = "#A1331C")+
  annotate(
    "text",
    x = as.Date("2016-01-01"),
    y = 100,
    label = "საქართველოში შინამეურნეობის მედიანური შემოსავალი, 2010 წლის ფასებში",
    color = "#036D9C", family = "BPG SSP Crystal", size = 4, hjust = 0, fontface = "bold"
  ) +
  annotate("segment", x = as.Date("2016-01-01"), xend = as.Date("2015-01-01"),
           y = 100,
           yend = hh_incomes$`Adjusted to inflation (base: 2010 prices)`[hh_incomes$year == 2015 & hh_incomes$measure == "Median"],
           color = "#036D9C")+
  ## Labels
  labs(
    title = "შეფარდება პარლამენტარის და საქართველოში შინამეურნეობების\nყოველთვიურ მედიანურ შემოსავლებს შორის",
    subtitle = "\nპარლამენტარის შინამეურნეობის ყოველთვიური მედიანური შემოსავალი,\nგაზომილი 2010 წლის ფასებში, ქვეყნის მედიანურ მაჩვენებელზე 7-ჯერ მეტია",
    caption = "\nწყარო: Declaration.gov.ge API; საქსტატის შინამეურნეობების შემოსავლების და ხარჯების კვლევა, 2009-2022"
  )+
  theme_ds()+
  theme(
    plot.title = element_text(family  = "BPG SSP Crystal Caps")
  )

ggsave("incomes_geo.png", device = "png", width=1536, height=903, units = "px")




