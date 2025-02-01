library(tidyverse)
library(jsonlite)
library(httr)
library(extrafont)
loadfonts(device = "win")

setwd('D:\\Dropbox (Personal)\\My projects\\scratchpad\\declarations')

# a <- GET("https://declaration.gov.ge/Api/Declarations?&OrganisationIds=441")

# 828

# Parliament

# a <- POST("https://declaration.gov.ge/Api/Declarations?&OrganisationIds=441")
## 429, 450 not accesible
## Scarped on March 15, 2023

# for(i in 450:828) {
for(i in 430:828) {
  url <- paste0("https://declaration.acb.gov.ge/Api/Declarations?&OrganisationIds=", i)

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

  content(req) |> write_json(paste0("raw_json/", i, '.json'))
  
  Sys.sleep(2)

}


b <- read_json("raw_json/461.json")

content(a) -> b

tibble(b) -> c

# write_json(c, "parliament.json")

c |> 
  unnest_wider(b) -> mps

c |> 
  unnest_wider(b) |>
  select(Id, FamilyMembers) |> 
  unnest_longer(FamilyMembers) |>
  unnest_wider(FamilyMembers) -> family_members

c |> 
  unnest_wider(b) |>
  select(Id, Properties) |> 
  unnest_longer(Properties) |>
  unnest_wider(Properties) -> properties

table(properties$PropertyType)

c |> 
  unnest_wider(b) |>
  select(Id, Properties) |> 
  unnest_longer(Properties) |>
  unnest_wider(Properties) |> 
  select(Id, Shareholders) |> 
  unnest_longer(Shareholders) |>
  unnest_wider(Shareholders) -> shareholders_properties
  

c |> 
  unnest_wider(b) |>
  select(Id, MovableProperties) |> 
  unnest_longer(MovableProperties) |>
  unnest_wider(MovableProperties) -> movable_properties
  
c |> 
  unnest_wider(b) |>
  select(Id, Securities) |> 
  unnest_longer(Securities) |>
  unnest_wider(Securities) -> securities
  
c |> 
  unnest_wider(b) |>
  select(Id, BankAccounts) |> 
  unnest_longer(BankAccounts) |>
  unnest_wider(BankAccounts) -> bank_accounts
  
c |> 
  unnest_wider(b) |>
  select(Id, InOuts) |> 
  unnest_longer(InOuts) |>
  unnest_wider(InOuts) -> in_outs

in_outs |> 
  group_by(Id) |> 
  filter(Type == 1) |> 
  summarize(
    instances = n(),
    sum = sum(Amount),
    mean = mean(Amount),
    median = mean(Amount),
    
  ) -> monetary_income



family_members |> 
  filter(Relationship == "შვილი") |> 
  mutate(
    country = gsub("^(.*?),.*", "\\1", BirthPlace),
    country = case_when(
      str_detect(country, "საქ|სააქ|saqartvelo") ~ "საქართველო",
      str_detect(country, "ამერიკის შეერთებული შტატები") ~ "აშშ",
      str_detect(country, "ბრიუსელი") ~ "ბელგია",
      str_detect(country, "ლიტვ") ~ "ლიეტუვა",
      str_detect(country, "მოლდავეთი") ~ "მოლდოვა",
      str_detect(country, "სომხე") ~ "სომხეთი",
      str_detect(country, "ყაზახ") ~ "ყაზახეთი",
      str_detect(country, "რუსე") ~ "რუსეთი",
      str_detect(country, "ინგლისი|ბრიტან") ~ "გაერთიანებული სამეფო",
      T ~ as.character(country)
    )
  ) |>
  group_by(Id, country) |> 
  count() |> 
  ungroup() |> 
  filter(country != "") |>
  pivot_wider(
    names_from = "country", values_from = "n" 
  )-> non_georgia_born_children


mps |> 
  select(Id, FirstName, LastName, BirthDate, DeclarationSubmitDate) |> 
  left_join(non_georgia_born_children, by = "Id") |> 
  mutate(
    submit_date = lubridate::dmy(DeclarationSubmitDate)
  ) |> 
  group_by(FirstName, LastName, BirthDate) |> 
  slice(which.max(submit_date)) |>
  ungroup() |> 
  summarize(
    `საქართველო` = sum(`საქართველო`, na.rm = T),
    `რუსეთი` = sum(`რუსეთი`, na.rm = T),
    `ყაზახეთი` = sum(`ყაზახეთი`, na.rm = T),
    `შვეიცარია` = sum(`შვეიცარია`, na.rm = T),
    `მოლდოვა` = sum(`მოლდოვა`, na.rm = T),
    `სომხეთი` = sum(`სომხეთი`, na.rm = T),
    `აშშ` = sum(`აშშ`, na.rm = T),
    `საფრანგეთი` = sum(`საფრანგეთი`, na.rm = T),
    `გერმანია` = sum(`გერმანია`, na.rm = T),
    `ლიეტუვა` = sum(`ლიეტუვა`, na.rm = T),
    `ავსტრია` = sum(`ავსტრია`, na.rm = T),
    `საფრანგეთი` = sum(`საფრანგეთი`, na.rm = T),
    `გერმანია` = sum(`გერმანია`, na.rm = T),
    `გაერთიანებული სამეფო` = sum(`გაერთიანებული სამეფო`, na.rm = T),
    `იტალია` = sum(`იტალია`, na.rm = T),
    `ისრაელი` = sum(`ისრაელი`, na.rm = T),
  ) |>
  t() |>
  data.frame() |> 
  rownames_to_column(var = "country") |>
  set_names("country", "count") |> 
  filter(count != 0 & country != "საქართველო") -> kids

kids |> 
  ggplot(aes(x = reorder(country, count), y = count, label = count))+
  geom_col()+
  geom_text(nudge_y = 1, family = "FiraGO", size = 4)+
  coord_flip()+
  labs(
    title = "უცხოეთში დაბადებული შვილების რაოდენობა",
    subtitle = "პარლამენტარების მიერ ჩაბარებული\nბოლო დეკლარაციების მიხედვით, 2010-2023",
    y = "რ-ნობა",
    caption = "წყარო: declaration.gov.ge/Api"
  )+
  theme_bw()+
  theme(
    text = element_text(family = "FiraGO"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_blank()
  )

c |> 
  unnest_wider(b) |>
  select(Id, Contracts) |> 
  unnest_longer(Contracts) |>
  unnest_wider(Contracts) |>
  group_by(ContractType) |>
  filter(str_detect(ContractType, "სესხ")) |> 
  select(Id, Subject, Amount, Currency, StartDate, Agency) -> loans

mps |> 
  select(Id, FirstName, LastName, BirthDate, DeclarationSubmitDate) |> 
  right_join(loans, by = "Id") |> 
  mutate(
    submit_date = lubridate::dmy(DeclarationSubmitDate),
    decl_year = lubridate::year(submit_date)
  ) |> 
  # group_by(FirstName, LastName, BirthDate, decl_year, Agency, Currency) |> 
  group_by(decl_year, Agency, Currency, submit_date) |> 
  summarize(
    sum = sum(Amount, na.rm = T)
  ) |>
  mutate(
    bank = case_when(
      str_detect(Agency, "საქარტ|საქართვ|საქართვეოლოს|საქართველოს ბანკ|საქ ბანკი|საქ. ბანკი|საქ.ბანკი|საქრთველოს") ~ "საქართველოს ბანკი",
      str_detect(Agency, "ტიბისი|ტი ბი სი|თი - ბი|თი-ბი|TBC|ტი-ბი-სი|თი-ბისი|თიბისი|თი-ბი-სი|თი ბი სი|თბს|თიბის|თი–ბი–სი") ~ "თი-ბი-სი",
      str_detect(Agency, "რესბუბლიკა|რესპუბლიკ") ~ "ბანკი რესპუბლიკა",
      str_detect(Agency, "ბაზის") ~ "ბაზისბანკი",
      str_detect(Agency, "ვითიბი|ვითიბი|ვი თი ბი|ვთბ|ვი-თი-ბი") ~ "ვი-თი-ბი",
      str_detect(Agency, "სტანდარ") ~ "კორ-სტანდარტბანკი",
      str_detect(Agency, "ქართუ") ~ "ქართუ",
      str_detect(Agency, "პრივატ") ~ "პრივატბანკი",
      str_detect(Agency, "ლიბერთი|სახალხო") ~ "ლიბერთი ბანკი",
      str_detect(Agency, "პროკრე|პროკრედიტ|პრო კრედიტ") ~ "პროკრედიტბანკი",
      str_detect(Agency, "ბანკი") ~ "სხვა ბანკი",
      T ~ "სხვა ორგანიზაცია/არაა მითითებული"
    )
  ) |> 
  group_by(decl_year, bank, Currency) |>
  summarize(
    loans = sum(sum, na.rm = T)
  ) |>
  filter(Currency == "USD") |> 
  ggplot(aes(x = decl_year, y = loans))+
  geom_col()+
  facet_wrap(~bank)
