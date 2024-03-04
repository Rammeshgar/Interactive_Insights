---
title: "Interactive Insights"
output: 
  github_document:
    keep_md: true
    # theme:
    #   bg: "#101010"
    #   fg: "#FDF7F7" 
    #   primary: "#ED79F9"
    #   base_font:
    #     google: Prompt
    #   code_font:
    #     google: JetBrains Mono
    # orientation: columns
    # vertical_layout: fill
    # social: [ "twitter", "facebook", "menu"]
    # source_code: embed
---


```css
body {
  padding: 10px!important;
}

.flexdashboard-navbar {
  margin-top: 0px!important;
}

.flexdashboard-title {
  padding-top: 0px;
}

.flexdashboard-body .flexdashboard-row > .chart {
  margin-top: 0 !important;
  padding: 0 !important;
}

```


<style type="text/css">
body {
  padding: 10px!important;
}

.flexdashboard-navbar {
  margin-top: 0px!important;
}

.flexdashboard-title {
  padding-top: 0px;
}

.flexdashboard-body .flexdashboard-row > .chart {
  margin-top: 0 !important;
  padding: 0 !important;
}

</style>





```r
## Loading data's

air_polution_death <-
  read.csv("D:/it proj/practice dashboard/outdoor-pollution-deaths-1990-2017.csv")

population_range <- 
  read.csv("D:/it proj/practice dashboard/population-and-demography.csv")
```



```r
## Data Cleaning (df1)


#(RENAMING)
population_range <- population_range %>% mutate(
  Country.name = case_match(Country.name,
                            "United States" ~ "United States of America",
                            "Democratic Republic of Congo" ~
                              "Democratic Republic of the Congo",
                            "Tanzania" ~ "United Republic of Tanzania",
                            "Congo" ~ "Republic of Congo",
                            "Cote d'Ivoire" ~ "Ivory Coast",
                            "Czechia" ~ "Czech Republic",
                            .default = Country.name))


#(CLEANING AND SYNCING)
population_range <- population_range %>% 
  filter(!Country.name %in% c("world",
                              "Less developed regions",
                              "Less developed regions, excluding China", 
                              "Less developed regions,
                              excluding least developed countries", 
                              "Lower-middle-income countries",
                              "Asia (UN)",
                              "Africa (UN)",
                              "Least developed countries",
                              "Upper-middle-income countries",
                              "Low-income countries",
                              "Land-locked developing countries (LLDC)",
                              "High-income countries",
                              "More developed regions",
                              "Latin America and the Caribbean (UN)",
                              "Europe (UN)",
                              "Small island developing states (SIDS)")) %>% 
  mutate(Country.name = str_trim(Country.name)) %>%
  mutate(Percentage.Under.25 = Population.under.the.age.of.25/Population*100)


#(MAKING 4 CATEGORIES)
population_range <- population_range %>%
  mutate(
    Population.Above.60 = Population.aged.60.to.69.years + Population.aged.70.to.79.years +
                          Population.aged.80.to.89.years + Population.aged.90.to.99.years +
                          Population.older.than.100.years,
    Percentage.Above.60 = round(Population.Above.60 / Population * 100, 1),
    Population.40.to.59 = Population.aged.40.to.49.years + Population.aged.50.to.59.years,
    Percentage.40.to.59 = round(Population.40.to.59 / Population * 100, 1),
    Population.20.to.39 = Population.aged.20.to.29.years + Population.aged.30.to.39.years,
    Percentage.20.to.39 = round(Population.20.to.39 / Population * 100, 1),
    Population.Below.20 = Population.at.age.1 + Population.aged.1.to.4.years +
                          Population.aged.5.to.9.years + Population.aged.10.to.14.years +
                          Population.aged.15.to.19.years,
    Percentage.Below.20 = round(Population.Below.20 / Population * 100, 1)
  )
# Adding Continent and iso2
population_range$Continent <- countrycode(population_range$Country.name,
                                          "country.name", "continent")
population_range$iso_code <- countrycode(population_range$Country.name,
                                         "country.name", "iso2c")
```



```r
## Data Cleaning (df2)

df <- air_polution_death %>% rename("All_couse_gender_age" = 4,
                                    "All_couse_gender_age2" = 6)

df$Continent <- countrycode(df$Entity, origin = "country.name",
                            destination = "continent")

map_info <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- df %>% 
  mutate(Code = recode(Code,"SSD" = "SDS")) %>% 
  left_join(map_info, by = c("Code" = "adm0_a3")) %>% 
  sf::st_as_sf()
```


Global Population {data-orientation="rows"}
================================================================================


Row 
--------------------------------------------------------------------------------


### "Germany" youth population under 25 (1950-2021) 


```r
total_percentage_G <- population_range %>%
  filter(Country.name == "Germany") %>%
  summarise(Total = round(sum(Percentage.Under.25/71, na.rm = TRUE),2)) %>%
  pull(Total)
total_percentage_G <- paste(total_percentage_G, "%")

valueBox(total_percentage_G,
         icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">31.74 %</span><!--/html_preserve-->


### "US" youth population under 25 (1950-2021) 


```r
total_percentage_US <- population_range %>%
  filter(Country.name == "Northern America (UN)") %>%
  summarise(Total = round(sum(Percentage.Under.25/71, na.rm = TRUE),2)) %>%
  pull(Total)
total_percentage_US <- paste(total_percentage_US, "%")

valueBox(total_percentage_US,
         icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">39.51 %</span><!--/html_preserve-->



```r
means <- population_range %>%
  filter(Year == "2021") 
means <- round(sapply(means[, c('Percentage.20.to.39', 'Percentage.40.to.59',
                                'Percentage.Above.60', 'Percentage.Below.20')],
                      mean, na.rm = TRUE),2)
 

cat(paste0("### Global.", names(which.max(means))," 2021 {data-width=100}"))
```

### Global.Percentage.Below.20 2021 {data-width=100}

```r
highest_mean_column <- names(which.max(means))

gauge(max(means),
            min = 0,
            max = 100,
            gaugeSectors(success = c(0, 32),
                         warning = c(33, 65),
                         danger = c(66, 100),
                         colors = c("green", "yellow", "red")))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
par(mar = c(0, 4, 0, 4))
```


### "Japan" youth population under 25 (1950-2021) 


```r
total_percentage_J <- population_range %>%
  filter(Country.name == "Japan") %>%
  summarise(Total = round(sum(Percentage.Under.25/71, na.rm = TRUE),2)) %>%
  pull(Total)
total_percentage_J <- paste(total_percentage_J, "%")

valueBox(total_percentage_J,
         icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">35.92 %</span><!--/html_preserve-->


### "UK" youth population under 25 (1950-2021) 


```r
total_percentage_UK <- population_range %>%
  filter(Country.name == "United Kingdom") %>%
  summarise(Total = round(sum(Percentage.Under.25/71, na.rm = TRUE),2)) %>%
  pull(Total)
total_percentage_UK <- paste(total_percentage_UK, "%")

valueBox(total_percentage_UK,
         icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">34.68 %</span><!--/html_preserve-->


Row 
--------------------------------------------------------------------------------


### Global View on Four Categories 2021 


```r
custom_colors <- c('rgb(255, 99, 71)', 'rgb(54, 162, 235)', 'rgb(255, 206, 86)',
                   'rgb(75, 192, 192)')

p2 <- plot_ly(labels = ~names(means),
              values = ~means,
              type = 'pie',
              marker = list(colors = custom_colors)) %>%
      layout(showlegend = TRUE,
             paper_bgcolor = "lightgray")
p2
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


### Population Growth by Continent 


```r
population_range <- na.omit(population_range)
line <- ggplot(population_range, aes (Year, Population, color = Continent,
                                     fill = Population.Above.60))+
  geom_smooth()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 0,hjust = 1, color = "cyan4"),
        axis.text.y = element_text(angle = 0,hjust = 1, color = "purple4"),
        axis.title.x = element_text(colour = "steelblue4"),
        axis.title.y = element_text(colour = "steelblue4"),
        panel.background = element_rect(fill = "lightcyan"),
        plot.background = element_rect(fill = "lightskyblue1", colour = NA),
        legend.text = element_text(color = "black", face = "bold"),
        legend.title = element_text(color = "antiquewhite"),
  legend.background = element_rect(fill = "lightseagreen",
                                   colour = "grey3"))+
  scale_color_manual(values = c("red4", "cyan4", "gold2", "navy", "orange3"))+
  scale_fill_manual(values = c("pink2", "navy", "cyan3", "purple", "grey")) +
  labs(x= "Date", y= "Population")

ggplotly(line)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


### Aged Nations: 2021 Insights 


```r
chart_new <- population_range %>%
  filter(Year == "2021") %>%
  select(Country.name,iso_code , Population, Percentage.Above.60,
         Continent, Year) %>% 
  arrange(-Percentage.Above.60) %>% 
  head(30)

chart <- ggplot(chart_new, aes (iso_code, Percentage.Above.60, color = Population,
                                     fill = Continent))+
  geom_col()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1, color = "navy"),
        axis.text.y = element_text(angle = 0,hjust = 1, color = "gold"),
        axis.title.x = element_text(colour = "azure2"),
        axis.title.y = element_text(colour = "azure2"),
        panel.background = element_rect(fill = "gray2"),
        plot.background = element_rect(fill = "mediumpurple2", colour = NA),
        legend.position = "none")+
  scale_color_gradient(low = "red4",high = "cyan")+
   scale_fill_manual(values = c("red4", "indianred2",  "greenyellow",
                                "hotpink4", "navy")) + 
  labs(x= "Countries", y= "Older than 60 %")

ggplotly(chart)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


Air Pollution Death Rate
================================================================================

Column {data-width=300 .tabset}
--------------------------------------------------------------------------------


### Most


```r
top_EU <- df %>%
  filter(Continent == "Europe",
         Year %in% (1990:2019),
         All_couse_gender_age >= 10000,
         !Entity %in% c("West Germany", "Vatican", "USSR", "Aland Islands")) %>%
  select(Entity, All_couse_gender_age, Year, Code) %>%
  arrange(-All_couse_gender_age) %>% 
  head(290)

#______________________

EU <- ggplot(top_EU, aes(Entity, All_couse_gender_age,
                         color=All_couse_gender_age, frame = Year))+
  geom_jitter()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1, color = "gray4"),
        axis.text.y = element_text(angle = 0,hjust = 1, color = "orange"),
        axis.title.x = element_text(colour = "honeydew"),
        axis.title.y = element_text(colour = "honeydew"),
        legend.position = "none",
        plot.background = element_rect(fill = "cyan4", colour = NA),
        panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(color = "honeydew", size = 16, face = "bold"))+
  scale_color_gradient(low = "purple4",high = "pink", guide = "jitter")+
  labs(title = " Top Death Rate EU",
       subtitle = "Death rate by color", x= "Nations", y= "Death rate")
ggplotly(EU)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


### Least


```r
base_EU <- df %>%
  filter(Continent == "Europe",
         All_couse_gender_age <= 10000,
         !Entity %in% c("West Germany", "Vatican", "USSR", "Aland Islands")) %>%
  select(Entity, All_couse_gender_age, Year, Code) %>%
  arrange(All_couse_gender_age) %>% 
  head(290)

#_______________________

EU2 <- ggplot(base_EU, aes(Entity, All_couse_gender_age,
                           color=Year, group = Year))+
  geom_point()+
  theme_update()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1, color = "cyan"),
        axis.text.y = element_text(angle = 0,hjust = 1, color = "green"),
        axis.title.x = element_text(colour = "grey70"),
        axis.title.y = element_text(colour = "grey70"),
        legend.position = "none",
        plot.background = element_rect(fill = "pink4", colour = NA),
        panel.background = element_rect(fill = "lightpink"),
        plot.title = element_text(color = "grey70", size = 16, face = "bold"))+
  scale_color_gradient(low = "red4",high = "cyan", guide = "jitter")+
  labs(title = " Least Death Rate EU",
       subtitle = "Death rate by color", x= "Nations", y= "Death rate")
ggplotly(EU2)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


### General


```r
filter_summarize <- function(df, Continent) {
  with(df, df[Continent == Continent,]) %>%
    group_by(Year, Continent) %>%
    summarise(total = sum(All_couse_gender_age, na.rm = T))
}
Asia <- filter_summarize(df, "Asia")
Africa <- filter_summarize(df, "Africa")
Americas <- filter_summarize(df, "Americas")
Oceania <- filter_summarize(df, "Oceania")
Europe <- filter_summarize(df, "Europe")
tables <- rbind(Asia, Africa, Americas, Oceania, Europe)
tables <- na.omit(tables)

#______________________

world <- ggplot(tables, aes(Year, total, color=Continent, fill = Continent))+
  geom_area()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 0,hjust = 1, color = "yellow"),
        axis.text.y = element_text(angle = 0,hjust = 1, color = "lightpink"),
        axis.title.x = element_text(colour = "bisque1"),
        axis.title.y = element_text(colour = "bisque1"),
        panel.background = element_rect(fill = "lightgrey"),
        plot.background = element_rect(fill = "purple4", colour = NA),
        plot.title = element_text(color = "bisque1", size = 16, face = "bold"))+
  scale_color_manual(values = c("red4", "cyan4", "gold2", "navy", "orange3"))+
  scale_fill_manual(values = c("pink2", "navy", "cyan3", "purple", "grey")) +
  labs(title = "World Death Rate", x= "Date", y= "Death")
ggplotly(world)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


Column {data-width=600}
--------------------------------------------------------------------------------


### World Map 2019


```r
world_death_pollution <- map_data %>% 
  filter(Year=="2019") %>% 
  select(Entity,All_couse_gender_age, Continent, Year, Code)

breaks <- seq(1, 120000, length.out = 11)

map.num <- mapview(world_death_pollution ,zcol = "All_couse_gender_age",
                   layer.name = "World Death Rate by Air Pollution",
                   at = breaks, crs = "+proj=robin")

map.num
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


Data Table1
================================================================================


```r
datatable(population_range,
          caption = "World Population",
          rownames = T,
          filter = "top",
          options = list(pageLength = 25))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


Data Table2
================================================================================


```r
datatable(air_polution_death,
          caption = "Air Pollution Death",
          rownames = T,
          filter = "top",
          options = list(pageLength = 25))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


Summary
================================================================================

Column {data-width=300}
--------------------------------------------------------------------------------


### “Air Pollution Fatalities Globally in 2019”


```r
sum1 <- df %>% 
  filter(Year == "2019",
         Entity == "World") %>% 
  select(All_couse_gender_age, Entity)
  
valueBox(sum1 ,icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">
4140971
World
</span><!--/html_preserve-->


### “Air Pollution Mortality at Its Lowest in 2019”


```r
sum2 <- df %>% 
  filter(Year == "2019",
         Entity == "Tokelau") %>% 
  select(All_couse_gender_age, Entity)
  
valueBox(sum2 ,icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">
0.21179798
Tokelau
</span><!--/html_preserve-->


### “Youth Triumph: Remarkable Under-20s in 2021”


```r
Sum3 <- population_range %>% 
  filter(Year == "2021",
         Country.name == "Central African Republic") %>% 
  select(Percentage.Below.20, Country.name)

Sum3 <- glue("{Sum3$Percentage.Below.20}% { Sum3$Country.name}")
  
valueBox(Sum3 ,icon = "fa-user")
```

<!--html_preserve--><span class="value-output" data-icon="fa-user" data-color-accent="primary">60.2% Central African Republic</span><!--/html_preserve-->
Column
--------------------------------------------------------------------------------

"Report Summary"

* Data Preparation:

Import Data Frame 1 (World Population) and Data Frame 2 (Air Pollution Death Rate).

Clean and manipulate Data Frame 1:
Rename values for better clarity, remove unnecessary data,
and create 4 age categories.
Calculate percentages and population for each category.

Clean and manipulate Data Frame 2:
Rename columns for ease in use, add new column for Continents,
Synchronize Data Frame 2 with a map data frame.

* Page 1: Global Age Categories:
Visualize age groups worldwide.
Highlight the most aged country.
Explore population growth and youth demographics.

* Page 2: European Death Rates:
Customize visualizations for European countries.
Compare death rates.
Utilize world map data.

* Pages 3 and 4: Key Insights:
Share findings and insights.


About Report
================================================================================

Column
--------------------------------------------------------------------------------

* This is a report on "16848 in 35" Age of world's Population,
And "6921 in 7" Fatality by Air Pollution Data.

* Source code is available on top right.

* Data Sources: 

#### https://ourworldindata.org/grapher/outdoor-pollution-deaths-1990-2017?tab=table

#### https://ourworldindata.org/population-growth#explore-data-poverty


Column
--------------------------------------------------------------------------------

* This report was generated on March 04, 2024.

* Created by: Sadeq Rezai
