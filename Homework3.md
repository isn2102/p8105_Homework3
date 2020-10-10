Homework3
================
Isabel Nelson
10/6/2020

## Problem 1

**Describe the data:**

``` r
data("instacart")
```

**comments**  
The dataset instacart contains 1384617 observations and 15 columns.
Observations are at the level of items in orders placed by users. The
variables provide information about the users/orders and the items
purchased. For example aisle contains values such as “fresh vegetables”
and “eggs,” product\_name contains values such as “spring water” and
“organic half and half” and days\_since\_prior\_order is numeric. The
total list of variables is: order\_id, product\_id,
add\_to\_cart\_order, reordered, user\_id, eval\_set, order\_number,
order\_dow, order\_hour\_of\_day, days\_since\_prior\_order,
product\_name, aisle\_id, department\_id, aisle, department.

**Count aisles:**

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

**comments**  
There are 134 aisles, with most items ordered from the fresh fruits and
fresh vegetables aisles.

**Make a plot of aisles:**

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate(
    aisle = factor(aisle), 
    aisle = fct_reorder(aisle, n)
    ) %>% 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="Homework3_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

**comments**  
Most of the aisles with order counts above 10,000 show between 10k - 20k
orders. There are about 8 aisles with order counts between 20-40k, 3
aisles with order counts between 40-80k, and two aisles (fruit and
vegetables) with order counts around 150k.

**Make a table of popular items:**

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle) %>% 
  count(product_name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(aisle, rank) %>% 
  knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

**comments**  
The most popular items in baking are brown sugar, baking soda, and cane
sugar. The most popular items in dog food care are snack sticks, organix
chicken recipe, and small dog biscuits. The most popular items in
packaged fruits/veggies are baby spinach, raspberries, and blueberries.

**Make a table for mean hour of product ordered on each day of the
week:**

``` r
instacart %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  group_by(product_name, order_dow) %>% 
  summarize(mean_hour = mean(order_hour_of_day)) %>% 
  pivot_wider(
    names_from = order_dow, 
    values_from = mean_hour
  )
```

    ## `summarise()` regrouping output by 'product_name' (override with `.groups` argument)

    ## # A tibble: 2 x 8
    ## # Groups:   product_name [2]
    ##   product_name       `0`   `1`   `2`   `3`   `4`   `5`   `6`
    ##   <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Coffee Ice Cream  13.8  14.3  15.4  15.3  15.2  12.3  13.8
    ## 2 Pink Lady Apples  13.4  11.4  11.7  14.2  11.6  12.8  11.9

**comments**  
Overall coffee ice cream is ordered a little later in the day on
weekdays (around 3pm), while pink lady apples are ordered a little
earlier in the day (around noon). On weekends both products on average
are ordered around 12pm.

## Problem 2

**A.** Load data and tidy by cleaning variable names and pivoting to
longer with one variable for the minute of activity in the day and one
variable for the activity count in that minute. Add a weekend and
weekday variable and change minute of activity to numeric.

``` r
accel_df <- read_csv("./data/accel_data.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "minute_activity",
    names_prefix = "activity_", 
    values_to = "activity_count") %>% 
  mutate(
    weekend = ifelse(day_id %in% c("3", "4"), "weekend", "weekday"),
    minute_activity = as.numeric(minute_activity), 
    day = as.factor(day), 
    day = forcats::fct_relevel(day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
```

**comments**  
The dataset accel\_df contains 50400 observations and 6 variables.
Variables are the week, the day, the specific minute of the day, the
activity count in that minute, and whether it was a weekend or a
weekday.

**B.** Create a table with daily activity counts summed across all
minutes for the 5 weeks.

``` r
accel_df %>% 
  group_by(week, day) %>% 
  summarize(
    daily_activity = sum(activity_count)) %>% 
  pivot_wider(
    names_from = day, 
    values_from = daily_activity) %>% 
  knitr::kable()
```

| week |    Monday |  Tuesday | Wednesday | Thursday |   Friday | Saturday | Sunday |
| ---: | --------: | -------: | --------: | -------: | -------: | -------: | -----: |
|    1 |  78828.07 | 307094.2 |    340115 | 355923.6 | 480542.6 |   376254 | 631105 |
|    2 | 295431.00 | 423245.0 |    440962 | 474048.0 | 568839.0 |   607175 | 422018 |
|    3 | 685910.00 | 381507.0 |    468869 | 371230.0 | 467420.0 |   382928 | 467052 |
|    4 | 409450.00 | 319568.0 |    434460 | 340291.0 | 154049.0 |     1440 | 260617 |
|    5 | 389080.00 | 367824.0 |    445366 | 549658.0 | 620860.0 |     1440 | 138421 |

**comments**  
From this summary it seems that in weeks 4 and 5 the activity on Sunday
and particularly Saturday was lower than the other days and weeks. Other
than that trends are not very apparent.

**C.** Create a plot showing activity over the course of the day, with
each day of the week represented by a different color.

``` r
accel_df %>% 
  ggplot(aes(x = minute_activity, y = activity_count, color = day)) + 
  geom_point(size = .1, alpha = .5) +
  geom_line(size = .2) + 
  geom_smooth(size = .2) +
  labs(title = "Activity over the course of the day", x = "Minute", y = "Activity Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust
                                   = 1))
```

<img src="Homework3_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

**comments**  
Based on this graph I can see that the first \~300 minutes of the day
have much less activity than the rest of the day. There are some spikes
in activity around 500-700 minutes (especially for Sunday) and around
1300-1400 minutes. The spikes later in the day are particularly
pronounced on Fridays.

## Problem 3

**A.** Describe the dataset:

``` r
data("ny_noaa")
miss_prcp <-
  ny_noaa %>% 
  drop_na(prcp)
miss_snow <-
  ny_noaa %>% 
  drop_na(snow)
miss_snwd <-
  ny_noaa %>% 
  drop_na(snwd)
miss_tmax <-
  ny_noaa %>% 
  drop_na(tmax)
miss_tmin <-
  ny_noaa %>% 
  drop_na(tmin)
```

**comments**  
The dataset ny\_noaa contains 2595176 observations and 7 variables.
Variables include: id, date, prcp, snow, snwd, tmax, tmin. It includes
data from 1981-01-01 to 2010-12-31. There are 145838 missing prcp
values, 381221 missing snow values, 591786 missing snwd values, 1134358
missing tmax values, and 1134420 missing tmin values. Given this, it
appears that about 5% of the data are missing for each variable.

**B.** Do some data cleaning: create separate variables for
year/month/day, divide temperature, snowfall, and snow depth by 10 to
result in cm, divide precipitation by 100 to result in cm.

``` r
tidy_ny_noaa <- 
  ny_noaa %>% 
  separate(date, c("Year", "Month", "Day"), convert = TRUE) %>% 
  mutate(prcp = prcp/100,
         snow = snow/10,
         snwd = snwd/10,
         tmax = as.numeric(tmax)/10,
         tmin = as.numeric(tmin)/10)  

month_df <-
  tibble(
    Month = 1:12, 
    month_name = month.name
  )

tidy_ny_noaa <- left_join(tidy_ny_noaa, month_df, by = "Month")

tidy_ny_noaa %>% 
  count(snow, sort = TRUE)
```

    ## # A tibble: 282 x 2
    ##     snow       n
    ##    <dbl>   <int>
    ##  1   0   2008508
    ##  2  NA    381221
    ##  3   2.5   31022
    ##  4   1.3   23095
    ##  5   5.1   18274
    ##  6   7.6   10173
    ##  7   0.8    9962
    ##  8   0.5    9748
    ##  9   3.8    9197
    ## 10   0.3    8790
    ## # … with 272 more rows

**comments**  
The most commonly observed values for snowfall are 0 and NA. This is
likely because overall there were a lot of missing values, and because
NY only gets snow a few weeks out of the year.

**C.** Make a two-panel plot showing the average max temperature in
January and in July in each station across years.

``` r
tidy_ny_noaa %>% 
  group_by(id, Year, month_name) %>% 
  filter(month_name %in% c("January", "July")) %>% 
  summarize(
    avg_max_temp = mean(tmax, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = avg_max_temp, group = id)) +
  geom_point(size = .1) +
  geom_path(size = .1) +
  facet_grid(. ~ month_name) +
  labs(
       title = "Mean average temperature for January and July by station over time", 
       x = "Year", 
       y = "Average maximum temperature (C)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

    ## `summarise()` regrouping output by 'id', 'Year' (override with `.groups` argument)

    ## Warning: Removed 5970 rows containing missing values (geom_point).

    ## Warning: Removed 5931 row(s) containing missing values (geom_path).

<img src="Homework3_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

**comments**  
Most of the stations are overlapping in their max temperatures,
generally spanning from about -12 degrees C to 10 degrees C in January
and from 20 degrees C to 32 degrees C in July. There are a few outliers
at particular stations. The temperature does not appear to be trending
in any particular direction overall across the years in July, and seems
to be trending slightly higher over the years in January. However some
years are clearly colder or warmer than others at all stations.

**D.** Make a two-panel plot showing (i) tmax vs tmin and (ii)
distribution of snowfall by year.

``` r
tmintmax_df <- 
  tidy_ny_noaa %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  stat_bin_hex(colour = "white", na.rm = TRUE) +
  scale_fill_gradientn(colours = c("blue","green"), 
                       name = "Frequency", 
                       na.value = NA) +
  labs(
    title = "Max and Min Temperature",
    x = "Minimum daily temperature (C)",
    y = "Maxiumum daily temperature (C)",
    caption = "Data from the rnoaa package") + 
  theme(legend.direction = "vertical")

snowfall_df <-
  tidy_ny_noaa %>% 
  filter(
    snow < 100,  
    snow > 0) %>% 
  group_by(Year) %>% 
  mutate(Year = as.factor(Year)) %>% 
  ggplot(aes(y = snow, x = Year)) + 
  geom_violin(aes(fill = Year), alpha = .4) + 
  labs(
    title = "Distribution of snowfall by year", 
    x = "year", 
    y = "Snowfall", 
    caption = "Data from the rnoaa package") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
(tmintmax_df + snowfall_df)
```

<img src="Homework3_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

**comments**  
Min and max temperature appear to be positively correlated. I used the
hexbin package to more clearly display density - the highest density is
in the area of min temperature 0-30 degrees C and max temperature 0-30
degrees C. The daily snowfall appears to be most frequently between zero
and five cm, with some outliers that are larger each year. The trends
across years seem stable.
