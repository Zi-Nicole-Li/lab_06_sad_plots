Lab 06 - Ugly charts and Simpson’s paradox
================
Zi Li
Feb 23, Due Feb 26

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 

library(usethis)
use_git_config(
  user.name = "Zi-Nicole-Li",
  user.email = "lzcr0203@gmail.com"
)

#I use my Gmail for my github account.
```

### Exercise 1

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))

staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line(linewidth = 1.1) + 
  labs(
    title = "Trends in Instructional Staff Employment"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/Ex1-1.png)<!-- -->

``` r
# then, I will show that the proportion of part-time faculty have gone up over time compared to other instructional staff types.
# to do that, I would need to highlight how part-time faculty increase over time, I would need to made their line thicker. 

staff_long %>%
  mutate(highlight = if_else(faculty_type == "Part-Time Faculty", "Part-Time Faculty", "Other")) %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = highlight,
    alpha = highlight,
    linewidth = highlight
  )) +
  geom_line() +
  scale_color_manual(values = c("Part-Time Faculty" = "red", "Other" = "black")) +
  scale_alpha_manual(values = c("Part-Time Faculty" = 1, "Other" = 0.4)) +
  scale_linewidth_manual(values = c("Part-Time Faculty" = 1.5, "Other" = 1)) +
  labs(
    title = "Increase in Part-Time Faculty Employment",
    x = "Year",
    y = "value",
    color = "Faculty Type"
  ) +
  theme_minimal() 
```

![](lab-06_files/figure-gfm/Ex,highlight%20how%20part-time%20faculty%20increase%20over%20time-1.png)<!-- -->

``` r
# In a ggplot2 tutorial, I found out how to make one of my lines different from the others. the link is here: https://www.rdocumentation.org/packages/ggplot2/versions/0.9.1/topics/scale_colour_manual.
```

### Exercise 2

Remove this text, and add your answer for Exercise 1 here. Add code
chunks as needed. Don’t forget to label your code chunk. Do not use
spaces in code chunk labels.

### Exercise 3

…

Add exercise headings as needed.
