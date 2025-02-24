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
  ) 
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
  ) 
```

![](lab-06_files/figure-gfm/Ex,highlight%20how%20part-time%20faculty%20increase%20over%20time-1.png)<!-- -->

``` r
# In a ggplot2 tutorial, I found out how to make one of my lines different from the others. the link is here: https://www.rdocumentation.org/packages/ggplot2/versions/0.9.1/topics/scale_colour_manual.
```

### Exercise 2

``` r
# maybe we can use horizontal bar charts for easier comparison and clarity. 

fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fisheries
```

    ## # A tibble: 216 × 4
    ##    country             capture aquaculture  total
    ##    <chr>                 <dbl>       <dbl>  <dbl>
    ##  1 Afghanistan            1000        1200   2200
    ##  2 Albania                7886         950   8836
    ##  3 Algeria               95000        1361  96361
    ##  4 American Samoa         3047          20   3067
    ##  5 Andorra                   0           0      0
    ##  6 Angola               486490         655 487145
    ##  7 Antigua and Barbuda    3000          10   3010
    ##  8 Argentina            755226        3673 758899
    ##  9 Armenia                3758       16381  20139
    ## 10 Aruba                   142           0    142
    ## # ℹ 206 more rows

``` r
fisheries_data <- fisheries %>%
  pivot_longer(cols = c(capture, aquaculture),
               names_to = "Type",
               values_to = "Tonnage")
fisheries_data %>%
  ggplot(aes(x = reorder(country, Tonnage), y = Tonnage, fill = Type)) +
  geom_col() +
  coord_flip() +
  labs(title = "Fishery Production by Country",
       x = "country",
       y = "Total Tonnage (tons)",
       fill = "Production Type") 
```

![](lab-06_files/figure-gfm/Fisheries-1.png)<!-- -->

``` r
# Nah... the above method made my bar charts all aggregated and it's impossible to see how each country is distributed.
# maybe I need to try a different plot.
# Nah, still not working, they all aggregated together. I may need to filter out the top country. I use top 15 countries. 

top15_fisheries <- fisheries %>%
  mutate(total = capture + aquaculture) %>%
  arrange(desc(total)) %>% 
  slice_head(n = 15) %>%
  pivot_longer(cols = c(capture, aquaculture),
               names_to = "type",
               values_to = "production")
# the above calculate my top 15 countries. 

top15_fisheries %>%
  ggplot(aes(x = production, y = fct_reorder(country, production), fill = type)) +
  geom_col() +
  labs(
    title = "Top 15 Countries by Fishery Production",
    x = "Production (tons)",
    y = "Country",
    fill = "Production Type"
  ) 
```

![](lab-06_files/figure-gfm/Fisheries-2.png)<!-- -->

``` r
# It works!! Many thanks for this guide: https://r-graph-gallery.com/web-horizontal-barplot-with-labels-the-economist.html.
# and also for this guy's dplyr tutorials: https://dplyr.tidyverse.org/reference/slice.html
```

### Exercise 3

``` r
library(tidyverse)
library(mosaicData)
data(Whickham)

Whickham
```

    ##      outcome smoker age
    ## 1      Alive    Yes  23
    ## 2      Alive    Yes  18
    ## 3       Dead    Yes  71
    ## 4      Alive     No  67
    ## 5      Alive     No  64
    ## 6      Alive    Yes  38
    ## 7      Alive    Yes  45
    ## 8       Dead     No  76
    ## 9      Alive     No  28
    ## 10     Alive     No  27
    ## 11     Alive     No  28
    ## 12     Alive    Yes  34
    ## 13     Alive     No  20
    ## 14      Dead    Yes  72
    ## 15     Alive    Yes  48
    ## 16     Alive    Yes  45
    ## 17      Dead     No  66
    ## 18     Alive    Yes  30
    ## 19     Alive     No  67
    ## 20     Alive    Yes  33
    ## 21      Dead     No  68
    ## 22      Dead     No  72
    ## 23     Alive    Yes  34
    ## 24      Dead     No  61
    ## 25      Dead     No  66
    ## 26     Alive     No  43
    ## 27      Dead     No  71
    ## 28     Alive    Yes  47
    ## 29      Dead     No  67
    ## 30     Alive     No  22
    ## 31     Alive     No  39
    ## 32      Dead    Yes  71
    ## 33     Alive    Yes  38
    ## 34      Dead     No  80
    ## 35      Dead    Yes  59
    ## 36     Alive    Yes  47
    ## 37      Dead    Yes  56
    ## 38      Dead     No  62
    ## 39     Alive    Yes  30
    ## 40     Alive    Yes  51
    ## 41     Alive     No  34
    ## 42     Alive     No  39
    ## 43      Dead     No  56
    ## 44     Alive     No  32
    ## 45      Dead     No  56
    ## 46     Alive    Yes  60
    ## 47      Dead    Yes  60
    ## 48     Alive     No  30
    ## 49     Alive    Yes  33
    ## 50     Alive     No  37
    ## 51      Dead     No  66
    ## 52      Dead    Yes  71
    ## 53     Alive    Yes  23
    ## 54     Alive     No  38
    ## 55     Alive    Yes  36
    ## 56      Dead    Yes  38
    ## 57     Alive    Yes  50
    ## 58     Alive     No  45
    ## 59     Alive    Yes  36
    ## 60      Dead     No  55
    ## 61      Dead    Yes  73
    ## 62      Dead    Yes  52
    ## 63     Alive     No  52
    ## 64     Alive    Yes  37
    ## 65     Alive     No  61
    ## 66     Alive     No  39
    ## 67     Alive     No  72
    ## 68     Alive    Yes  25
    ## 69     Alive    Yes  50
    ## 70      Dead     No  68
    ## 71     Alive    Yes  53
    ## 72     Alive     No  31
    ## 73      Dead     No  73
    ## 74     Alive     No  54
    ## 75     Alive    Yes  56
    ## 76     Alive    Yes  51
    ## 77      Dead     No  76
    ## 78     Alive     No  55
    ## 79      Dead    Yes  56
    ## 80     Alive     No  56
    ## 81      Dead    Yes  69
    ## 82      Dead     No  79
    ## 83      Dead    Yes  75
    ## 84      Dead     No  67
    ## 85     Alive     No  20
    ## 86     Alive     No  20
    ## 87      Dead     No  68
    ## 88      Dead    Yes  68
    ## 89     Alive     No  21
    ## 90     Alive    Yes  29
    ## 91     Alive    Yes  33
    ## 92     Alive    Yes  30
    ## 93     Alive    Yes  24
    ## 94      Dead    Yes  61
    ## 95     Alive    Yes  54
    ## 96     Alive     No  26
    ## 97      Dead     No  68
    ## 98     Alive    Yes  49
    ## 99      Dead     No  72
    ## 100    Alive     No  21
    ## 101    Alive     No  28
    ## 102    Alive     No  47
    ## 103     Dead     No  68
    ## 104     Dead     No  71
    ## 105    Alive    Yes  59
    ## 106    Alive     No  64
    ## 107    Alive    Yes  47
    ## 108     Dead     No  84
    ## 109    Alive     No  28
    ## 110    Alive     No  67
    ## 111    Alive     No  30
    ## 112     Dead    Yes  33
    ## 113     Dead     No  68
    ## 114     Dead     No  55
    ## 115     Dead     No  79
    ## 116    Alive     No  40
    ## 117    Alive     No  43
    ## 118    Alive    Yes  37
    ## 119     Dead     No  73
    ## 120     Dead    Yes  45
    ## 121    Alive     No  49
    ## 122    Alive    Yes  44
    ## 123    Alive    Yes  74
    ## 124    Alive    Yes  20
    ## 125    Alive    Yes  31
    ## 126    Alive     No  20
    ## 127    Alive     No  61
    ## 128     Dead    Yes  66
    ## 129    Alive     No  53
    ## 130    Alive    Yes  36
    ## 131     Dead     No  71
    ## 132     Dead    Yes  64
    ## 133    Alive     No  52
    ## 134    Alive     No  37
    ## 135    Alive    Yes  34
    ## 136    Alive     No  61
    ## 137    Alive    Yes  46
    ## 138    Alive    Yes  26
    ## 139    Alive     No  33
    ## 140    Alive     No  32
    ## 141    Alive    Yes  47
    ## 142    Alive     No  27
    ## 143    Alive     No  25
    ## 144     Dead     No  69
    ## 145     Dead    Yes  43
    ## 146     Dead    Yes  71
    ## 147    Alive     No  33
    ## 148     Dead     No  73
    ## 149     Dead     No  68
    ## 150    Alive     No  31
    ## 151    Alive     No  35
    ## 152     Dead     No  73
    ## 153     Dead    Yes  59
    ## 154    Alive     No  31
    ## 155     Dead     No  77
    ## 156    Alive    Yes  33
    ## 157    Alive    Yes  48
    ## 158    Alive    Yes  34
    ## 159    Alive     No  43
    ## 160     Dead     No  68
    ## 161    Alive    Yes  29
    ## 162     Dead     No  71
    ## 163     Dead    Yes  21
    ## 164    Alive     No  18
    ## 165    Alive     No  24
    ## 166    Alive    Yes  30
    ## 167    Alive     No  24
    ## 168     Dead     No  74
    ## 169    Alive    Yes  57
    ## 170    Alive     No  23
    ## 171    Alive     No  42
    ## 172    Alive     No  34
    ## 173    Alive    Yes  46
    ## 174    Alive     No  40
    ## 175    Alive    Yes  64
    ## 176     Dead    Yes  20
    ## 177    Alive    Yes  30
    ## 178     Dead    Yes  81
    ## 179     Dead    Yes  45
    ## 180    Alive     No  26
    ## 181     Dead     No  77
    ## 182    Alive    Yes  53
    ## 183    Alive    Yes  46
    ## 184     Dead     No  69
    ## 185    Alive    Yes  44
    ## 186     Dead    Yes  50
    ## 187    Alive    Yes  50
    ## 188     Dead     No  62
    ## 189     Dead    Yes  60
    ## 190    Alive     No  19
    ## 191    Alive    Yes  21
    ## 192    Alive    Yes  50
    ## 193    Alive    Yes  33
    ## 194     Dead     No  59
    ## 195     Dead     No  81
    ## 196    Alive    Yes  31
    ## 197     Dead     No  77
    ## 198    Alive    Yes  23
    ## 199    Alive     No  55
    ## 200    Alive     No  51
    ## 201    Alive     No  26
    ## 202    Alive     No  20
    ## 203    Alive     No  57
    ## 204     Dead     No  67
    ## 205    Alive     No  29
    ## 206    Alive    Yes  28
    ## 207    Alive    Yes  26
    ## 208     Dead     No  76
    ## 209    Alive     No  32
    ## 210     Dead    Yes  40
    ## 211    Alive     No  40
    ## 212    Alive     No  52
    ## 213     Dead     No  35
    ## 214    Alive    Yes  27
    ## 215    Alive    Yes  33
    ## 216    Alive    Yes  20
    ## 217    Alive    Yes  31
    ## 218    Alive     No  28
    ## 219    Alive     No  22
    ## 220     Dead    Yes  73
    ## 221    Alive     No  23
    ## 222    Alive    Yes  24
    ## 223    Alive     No  48
    ## 224    Alive     No  60
    ## 225    Alive    Yes  50
    ## 226    Alive    Yes  37
    ## 227    Alive    Yes  29
    ## 228    Alive     No  35
    ## 229     Dead     No  67
    ## 230    Alive     No  33
    ## 231    Alive    Yes  36
    ## 232     Dead     No  64
    ## 233    Alive    Yes  59
    ## 234    Alive     No  23
    ## 235    Alive    Yes  63
    ## 236    Alive     No  26
    ## 237     Dead     No  50
    ## 238     Dead     No  36
    ## 239     Dead    Yes  51
    ## 240    Alive     No  25
    ## 241    Alive     No  48
    ## 242    Alive     No  24
    ## 243     Dead    Yes  42
    ## 244    Alive     No  64
    ## 245    Alive     No  36
    ## 246    Alive    Yes  47
    ## 247    Alive     No  64
    ## 248    Alive     No  22
    ## 249    Alive     No  31
    ## 250    Alive     No  55
    ## 251     Dead    Yes  78
    ## 252    Alive     No  27
    ## 253    Alive     No  63
    ## 254     Dead    Yes  83
    ## 255    Alive     No  55
    ## 256    Alive    Yes  57
    ## 257    Alive    Yes  31
    ## 258     Dead     No  64
    ## 259    Alive    Yes  25
    ## 260    Alive     No  38
    ## 261     Dead     No  68
    ## 262    Alive     No  59
    ## 263    Alive    Yes  63
    ## 264    Alive    Yes  18
    ## 265    Alive    Yes  44
    ## 266    Alive     No  38
    ## 267     Dead    Yes  72
    ## 268    Alive    Yes  28
    ## 269    Alive    Yes  37
    ## 270     Dead     No  51
    ## 271     Dead    Yes  53
    ## 272    Alive    Yes  31
    ## 273     Dead     No  69
    ## 274    Alive    Yes  51
    ## 275    Alive    Yes  36
    ## 276    Alive     No  27
    ## 277    Alive    Yes  24
    ## 278    Alive    Yes  64
    ## 279    Alive     No  64
    ## 280    Alive    Yes  64
    ## 281     Dead     No  64
    ## 282    Alive    Yes  46
    ## 283    Alive    Yes  67
    ## 284    Alive    Yes  40
    ## 285    Alive    Yes  49
    ## 286    Alive    Yes  25
    ## 287    Alive    Yes  36
    ## 288     Dead     No  82
    ## 289    Alive    Yes  56
    ## 290     Dead    Yes  56
    ## 291    Alive     No  37
    ## 292     Dead    Yes  59
    ## 293    Alive    Yes  42
    ## 294    Alive     No  44
    ## 295    Alive    Yes  18
    ## 296    Alive    Yes  44
    ## 297    Alive     No  32
    ## 298    Alive    Yes  38
    ## 299    Alive    Yes  32
    ## 300    Alive    Yes  29
    ## 301    Alive     No  51
    ## 302    Alive    Yes  34
    ## 303    Alive    Yes  32
    ## 304    Alive     No  23
    ## 305    Alive    Yes  48
    ## 306    Alive    Yes  24
    ## 307    Alive    Yes  21
    ## 308    Alive     No  54
    ## 309     Dead     No  68
    ## 310    Alive    Yes  26
    ## 311    Alive     No  51
    ## 312    Alive    Yes  45
    ## 313    Alive     No  31
    ## 314    Alive    Yes  43
    ## 315    Alive    Yes  30
    ## 316     Dead     No  72
    ## 317    Alive     No  19
    ## 318    Alive     No  31
    ## 319     Dead     No  70
    ## 320    Alive     No  58
    ## 321    Alive     No  27
    ## 322    Alive     No  56
    ## 323     Dead    Yes  46
    ## 324     Dead     No  75
    ## 325    Alive     No  35
    ## 326    Alive     No  30
    ## 327     Dead     No  47
    ## 328    Alive     No  63
    ## 329    Alive     No  66
    ## 330    Alive    Yes  41
    ## 331     Dead    Yes  82
    ## 332    Alive    Yes  39
    ## 333    Alive    Yes  33
    ## 334     Dead    Yes  60
    ## 335     Dead     No  73
    ## 336    Alive    Yes  26
    ## 337    Alive     No  21
    ## 338    Alive    Yes  39
    ## 339     Dead    Yes  51
    ## 340    Alive     No  28
    ## 341    Alive    Yes  54
    ## 342    Alive     No  51
    ## 343     Dead     No  69
    ## 344    Alive    Yes  63
    ## 345    Alive    Yes  47
    ## 346    Alive     No  38
    ## 347     Dead    Yes  61
    ## 348    Alive    Yes  31
    ## 349    Alive    Yes  62
    ## 350    Alive     No  62
    ## 351    Alive    Yes  21
    ## 352    Alive    Yes  32
    ## 353    Alive     No  38
    ## 354     Dead     No  67
    ## 355    Alive     No  59
    ## 356     Dead     No  71
    ## 357    Alive     No  35
    ## 358    Alive    Yes  28
    ## 359    Alive    Yes  31
    ## 360     Dead    Yes  58
    ## 361    Alive    Yes  53
    ## 362    Alive     No  48
    ## 363    Alive     No  27
    ## 364    Alive     No  30
    ## 365     Dead    Yes  50
    ## 366     Dead    Yes  43
    ## 367    Alive     No  41
    ## 368    Alive     No  37
    ## 369    Alive     No  31
    ## 370    Alive     No  30
    ## 371    Alive    Yes  52
    ## 372    Alive    Yes  37
    ## 373    Alive     No  45
    ## 374    Alive    Yes  36
    ## 375    Alive    Yes  34
    ## 376    Alive    Yes  39
    ## 377    Alive     No  30
    ## 378     Dead    Yes  57
    ## 379    Alive     No  40
    ## 380     Dead    Yes  62
    ## 381    Alive    Yes  62
    ## 382    Alive    Yes  60
    ## 383    Alive     No  56
    ## 384    Alive     No  45
    ## 385    Alive     No  22
    ## 386    Alive     No  27
    ## 387    Alive    Yes  18
    ## 388    Alive    Yes  26
    ## 389     Dead     No  77
    ## 390    Alive     No  33
    ## 391    Alive     No  35
    ## 392    Alive     No  33
    ## 393     Dead     No  55
    ## 394    Alive     No  45
    ## 395    Alive     No  32
    ## 396    Alive     No  34
    ## 397    Alive     No  67
    ## 398    Alive     No  35
    ## 399     Dead    Yes  72
    ## 400     Dead     No  79
    ## 401    Alive    Yes  35
    ## 402    Alive     No  38
    ## 403    Alive    Yes  25
    ## 404    Alive    Yes  37
    ## 405    Alive     No  56
    ## 406     Dead     No  66
    ## 407     Dead     No  49
    ## 408    Alive    Yes  33
    ## 409     Dead     No  59
    ## 410    Alive     No  44
    ## 411    Alive     No  39
    ## 412     Dead     No  58
    ## 413    Alive     No  64
    ## 414    Alive     No  18
    ## 415    Alive    Yes  45
    ## 416    Alive     No  33
    ## 417    Alive     No  32
    ## 418     Dead     No  80
    ## 419    Alive    Yes  44
    ## 420    Alive    Yes  25
    ## 421    Alive     No  68
    ## 422    Alive    Yes  34
    ## 423    Alive    Yes  29
    ## 424     Dead    Yes  72
    ## 425    Alive    Yes  32
    ## 426    Alive    Yes  56
    ## 427    Alive    Yes  43
    ## 428     Dead    Yes  60
    ## 429    Alive    Yes  53
    ## 430    Alive    Yes  41
    ## 431    Alive    Yes  63
    ## 432     Dead    Yes  57
    ## 433    Alive     No  24
    ## 434    Alive    Yes  39
    ## 435    Alive    Yes  39
    ## 436    Alive     No  31
    ## 437    Alive    Yes  45
    ## 438    Alive     No  66
    ## 439    Alive     No  18
    ## 440     Dead     No  72
    ## 441    Alive     No  49
    ## 442    Alive     No  37
    ## 443     Dead     No  68
    ## 444     Dead    Yes  69
    ## 445    Alive     No  27
    ## 446    Alive    Yes  20
    ## 447    Alive     No  52
    ## 448    Alive    Yes  62
    ## 449    Alive    Yes  64
    ## 450    Alive    Yes  42
    ## 451    Alive     No  66
    ## 452     Dead     No  78
    ## 453    Alive     No  33
    ## 454    Alive     No  31
    ## 455    Alive    Yes  49
    ## 456    Alive     No  58
    ## 457    Alive    Yes  23
    ## 458    Alive    Yes  41
    ## 459    Alive    Yes  34
    ## 460    Alive    Yes  30
    ## 461    Alive    Yes  24
    ## 462    Alive    Yes  31
    ## 463    Alive    Yes  64
    ## 464     Dead    Yes  81
    ## 465     Dead     No  79
    ## 466    Alive     No  54
    ## 467    Alive    Yes  47
    ## 468    Alive    Yes  25
    ## 469     Dead    Yes  68
    ## 470    Alive    Yes  23
    ## 471    Alive     No  74
    ## 472    Alive    Yes  25
    ## 473    Alive    Yes  54
    ## 474     Dead     No  78
    ## 475     Dead     No  65
    ## 476    Alive    Yes  39
    ## 477     Dead     No  73
    ## 478    Alive     No  27
    ## 479    Alive    Yes  62
    ## 480    Alive    Yes  26
    ## 481     Dead    Yes  57
    ## 482    Alive     No  41
    ## 483    Alive     No  41
    ## 484     Dead    Yes  66
    ## 485     Dead    Yes  48
    ## 486     Dead    Yes  68
    ## 487    Alive    Yes  43
    ## 488    Alive     No  28
    ## 489    Alive    Yes  58
    ## 490     Dead    Yes  56
    ## 491     Dead    Yes  40
    ## 492     Dead     No  69
    ## 493    Alive    Yes  47
    ## 494    Alive    Yes  52
    ## 495    Alive    Yes  52
    ## 496     Dead     No  80
    ## 497    Alive    Yes  64
    ## 498    Alive     No  39
    ## 499    Alive    Yes  54
    ## 500    Alive     No  55
    ## 501    Alive     No  51
    ## 502    Alive    Yes  28
    ## 503     Dead     No  77
    ## 504    Alive    Yes  27
    ## 505     Dead     No  82
    ## 506     Dead     No  65
    ## 507     Dead     No  80
    ## 508    Alive     No  46
    ## 509    Alive    Yes  38
    ## 510    Alive    Yes  28
    ## 511     Dead     No  49
    ## 512    Alive    Yes  44
    ## 513    Alive     No  30
    ## 514    Alive     No  63
    ## 515    Alive     No  63
    ## 516     Dead    Yes  68
    ## 517    Alive    Yes  46
    ## 518    Alive     No  52
    ## 519    Alive     No  52
    ## 520    Alive     No  55
    ## 521    Alive     No  74
    ## 522    Alive    Yes  44
    ## 523    Alive     No  51
    ## 524    Alive    Yes  35
    ## 525    Alive    Yes  36
    ## 526    Alive    Yes  33
    ## 527    Alive     No  26
    ## 528    Alive     No  21
    ## 529    Alive     No  18
    ## 530    Alive     No  30
    ## 531     Dead     No  81
    ## 532    Alive    Yes  63
    ## 533     Dead    Yes  42
    ## 534    Alive     No  35
    ## 535    Alive    Yes  38
    ## 536    Alive     No  28
    ## 537    Alive    Yes  62
    ## 538    Alive     No  39
    ## 539    Alive     No  39
    ## 540    Alive     No  61
    ## 541    Alive    Yes  19
    ## 542    Alive    Yes  46
    ## 543     Dead    Yes  70
    ## 544    Alive    Yes  38
    ## 545    Alive     No  33
    ## 546    Alive    Yes  53
    ## 547    Alive     No  19
    ## 548     Dead     No  82
    ## 549    Alive    Yes  49
    ## 550    Alive    Yes  47
    ## 551     Dead     No  78
    ## 552    Alive    Yes  22
    ## 553    Alive    Yes  31
    ## 554    Alive     No  31
    ## 555    Alive     No  57
    ## 556    Alive     No  65
    ## 557     Dead     No  68
    ## 558    Alive    Yes  22
    ## 559    Alive    Yes  27
    ## 560    Alive    Yes  50
    ## 561     Dead    Yes  53
    ## 562    Alive     No  34
    ## 563    Alive    Yes  37
    ## 564    Alive     No  35
    ## 565    Alive     No  38
    ## 566     Dead     No  43
    ## 567    Alive    Yes  34
    ## 568    Alive     No  64
    ## 569    Alive     No  43
    ## 570     Dead    Yes  57
    ## 571    Alive    Yes  55
    ## 572    Alive    Yes  34
    ## 573    Alive     No  29
    ## 574    Alive     No  18
    ## 575     Dead     No  38
    ## 576    Alive    Yes  40
    ## 577    Alive     No  44
    ## 578     Dead     No  71
    ## 579     Dead    Yes  61
    ## 580    Alive    Yes  26
    ## 581     Dead     No  35
    ## 582    Alive     No  26
    ## 583    Alive     No  39
    ## 584    Alive    Yes  43
    ## 585    Alive    Yes  44
    ## 586    Alive     No  38
    ## 587    Alive     No  37
    ## 588     Dead    Yes  52
    ## 589    Alive     No  56
    ## 590     Dead     No  70
    ## 591    Alive    Yes  39
    ## 592     Dead    Yes  57
    ## 593    Alive    Yes  46
    ## 594    Alive    Yes  28
    ## 595    Alive     No  21
    ## 596     Dead    Yes  66
    ## 597    Alive    Yes  38
    ## 598    Alive    Yes  32
    ## 599    Alive    Yes  23
    ## 600    Alive     No  61
    ## 601    Alive    Yes  32
    ## 602    Alive     No  27
    ## 603     Dead    Yes  73
    ## 604    Alive    Yes  36
    ## 605    Alive     No  33
    ## 606    Alive     No  28
    ## 607    Alive     No  43
    ## 608    Alive    Yes  56
    ## 609    Alive     No  27
    ## 610    Alive     No  36
    ## 611    Alive     No  20
    ## 612     Dead     No  69
    ## 613    Alive     No  44
    ## 614    Alive    Yes  26
    ## 615     Dead    Yes  31
    ## 616     Dead     No  72
    ## 617     Dead     No  84
    ## 618    Alive    Yes  48
    ## 619    Alive     No  18
    ## 620     Dead    Yes  82
    ## 621    Alive    Yes  51
    ## 622    Alive    Yes  33
    ## 623    Alive     No  56
    ## 624    Alive     No  74
    ## 625    Alive     No  24
    ## 626     Dead     No  74
    ## 627    Alive     No  28
    ## 628    Alive    Yes  58
    ## 629    Alive    Yes  49
    ## 630     Dead    Yes  64
    ## 631    Alive     No  34
    ## 632     Dead     No  84
    ## 633     Dead    Yes  47
    ## 634    Alive    Yes  58
    ## 635     Dead    Yes  63
    ## 636     Dead     No  57
    ## 637    Alive    Yes  18
    ## 638    Alive     No  23
    ## 639    Alive     No  51
    ## 640    Alive     No  55
    ## 641    Alive     No  49
    ## 642     Dead     No  60
    ## 643    Alive     No  39
    ## 644    Alive     No  48
    ## 645    Alive     No  28
    ## 646     Dead     No  74
    ## 647    Alive    Yes  61
    ## 648    Alive    Yes  33
    ## 649     Dead     No  64
    ## 650     Dead     No  77
    ## 651    Alive    Yes  51
    ## 652    Alive    Yes  23
    ## 653     Dead     No  43
    ## 654    Alive     No  47
    ## 655    Alive     No  27
    ## 656    Alive     No  48
    ## 657    Alive    Yes  29
    ## 658    Alive     No  45
    ## 659    Alive    Yes  31
    ## 660     Dead    Yes  60
    ## 661    Alive     No  53
    ## 662    Alive     No  26
    ## 663     Dead    Yes  61
    ## 664    Alive     No  38
    ## 665    Alive     No  22
    ## 666    Alive    Yes  41
    ## 667    Alive    Yes  27
    ## 668    Alive     No  63
    ## 669     Dead    Yes  60
    ## 670     Dead     No  64
    ## 671    Alive     No  58
    ## 672    Alive     No  42
    ## 673     Dead     No  82
    ## 674     Dead     No  62
    ## 675    Alive     No  64
    ## 676    Alive     No  34
    ## 677    Alive     No  58
    ## 678    Alive    Yes  63
    ## 679     Dead     No  63
    ## 680     Dead     No  79
    ## 681    Alive     No  44
    ## 682    Alive     No  41
    ## 683    Alive     No  34
    ## 684    Alive    Yes  38
    ## 685    Alive    Yes  54
    ## 686    Alive     No  53
    ## 687     Dead     No  80
    ## 688     Dead    Yes  46
    ## 689    Alive    Yes  61
    ## 690     Dead     No  72
    ## 691    Alive    Yes  61
    ## 692    Alive    Yes  28
    ## 693    Alive    Yes  20
    ## 694     Dead    Yes  46
    ## 695     Dead     No  56
    ## 696     Dead     No  84
    ## 697     Dead    Yes  64
    ## 698     Dead     No  42
    ## 699    Alive     No  30
    ## 700    Alive     No  48
    ## 701    Alive    Yes  25
    ## 702     Dead     No  76
    ## 703     Dead    Yes  61
    ## 704    Alive     No  41
    ## 705    Alive     No  29
    ## 706     Dead     No  80
    ## 707    Alive    Yes  56
    ## 708    Alive    Yes  58
    ## 709     Dead     No  73
    ## 710    Alive     No  18
    ## 711    Alive     No  42
    ## 712     Dead     No  67
    ## 713    Alive     No  30
    ## 714     Dead     No  78
    ## 715     Dead    Yes  36
    ## 716     Dead     No  78
    ## 717    Alive     No  44
    ## 718    Alive     No  35
    ## 719    Alive    Yes  53
    ## 720     Dead     No  52
    ## 721    Alive    Yes  63
    ## 722     Dead     No  79
    ## 723    Alive     No  51
    ## 724    Alive     No  59
    ## 725    Alive     No  62
    ## 726    Alive     No  35
    ## 727    Alive     No  32
    ## 728    Alive    Yes  48
    ## 729     Dead    Yes  53
    ## 730     Dead    Yes  49
    ## 731    Alive    Yes  23
    ## 732    Alive    Yes  50
    ## 733    Alive     No  26
    ## 734     Dead    Yes  44
    ## 735     Dead    Yes  71
    ## 736     Dead     No  78
    ## 737    Alive     No  40
    ## 738    Alive     No  71
    ## 739    Alive    Yes  62
    ## 740    Alive     No  30
    ## 741    Alive     No  32
    ## 742     Dead     No  71
    ## 743    Alive     No  70
    ## 744    Alive     No  27
    ## 745     Dead    Yes  60
    ## 746     Dead     No  65
    ## 747     Dead     No  72
    ## 748    Alive     No  22
    ## 749    Alive    Yes  26
    ## 750    Alive     No  26
    ## 751    Alive    Yes  30
    ## 752    Alive    Yes  21
    ## 753    Alive    Yes  28
    ## 754    Alive     No  60
    ## 755    Alive     No  53
    ## 756    Alive    Yes  59
    ## 757     Dead    Yes  63
    ## 758    Alive    Yes  36
    ## 759    Alive     No  34
    ## 760    Alive    Yes  61
    ## 761     Dead     No  65
    ## 762     Dead    Yes  59
    ## 763     Dead     No  70
    ## 764     Dead    Yes  48
    ## 765    Alive    Yes  50
    ## 766     Dead     No  83
    ## 767    Alive     No  51
    ## 768    Alive    Yes  24
    ## 769    Alive     No  46
    ## 770    Alive     No  19
    ## 771    Alive    Yes  36
    ## 772    Alive     No  33
    ## 773     Dead     No  56
    ## 774    Alive     No  57
    ## 775    Alive     No  44
    ## 776    Alive     No  53
    ## 777    Alive    Yes  30
    ## 778    Alive     No  42
    ## 779    Alive    Yes  42
    ## 780    Alive    Yes  58
    ## 781    Alive     No  32
    ## 782    Alive     No  30
    ## 783     Dead     No  73
    ## 784    Alive     No  34
    ## 785     Dead    Yes  76
    ## 786    Alive    Yes  51
    ## 787     Dead     No  71
    ## 788     Dead     No  60
    ## 789    Alive     No  36
    ## 790    Alive    Yes  18
    ## 791    Alive     No  34
    ## 792     Dead     No  70
    ## 793    Alive    Yes  50
    ## 794    Alive    Yes  41
    ## 795    Alive     No  55
    ## 796    Alive     No  36
    ## 797     Dead    Yes  66
    ## 798    Alive     No  58
    ## 799    Alive    Yes  31
    ## 800    Alive     No  36
    ## 801     Dead     No  68
    ## 802    Alive     No  24
    ## 803    Alive     No  25
    ## 804    Alive    Yes  42
    ## 805    Alive     No  31
    ## 806    Alive    Yes  18
    ## 807    Alive    Yes  27
    ## 808    Alive     No  20
    ## 809    Alive     No  40
    ## 810    Alive    Yes  57
    ## 811    Alive     No  73
    ## 812    Alive     No  44
    ## 813    Alive     No  63
    ## 814    Alive     No  19
    ## 815     Dead    Yes  49
    ## 816    Alive     No  59
    ## 817    Alive    Yes  23
    ## 818    Alive    Yes  53
    ## 819    Alive    Yes  51
    ## 820    Alive    Yes  22
    ## 821    Alive     No  24
    ## 822    Alive    Yes  51
    ## 823    Alive    Yes  26
    ## 824     Dead     No  68
    ## 825     Dead     No  56
    ## 826    Alive     No  47
    ## 827    Alive    Yes  36
    ## 828    Alive    Yes  46
    ## 829    Alive     No  26
    ## 830    Alive    Yes  74
    ## 831    Alive     No  26
    ## 832    Alive    Yes  52
    ## 833    Alive     No  57
    ## 834    Alive    Yes  34
    ## 835    Alive     No  60
    ## 836    Alive    Yes  32
    ## 837    Alive    Yes  41
    ## 838     Dead     No  82
    ## 839    Alive    Yes  31
    ## 840    Alive     No  68
    ## 841    Alive     No  25
    ## 842     Dead     No  84
    ## 843    Alive     No  37
    ## 844    Alive     No  37
    ## 845     Dead     No  83
    ## 846    Alive     No  62
    ## 847    Alive     No  25
    ## 848    Alive    Yes  55
    ## 849    Alive     No  23
    ## 850    Alive    Yes  52
    ## 851    Alive    Yes  33
    ## 852    Alive    Yes  53
    ## 853    Alive     No  55
    ## 854    Alive    Yes  45
    ## 855    Alive    Yes  19
    ## 856    Alive    Yes  53
    ## 857    Alive     No  18
    ## 858    Alive    Yes  32
    ## 859    Alive     No  32
    ## 860     Dead     No  67
    ## 861     Dead    Yes  60
    ## 862    Alive    Yes  35
    ## 863    Alive    Yes  60
    ## 864    Alive    Yes  56
    ## 865    Alive     No  28
    ## 866     Dead     No  61
    ## 867    Alive    Yes  46
    ## 868    Alive     No  22
    ## 869     Dead     No  55
    ## 870     Dead     No  66
    ## 871     Dead    Yes  71
    ## 872     Dead     No  68
    ## 873    Alive     No  50
    ## 874    Alive    Yes  32
    ## 875    Alive     No  24
    ## 876    Alive     No  38
    ## 877    Alive     No  64
    ## 878     Dead    Yes  51
    ## 879    Alive    Yes  29
    ## 880     Dead     No  77
    ## 881    Alive     No  25
    ## 882    Alive     No  50
    ## 883    Alive     No  61
    ## 884    Alive    Yes  23
    ## 885     Dead     No  67
    ## 886     Dead     No  73
    ## 887    Alive     No  23
    ## 888    Alive    Yes  59
    ## 889     Dead     No  61
    ## 890    Alive    Yes  43
    ## 891    Alive     No  25
    ## 892    Alive     No  39
    ## 893    Alive     No  45
    ## 894    Alive     No  25
    ## 895    Alive    Yes  52
    ## 896     Dead     No  46
    ## 897    Alive     No  25
    ## 898    Alive     No  30
    ## 899    Alive    Yes  62
    ## 900    Alive     No  58
    ## 901    Alive    Yes  25
    ## 902    Alive     No  31
    ## 903     Dead     No  46
    ## 904     Dead    Yes  27
    ## 905     Dead     No  68
    ## 906    Alive    Yes  35
    ## 907    Alive     No  25
    ## 908    Alive    Yes  42
    ## 909     Dead     No  72
    ## 910    Alive    Yes  55
    ## 911    Alive     No  29
    ## 912    Alive    Yes  29
    ## 913    Alive    Yes  36
    ## 914     Dead     No  51
    ## 915     Dead     No  80
    ## 916     Dead     No  74
    ## 917    Alive     No  20
    ## 918     Dead     No  45
    ## 919    Alive     No  46
    ## 920    Alive    Yes  43
    ## 921    Alive     No  66
    ## 922     Dead    Yes  45
    ## 923    Alive    Yes  71
    ## 924    Alive     No  43
    ## 925    Alive    Yes  27
    ## 926    Alive     No  22
    ## 927    Alive    Yes  41
    ## 928    Alive     No  60
    ## 929    Alive    Yes  46
    ## 930    Alive     No  29
    ## 931     Dead    Yes  55
    ## 932    Alive    Yes  36
    ## 933     Dead     No  84
    ## 934    Alive     No  67
    ## 935     Dead     No  45
    ## 936    Alive     No  34
    ## 937     Dead     No  65
    ## 938    Alive    Yes  32
    ## 939     Dead     No  71
    ## 940    Alive     No  26
    ## 941    Alive    Yes  26
    ## 942     Dead     No  72
    ## 943    Alive     No  45
    ## 944    Alive     No  27
    ## 945    Alive    Yes  36
    ## 946    Alive     No  34
    ## 947     Dead     No  78
    ## 948    Alive    Yes  20
    ## 949    Alive    Yes  46
    ## 950    Alive     No  27
    ## 951    Alive     No  74
    ## 952    Alive    Yes  46
    ## 953    Alive    Yes  31
    ## 954    Alive    Yes  46
    ## 955    Alive     No  58
    ## 956     Dead     No  72
    ## 957     Dead    Yes  56
    ## 958    Alive     No  45
    ## 959    Alive     No  60
    ## 960     Dead     No  65
    ## 961     Dead     No  71
    ## 962     Dead     No  76
    ## 963    Alive     No  55
    ## 964    Alive     No  35
    ## 965    Alive    Yes  44
    ## 966     Dead    Yes  47
    ## 967     Dead    Yes  63
    ## 968    Alive    Yes  32
    ## 969    Alive     No  43
    ## 970    Alive    Yes  35
    ## 971     Dead    Yes  63
    ## 972    Alive     No  29
    ## 973    Alive     No  51
    ## 974    Alive    Yes  42
    ## 975     Dead    Yes  45
    ## 976    Alive    Yes  19
    ## 977     Dead     No  69
    ## 978    Alive    Yes  39
    ## 979     Dead    Yes  61
    ## 980    Alive    Yes  19
    ## 981    Alive     No  53
    ## 982    Alive    Yes  61
    ## 983    Alive     No  40
    ## 984    Alive     No  37
    ## 985    Alive     No  37
    ## 986     Dead     No  65
    ## 987    Alive     No  74
    ## 988    Alive    Yes  50
    ## 989    Alive    Yes  25
    ## 990    Alive     No  32
    ## 991    Alive     No  28
    ## 992    Alive     No  71
    ## 993     Dead     No  27
    ## 994    Alive     No  23
    ## 995    Alive    Yes  64
    ## 996    Alive     No  28
    ## 997     Dead     No  56
    ## 998    Alive     No  25
    ## 999     Dead    Yes  61
    ## 1000   Alive     No  63
    ## 1001   Alive     No  24
    ## 1002   Alive     No  64
    ## 1003    Dead     No  58
    ## 1004   Alive     No  34
    ## 1005   Alive     No  31
    ## 1006   Alive    Yes  33
    ## 1007   Alive    Yes  48
    ## 1008   Alive    Yes  55
    ## 1009   Alive     No  72
    ## 1010   Alive     No  43
    ## 1011   Alive    Yes  20
    ## 1012   Alive    Yes  62
    ## 1013   Alive     No  18
    ## 1014   Alive    Yes  21
    ## 1015   Alive     No  38
    ## 1016   Alive    Yes  45
    ## 1017    Dead    Yes  81
    ## 1018   Alive    Yes  66
    ## 1019   Alive     No  33
    ## 1020   Alive    Yes  32
    ## 1021   Alive     No  29
    ## 1022   Alive    Yes  53
    ## 1023   Alive     No  21
    ## 1024   Alive     No  61
    ## 1025   Alive     No  24
    ## 1026    Dead     No  54
    ## 1027   Alive    Yes  46
    ## 1028   Alive     No  58
    ## 1029    Dead     No  65
    ## 1030    Dead     No  82
    ## 1031   Alive     No  53
    ## 1032    Dead     No  33
    ## 1033   Alive    Yes  49
    ## 1034   Alive     No  42
    ## 1035   Alive    Yes  43
    ## 1036   Alive    Yes  20
    ## 1037    Dead     No  56
    ## 1038    Dead     No  82
    ## 1039    Dead     No  72
    ## 1040   Alive     No  20
    ## 1041    Dead     No  82
    ## 1042   Alive     No  62
    ## 1043   Alive    Yes  54
    ## 1044   Alive     No  54
    ## 1045    Dead    Yes  82
    ## 1046   Alive    Yes  47
    ## 1047   Alive    Yes  18
    ## 1048    Dead     No  72
    ## 1049   Alive    Yes  35
    ## 1050    Dead     No  69
    ## 1051   Alive     No  52
    ## 1052    Dead     No  59
    ## 1053    Dead    Yes  52
    ## 1054   Alive    Yes  29
    ## 1055   Alive     No  21
    ## 1056   Alive     No  28
    ## 1057   Alive    Yes  25
    ## 1058   Alive    Yes  25
    ## 1059   Alive    Yes  58
    ## 1060   Alive    Yes  43
    ## 1061   Alive     No  62
    ## 1062    Dead     No  67
    ## 1063   Alive    Yes  48
    ## 1064   Alive    Yes  54
    ## 1065   Alive    Yes  42
    ## 1066   Alive     No  66
    ## 1067   Alive    Yes  23
    ## 1068   Alive    Yes  67
    ## 1069   Alive     No  47
    ## 1070   Alive     No  28
    ## 1071   Alive     No  38
    ## 1072   Alive     No  64
    ## 1073    Dead     No  57
    ## 1074   Alive     No  56
    ## 1075   Alive     No  34
    ## 1076   Alive    Yes  50
    ## 1077   Alive     No  58
    ## 1078   Alive     No  32
    ## 1079    Dead     No  77
    ## 1080   Alive    Yes  23
    ## 1081   Alive    Yes  36
    ## 1082    Dead     No  18
    ## 1083   Alive     No  47
    ## 1084    Dead     No  70
    ## 1085    Dead    Yes  60
    ## 1086    Dead     No  71
    ## 1087    Dead    Yes  56
    ## 1088   Alive    Yes  32
    ## 1089   Alive     No  28
    ## 1090   Alive     No  51
    ## 1091   Alive     No  37
    ## 1092   Alive     No  30
    ## 1093    Dead     No  77
    ## 1094    Dead     No  70
    ## 1095   Alive     No  41
    ## 1096   Alive     No  32
    ## 1097   Alive    Yes  48
    ## 1098   Alive     No  53
    ## 1099    Dead    Yes  65
    ## 1100    Dead     No  66
    ## 1101    Dead    Yes  59
    ## 1102    Dead     No  60
    ## 1103    Dead    Yes  58
    ## 1104   Alive     No  64
    ## 1105   Alive    Yes  61
    ## 1106    Dead     No  62
    ## 1107   Alive     No  58
    ## 1108   Alive     No  37
    ## 1109    Dead    Yes  53
    ## 1110   Alive     No  65
    ## 1111   Alive     No  27
    ## 1112    Dead     No  72
    ## 1113    Dead    Yes  60
    ## 1114   Alive    Yes  45
    ## 1115   Alive    Yes  63
    ## 1116   Alive     No  32
    ## 1117   Alive     No  18
    ## 1118   Alive    Yes  49
    ## 1119   Alive     No  34
    ## 1120   Alive     No  23
    ## 1121   Alive     No  61
    ## 1122   Alive     No  44
    ## 1123   Alive     No  66
    ## 1124   Alive     No  28
    ## 1125   Alive    Yes  44
    ## 1126   Alive    Yes  29
    ## 1127   Alive    Yes  38
    ## 1128   Alive    Yes  52
    ## 1129   Alive    Yes  54
    ## 1130   Alive    Yes  63
    ## 1131    Dead    Yes  45
    ## 1132    Dead     No  74
    ## 1133   Alive     No  24
    ## 1134   Alive     No  40
    ## 1135   Alive     No  28
    ## 1136   Alive    Yes  30
    ## 1137   Alive    Yes  43
    ## 1138    Dead    Yes  63
    ## 1139   Alive    Yes  33
    ## 1140   Alive    Yes  41
    ## 1141   Alive     No  60
    ## 1142   Alive    Yes  31
    ## 1143   Alive    Yes  47
    ## 1144    Dead     No  81
    ## 1145    Dead     No  78
    ## 1146   Alive     No  56
    ## 1147    Dead     No  73
    ## 1148   Alive     No  56
    ## 1149   Alive    Yes  28
    ## 1150   Alive    Yes  22
    ## 1151   Alive     No  34
    ## 1152   Alive     No  48
    ## 1153   Alive     No  27
    ## 1154   Alive    Yes  47
    ## 1155   Alive     No  31
    ## 1156   Alive     No  47
    ## 1157    Dead     No  60
    ## 1158    Dead     No  59
    ## 1159    Dead     No  80
    ## 1160   Alive    Yes  41
    ## 1161    Dead    Yes  46
    ## 1162   Alive    Yes  50
    ## 1163   Alive     No  48
    ## 1164   Alive     No  37
    ## 1165   Alive    Yes  50
    ## 1166   Alive    Yes  25
    ## 1167   Alive     No  25
    ## 1168   Alive     No  41
    ## 1169   Alive     No  24
    ## 1170   Alive    Yes  57
    ## 1171    Dead     No  71
    ## 1172   Alive    Yes  26
    ## 1173    Dead     No  32
    ## 1174   Alive     No  25
    ## 1175    Dead     No  84
    ## 1176   Alive    Yes  19
    ## 1177   Alive     No  26
    ## 1178   Alive    Yes  58
    ## 1179    Dead    Yes  67
    ## 1180   Alive     No  24
    ## 1181    Dead    Yes  74
    ## 1182   Alive    Yes  58
    ## 1183   Alive     No  57
    ## 1184   Alive    Yes  52
    ## 1185   Alive    Yes  42
    ## 1186   Alive     No  42
    ## 1187    Dead     No  69
    ## 1188    Dead    Yes  39
    ## 1189    Dead    Yes  84
    ## 1190   Alive    Yes  57
    ## 1191   Alive     No  42
    ## 1192   Alive     No  35
    ## 1193   Alive     No  23
    ## 1194   Alive     No  65
    ## 1195   Alive    Yes  28
    ## 1196   Alive     No  39
    ## 1197   Alive    Yes  36
    ## 1198   Alive     No  40
    ## 1199   Alive     No  39
    ## 1200   Alive     No  19
    ## 1201   Alive     No  61
    ## 1202   Alive    Yes  37
    ## 1203   Alive    Yes  52
    ## 1204   Alive    Yes  62
    ## 1205   Alive     No  30
    ## 1206   Alive    Yes  23
    ## 1207    Dead    Yes  72
    ## 1208   Alive    Yes  46
    ## 1209   Alive     No  33
    ## 1210   Alive     No  32
    ## 1211   Alive     No  27
    ## 1212    Dead    Yes  72
    ## 1213    Dead    Yes  53
    ## 1214    Dead    Yes  58
    ## 1215   Alive    Yes  32
    ## 1216   Alive     No  25
    ## 1217    Dead     No  71
    ## 1218   Alive    Yes  37
    ## 1219   Alive    Yes  41
    ## 1220    Dead     No  75
    ## 1221   Alive     No  29
    ## 1222    Dead    Yes  62
    ## 1223   Alive    Yes  30
    ## 1224   Alive     No  20
    ## 1225   Alive    Yes  26
    ## 1226    Dead     No  60
    ## 1227    Dead     No  65
    ## 1228    Dead    Yes  63
    ## 1229   Alive     No  19
    ## 1230    Dead    Yes  55
    ## 1231   Alive    Yes  45
    ## 1232   Alive     No  28
    ## 1233   Alive    Yes  21
    ## 1234   Alive     No  35
    ## 1235   Alive     No  24
    ## 1236    Dead     No  74
    ## 1237    Dead     No  63
    ## 1238   Alive     No  44
    ## 1239   Alive    Yes  26
    ## 1240    Dead     No  84
    ## 1241   Alive    Yes  27
    ## 1242   Alive     No  28
    ## 1243    Dead    Yes  60
    ## 1244   Alive     No  44
    ## 1245    Dead     No  82
    ## 1246    Dead     No  29
    ## 1247   Alive    Yes  52
    ## 1248   Alive    Yes  42
    ## 1249   Alive    Yes  18
    ## 1250    Dead     No  61
    ## 1251    Dead     No  60
    ## 1252   Alive    Yes  19
    ## 1253    Dead     No  84
    ## 1254    Dead     No  72
    ## 1255   Alive     No  59
    ## 1256    Dead     No  78
    ## 1257   Alive     No  41
    ## 1258   Alive     No  21
    ## 1259   Alive     No  31
    ## 1260    Dead     No  83
    ## 1261   Alive     No  62
    ## 1262    Dead     No  76
    ## 1263    Dead    Yes  63
    ## 1264   Alive     No  44
    ## 1265    Dead     No  65
    ## 1266   Alive     No  33
    ## 1267   Alive    Yes  40
    ## 1268   Alive     No  50
    ## 1269    Dead     No  74
    ## 1270   Alive     No  19
    ## 1271   Alive     No  50
    ## 1272    Dead    Yes  62
    ## 1273   Alive    Yes  25
    ## 1274    Dead    Yes  66
    ## 1275   Alive     No  67
    ## 1276   Alive    Yes  18
    ## 1277   Alive    Yes  28
    ## 1278   Alive    Yes  49
    ## 1279   Alive    Yes  39
    ## 1280   Alive    Yes  33
    ## 1281   Alive     No  44
    ## 1282   Alive     No  48
    ## 1283   Alive    Yes  37
    ## 1284    Dead    Yes  71
    ## 1285    Dead     No  66
    ## 1286   Alive    Yes  34
    ## 1287    Dead    Yes  77
    ## 1288   Alive     No  48
    ## 1289   Alive    Yes  60
    ## 1290   Alive     No  41
    ## 1291   Alive     No  55
    ## 1292   Alive    Yes  46
    ## 1293   Alive     No  55
    ## 1294   Alive     No  20
    ## 1295    Dead    Yes  80
    ## 1296   Alive    Yes  57
    ## 1297   Alive     No  52
    ## 1298   Alive    Yes  48
    ## 1299   Alive     No  30
    ## 1300   Alive    Yes  57
    ## 1301   Alive    Yes  37
    ## 1302   Alive     No  27
    ## 1303   Alive     No  37
    ## 1304   Alive     No  36
    ## 1305   Alive     No  23
    ## 1306    Dead     No  55
    ## 1307   Alive     No  40
    ## 1308   Alive     No  33
    ## 1309   Alive     No  23
    ## 1310   Alive    Yes  35
    ## 1311   Alive     No  33
    ## 1312   Alive    Yes  21
    ## 1313   Alive     No  46
    ## 1314   Alive    Yes  41

``` r
# What type of study do you think these data come from: observational or experiment? Observational. Since this study is about smoking, I don't think any reasonable IRB would pass a study that asked participants to smoke or randomly assigned them to smoke or not.

# How many observations are in this dataset? What does each observation represent? 1314 overcations, each row (observation) represent a participant. 

# How many variables are in this dataset? What type of variable is each? 3 variables, each representing their health outcome (alive/dead), age, and whether or not they smoke. age is numeric variable, other two are categorical variable.

# Display each variable using an appropriate visualization.
# we can either display each variable separately.
# but I think maybe we 're more intersted to konw the relationship between smoking status and healthoutcome, so I display these two in one graph. 

# distribution of age
ggplot(Whickham, aes(x = age)) +
  geom_histogram(fill = "skyblue") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Frequency") 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-06_files/figure-gfm/Stretch%20Practice%20with%20Smokers%20in%20Whickham-1.png)<!-- -->

``` r
# distribution of smoking status
ggplot(Whickham, aes(x = smoker)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Smoking Status Distribution",
       x = "Smoking Status",
       y = "Count") 
```

![](lab-06_files/figure-gfm/Stretch%20Practice%20with%20Smokers%20in%20Whickham-2.png)<!-- -->

``` r
# distribution of health outcome
ggplot(Whickham, aes(x = outcome)) +
  geom_bar(fill = "pink") +
  labs(title = "Health Outcome Distribution",
       x = "Health Outcome",
       y = "Count") 
```

![](lab-06_files/figure-gfm/Stretch%20Practice%20with%20Smokers%20in%20Whickham-3.png)<!-- -->

``` r
# distribution of smoker Vs. health outcome. 
 ggplot(Whickham, aes(x = smoker, fill = outcome)) + 
        geom_bar(position = "dodge") +
   labs(title= "distribution of smoker Vs. health outcome")
```

![](lab-06_files/figure-gfm/Stretch%20Practice%20with%20Smokers%20in%20Whickham-4.png)<!-- -->

``` r
 # learn something new~ dodge: placed the variable side-by-side.
 
# What would you expect the relationship between smoking status and health outcome to be? The longer the smoking history, the earlier the death. or people who smoke die earlier.
```

### Exercise 5

``` r
# Create a visualization depicting the relationship between smoking status and health outcome. (above⬆️)
# the data did shows that there are more non-smokers alive.

#calculate the relevant conditional probabilities 
Whickham %>%
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(prop = n / sum(n))
```

    ## # A tibble: 4 × 4
    ## # Groups:   smoker [2]
    ##   smoker outcome     n  prop
    ##   <fct>  <fct>   <int> <dbl>
    ## 1 No     Alive     502 0.686
    ## 2 No     Dead      230 0.314
    ## 3 Yes    Alive     443 0.761
    ## 4 Yes    Dead      139 0.239

### Exercise 7

``` r
Whickham<- Whickham %>%
  mutate(age_cat = case_when(
    age <= 44 ~ "18-44",
    age > 44 & age <= 64 ~ "45-64",
    age > 64 ~ "65+"
  ))

# display data by age_cat
Whickham %>%
  count(smoker, age_cat, outcome) %>%
  ggplot(aes(smoker, n, fill = outcome)) +
  geom_col(position = "fill") +
  facet_wrap(~ age_cat) +
  labs(title = "Smoking vs. Health Outcome by Age",
       y = "Proportion",
       x = "Smoker") 
```

![](lab-06_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# The data is much more clearly distributed, and it is possible to see that the mortality rate actually rises with age.
```

…
