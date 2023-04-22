---
title: "An Introduction to Tablet for HTML"
author: "Tim Bergsma"
date: "2023-04-22"
urlcolor: blue
output: 
  rmarkdown::html_document:
    toc: true
    keep_md: true
vignette: >
  %\VignetteIndexEntry{An Introduction to Tablet for HTML}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---



# Motivation

Occasionally it is useful to generate a table of 
summary statistics for rows of a dataset, where
such rows represent sampling units and and columns
may be categorical or continuous.  The excellent R package [table1](https://CRAN.R-project.org/package=table1)
does exactly this, and was the inspiration for 
```tablet```.  ```table1``` however is optimized
for html; ```tablet``` tries to provide
a format-neutral implementation and relies
on [kableExtra](https://CRAN.R-project.org/package=kableExtra) to handle the rendering.
Support for pdf (latex) is of particular interest,
and is illustrated in the companion vignette. 
Here we convert that presentation to html for completeness.
If you only need html output, you may prefer the interface,
flexibility, and styling options of ```table1```.


# Software
To support our examples, we load some other packages
and in particular locate the melanoma dataset from
[boot](https://CRAN.R-project.org/package=boot).





```r
library(tidyr)
library(dplyr)
library(magrittr)
library(kableExtra)
library(boot)
library(yamlet)
library(tablet)
```



```r
x <- melanoma
x %<>% select(-time, -year)
```

# Simple Case
For starters, we'll just coerce two variables to factor
to show that they are categorical, and then pass the whole
thing to tablet(). Then we forward to as_kable() for rendering
(calls kableExtra::kbl and adds some magic).


```r
x %>%
  mutate(
    sex = factor(sex), 
    ulcer = factor(ulcer)
  ) %>%
  tablet %>%
  as_kable
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> All<br>(N = 205) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="3" style="border-bottom: 1px solid;"><strong>status</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 1.79 (0.551) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 (1, 3) </td>
  </tr>
  <tr grouplength="2"><td colspan="3" style="border-bottom: 1px solid;"><strong>sex</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 126 (61.5%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 79 (38.5%) </td>
  </tr>
  <tr grouplength="2"><td colspan="3" style="border-bottom: 1px solid;"><strong>age</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 54 (4, 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="3" style="border-bottom: 1px solid;"><strong>thickness</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 1.94 (0.1, 17.4) </td>
  </tr>
  <tr grouplength="2"><td colspan="3" style="border-bottom: 1px solid;"><strong>ulcer</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 115 (56.1%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> 90 (43.9%) </td>
  </tr>
</tbody>
</table>

# With Metadata

Now we redefine the dataset, supplying metadata almost verbatim from ```?melanoma```.
This is fairly easy using package ```yamlet```. Note that we reverse
the authors' factor order of 1, 0 for ulcer and move status 'Alive' to 
first position.


```r
x <- melanoma

x %<>% decorate('
time:      [ Survival Time Since Operation, day ]
status:
 - End of Study Patient Status
 -
  - Alive: 2
  - Melanoma Death: 1
  - Unrelated Death: 3
sex:       [ Sex, [ Male: 1, Female: 0 ]]
age:       [ Age at Time of Operation, year ]
year:      [ Year of Operation, year ]
thickness: [ Tumor Thickness, mm ]
ulcer:     [ Ulceration, [ Absent: 0, Present: 1 ]]
')
x %<>% select(-time, -year)
x %<>% group_by(status)
x %<>% resolve
```

* group_by(status) causes statistics to be summarized
in columns by group.  

* resolve() disambiguates 
labels, units, and factor levels (actually creating 
factors where appropriate, such as for sex and ulcer).

* alias() substitutes labels for column names.

Now we pass x to tablet() and as_kable() for a more
informative result.


```r
x %>% tablet %>% as_kable
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> Alive<br>(N = 134) </th>
   <th style="text-align:left;"> Melanoma Death<br>(N = 57) </th>
   <th style="text-align:left;"> Unrelated Death<br>(N = 14) </th>
   <th style="text-align:left;"> All<br>(N = 205) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Sex</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Male </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 43 (32.1%) </td>
   <td style="text-align:left;"> 29 (50.9%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 79 (38.5%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Female </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 91 (67.9%) </td>
   <td style="text-align:left;"> 28 (49.1%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 126 (61.5%) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Age at Time of Operation (year)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 50 (15.9) </td>
   <td style="text-align:left;"> 55.1 (17.9) </td>
   <td style="text-align:left;"> 65.3 (10.9) </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 52 (4, 84) </td>
   <td style="text-align:left;"> 56 (14, 95) </td>
   <td style="text-align:left;"> 65 (49, 86) </td>
   <td style="text-align:left;"> 54 (4, 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Tumor Thickness (mm)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2.24 (2.33) </td>
   <td style="text-align:left;"> 4.31 (3.57) </td>
   <td style="text-align:left;"> 3.72 (3.63) </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 1.36 (0.1, 12.9) </td>
   <td style="text-align:left;"> 3.54 (0.32, 17.4) </td>
   <td style="text-align:left;"> 2.26 (0.16, 12.6) </td>
   <td style="text-align:left;"> 1.94 (0.1, 17.4) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Ulceration</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Absent </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 92 (68.7%) </td>
   <td style="text-align:left;"> 16 (28.1%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 115 (56.1%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Present </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> 42 (31.3%) </td>
   <td style="text-align:left;"> 41 (71.9%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 90 (43.9%) </td>
  </tr>
</tbody>
</table>

Notice that:

* the order of variables down the left side is exactly their order in the dataset;
* the order of factor levels is exactly that in x;
* the order of groups across the top is exactly the levels (if any) of the grouping variable(s), and
* labels and titles (highest priority) are substituted for item names.

If you don't particularly care for some aspect of the
presentation, you can jump in between tablet() and as_kable()
to fix things up.  For example, if you don't want the "All"
column you can just say 

* ```x %>% tablet %>% select(-All) %>% as_kable```.  

If you **only** want the the "All" column,
you can just remove the group(s): 

* ```x %>% ungroup %>% select(-1) %>% tablet %>% as_kable```.

By the way, you can also pass ```all = NULL``` to suppress the 'All' column.

# Grouped Columns

In tablet(), most columns are the consequences of a grouping
variable.  Not surprisingly, grouped columns are just 
a consequence of nested grouping variables.  To illustrate,
we follow the [table1 vignette](https://CRAN.R-project.org/package=table1) by adding a grouping variable that
groups the two kinds of death.


```r
x %<>% mutate(class = status)                          # copy the current group
x %<>% modify(class, label = 'class')                  # change its label
levels(x$status) <- c('Alive','Melanoma','Unrelated')  # tweak current group
levels(x$class)  <- c(' ',    'Death',   'Death')      # cluster groups
x %<>% group_by(class, status)                         # nest groups
x %>% tablet %>% as_kable                              # render
```

<table>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="3"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Death</div></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> Alive<br>(N = 134) </th>
   <th style="text-align:left;"> Melanoma<br>(N = 57) </th>
   <th style="text-align:left;"> Unrelated<br>(N = 14) </th>
   <th style="text-align:left;"> All<br>(N = 205) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Sex</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Male </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 43 (32.1%) </td>
   <td style="text-align:left;"> 29 (50.9%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 79 (38.5%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Female </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 91 (67.9%) </td>
   <td style="text-align:left;"> 28 (49.1%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 126 (61.5%) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Age at Time of Operation (year)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 50 (15.9) </td>
   <td style="text-align:left;"> 55.1 (17.9) </td>
   <td style="text-align:left;"> 65.3 (10.9) </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 52 (4, 84) </td>
   <td style="text-align:left;"> 56 (14, 95) </td>
   <td style="text-align:left;"> 65 (49, 86) </td>
   <td style="text-align:left;"> 54 (4, 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Tumor Thickness (mm)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2.24 (2.33) </td>
   <td style="text-align:left;"> 4.31 (3.57) </td>
   <td style="text-align:left;"> 3.72 (3.63) </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 1.36 (0.1, 12.9) </td>
   <td style="text-align:left;"> 3.54 (0.32, 17.4) </td>
   <td style="text-align:left;"> 2.26 (0.16, 12.6) </td>
   <td style="text-align:left;"> 1.94 (0.1, 17.4) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Ulceration</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Absent </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 92 (68.7%) </td>
   <td style="text-align:left;"> 16 (28.1%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 115 (56.1%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Present </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> 42 (31.3%) </td>
   <td style="text-align:left;"> 41 (71.9%) </td>
   <td style="text-align:left;"> 7 (50%) </td>
   <td style="text-align:left;"> 90 (43.9%) </td>
  </tr>
</tbody>
</table>

# Transposed Groups

Categorical observations (in principle) and grouping variables
are all factors, and are thus transposable.
To illustrate, we drop the column group above and instead
nest sex within status ...


```r
x %<>% group_by(status, sex)
x %<>% select(-class)
x %>% 
  tablet %>% 
  as_kable
```

<table>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="2"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Alive</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Melanoma</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Unrelated</div></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> Male<br>(N = 43) </th>
   <th style="text-align:left;"> Female<br>(N = 91) </th>
   <th style="text-align:left;"> Male<br>(N = 29) </th>
   <th style="text-align:left;"> Female<br>(N = 28) </th>
   <th style="text-align:left;"> Male<br>(N = 7) </th>
   <th style="text-align:left;"> Female<br>(N = 7) </th>
   <th style="text-align:left;"> All<br>(N = 205) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="9" style="border-bottom: 1px solid;"><strong>Age at Time of Operation (year)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 52.5 (16.9) </td>
   <td style="text-align:left;"> 48.8 (15.4) </td>
   <td style="text-align:left;"> 53.9 (19.7) </td>
   <td style="text-align:left;"> 56.4 (16.2) </td>
   <td style="text-align:left;"> 62.4 (11.2) </td>
   <td style="text-align:left;"> 68.1 (10.6) </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 55 (12, 84) </td>
   <td style="text-align:left;"> 49 (4, 77) </td>
   <td style="text-align:left;"> 52 (19, 95) </td>
   <td style="text-align:left;"> 58 (14, 89) </td>
   <td style="text-align:left;"> 64 (49, 76) </td>
   <td style="text-align:left;"> 66 (54, 86) </td>
   <td style="text-align:left;"> 54 (4, 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="9" style="border-bottom: 1px solid;"><strong>Tumor Thickness (mm)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 2.73 (2.49) </td>
   <td style="text-align:left;"> 2.02 (2.22) </td>
   <td style="text-align:left;"> 4.63 (3.47) </td>
   <td style="text-align:left;"> 3.99 (3.71) </td>
   <td style="text-align:left;"> 4.83 (4.19) </td>
   <td style="text-align:left;"> 2.6 (2.84) </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1.62 (0.16, 8.38) </td>
   <td style="text-align:left;"> 1.29 (0.1, 12.9) </td>
   <td style="text-align:left;"> 4.04 (0.81, 14.7) </td>
   <td style="text-align:left;"> 3.14 (0.32, 17.4) </td>
   <td style="text-align:left;"> 4.84 (0.65, 12.6) </td>
   <td style="text-align:left;"> 1.45 (0.16, 8.54) </td>
   <td style="text-align:left;"> 1.94 (0.1, 17.4) </td>
  </tr>
  <tr grouplength="2"><td colspan="9" style="border-bottom: 1px solid;"><strong>Ulceration</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Absent </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 24 (55.8%) </td>
   <td style="text-align:left;"> 68 (74.7%) </td>
   <td style="text-align:left;"> 8 (27.6%) </td>
   <td style="text-align:left;"> 8 (28.6%) </td>
   <td style="text-align:left;"> 4 (57.1%) </td>
   <td style="text-align:left;"> 3 (42.9%) </td>
   <td style="text-align:left;"> 115 (56.1%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Present </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 19 (44.2%) </td>
   <td style="text-align:left;"> 23 (25.3%) </td>
   <td style="text-align:left;"> 21 (72.4%) </td>
   <td style="text-align:left;"> 20 (71.4%) </td>
   <td style="text-align:left;"> 3 (42.9%) </td>
   <td style="text-align:left;"> 4 (57.1%) </td>
   <td style="text-align:left;"> 90 (43.9%) </td>
  </tr>
</tbody>
</table>

... or nest ulceration within status ...


```r
x %<>% group_by(status, ulcer)
x %>% 
  tablet %>% 
  as_kable
```

<table>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="2"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Alive</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Melanoma</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Unrelated</div></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> Absent<br>(N = 92) </th>
   <th style="text-align:left;"> Present<br>(N = 42) </th>
   <th style="text-align:left;"> Absent<br>(N = 16) </th>
   <th style="text-align:left;"> Present<br>(N = 41) </th>
   <th style="text-align:left;"> Absent<br>(N = 7) </th>
   <th style="text-align:left;"> Present<br>(N = 7) </th>
   <th style="text-align:left;"> All<br>(N = 205) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="9" style="border-bottom: 1px solid;"><strong>Sex</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Male </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 24 (26.1%) </td>
   <td style="text-align:left;"> 19 (45.2%) </td>
   <td style="text-align:left;"> 8 (50%) </td>
   <td style="text-align:left;"> 21 (51.2%) </td>
   <td style="text-align:left;"> 4 (57.1%) </td>
   <td style="text-align:left;"> 3 (42.9%) </td>
   <td style="text-align:left;"> 79 (38.5%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Female </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 68 (73.9%) </td>
   <td style="text-align:left;"> 23 (54.8%) </td>
   <td style="text-align:left;"> 8 (50%) </td>
   <td style="text-align:left;"> 20 (48.8%) </td>
   <td style="text-align:left;"> 3 (42.9%) </td>
   <td style="text-align:left;"> 4 (57.1%) </td>
   <td style="text-align:left;"> 126 (61.5%) </td>
  </tr>
  <tr grouplength="2"><td colspan="9" style="border-bottom: 1px solid;"><strong>Age at Time of Operation (year)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 49.3 (15.4) </td>
   <td style="text-align:left;"> 51.6 (17.1) </td>
   <td style="text-align:left;"> 54.9 (19.9) </td>
   <td style="text-align:left;"> 55.1 (17.4) </td>
   <td style="text-align:left;"> 58.4 (8.66) </td>
   <td style="text-align:left;"> 72.1 (8.53) </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 50 (4, 83) </td>
   <td style="text-align:left;"> 54.5 (12, 84) </td>
   <td style="text-align:left;"> 59 (16, 83) </td>
   <td style="text-align:left;"> 56 (14, 95) </td>
   <td style="text-align:left;"> 56 (49, 71) </td>
   <td style="text-align:left;"> 72 (60, 86) </td>
   <td style="text-align:left;"> 54 (4, 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="9" style="border-bottom: 1px solid;"><strong>Tumor Thickness (mm)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1.63 (1.93) </td>
   <td style="text-align:left;"> 3.58 (2.58) </td>
   <td style="text-align:left;"> 2.7 (3.35) </td>
   <td style="text-align:left;"> 4.94 (3.5) </td>
   <td style="text-align:left;"> 2.1 (1.93) </td>
   <td style="text-align:left;"> 5.34 (4.33) </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1.13 (0.1, 12.9) </td>
   <td style="text-align:left;"> 3.06 (0.32, 12.2) </td>
   <td style="text-align:left;"> 1.94 (0.32, 14.7) </td>
   <td style="text-align:left;"> 4.04 (0.97, 17.4) </td>
   <td style="text-align:left;"> 1.45 (0.65, 6.12) </td>
   <td style="text-align:left;"> 4.84 (0.16, 12.6) </td>
   <td style="text-align:left;"> 1.94 (0.1, 17.4) </td>
  </tr>
</tbody>
</table>

... or where it makes sense, use multiple levels of nesting.


```r
x %<>% group_by(status, ulcer, sex)
x %>% 
  tablet %>% 
  as_kable
```

<table>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="2"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Alive</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Melanoma</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Unrelated</div></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
</tr>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="2"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Absent</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Present</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Absent</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Present</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Absent</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Present</div></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> Male<br>(N = 24) </th>
   <th style="text-align:left;"> Female<br>(N = 68) </th>
   <th style="text-align:left;"> Male<br>(N = 19) </th>
   <th style="text-align:left;"> Female<br>(N = 23) </th>
   <th style="text-align:left;"> Male<br>(N = 8) </th>
   <th style="text-align:left;"> Female<br>(N = 8) </th>
   <th style="text-align:left;"> Male<br>(N = 21) </th>
   <th style="text-align:left;"> Female<br>(N = 20) </th>
   <th style="text-align:left;"> Male<br>(N = 4) </th>
   <th style="text-align:left;"> Female<br>(N = 3) </th>
   <th style="text-align:left;"> Male<br>(N = 3) </th>
   <th style="text-align:left;"> Female<br>(N = 4) </th>
   <th style="text-align:left;"> All<br>(N = 205) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="15" style="border-bottom: 1px solid;"><strong>Age at Time of Operation (year)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 50.4 (17) </td>
   <td style="text-align:left;"> 48.9 (14.9) </td>
   <td style="text-align:left;"> 55.3 (16.9) </td>
   <td style="text-align:left;"> 48.7 (17) </td>
   <td style="text-align:left;"> 55.2 (22.2) </td>
   <td style="text-align:left;"> 54.6 (18.8) </td>
   <td style="text-align:left;"> 53.3 (19.2) </td>
   <td style="text-align:left;"> 57 (15.5) </td>
   <td style="text-align:left;"> 54.5 (7.14) </td>
   <td style="text-align:left;"> 63.7 (8.74) </td>
   <td style="text-align:left;"> 73 (2.65) </td>
   <td style="text-align:left;"> 71.5 (11.8) </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 54 (15, 83) </td>
   <td style="text-align:left;"> 49 (4, 77) </td>
   <td style="text-align:left;"> 56 (12, 84) </td>
   <td style="text-align:left;"> 48 (19, 75) </td>
   <td style="text-align:left;"> 56 (27, 83) </td>
   <td style="text-align:left;"> 59 (16, 77) </td>
   <td style="text-align:left;"> 52 (19, 95) </td>
   <td style="text-align:left;"> 58 (14, 89) </td>
   <td style="text-align:left;"> 52.5 (49, 64) </td>
   <td style="text-align:left;"> 66 (54, 71) </td>
   <td style="text-align:left;"> 72 (71, 76) </td>
   <td style="text-align:left;"> 70 (60, 86) </td>
   <td style="text-align:left;"> 54 (4, 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="15" style="border-bottom: 1px solid;"><strong>Tumor Thickness (mm)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1.47 (1.72) </td>
   <td style="text-align:left;"> 1.69 (2) </td>
   <td style="text-align:left;"> 4.32 (2.42) </td>
   <td style="text-align:left;"> 2.97 (2.59) </td>
   <td style="text-align:left;"> 3.27 (4.68) </td>
   <td style="text-align:left;"> 2.14 (1.18) </td>
   <td style="text-align:left;"> 5.14 (2.86) </td>
   <td style="text-align:left;"> 4.72 (4.13) </td>
   <td style="text-align:left;"> 2.42 (2.5) </td>
   <td style="text-align:left;"> 1.67 (1.14) </td>
   <td style="text-align:left;"> 8.05 (4.02) </td>
   <td style="text-align:left;"> 3.3 (3.71) </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 0.97 (0.16, 7.09) </td>
   <td style="text-align:left;"> 1.29 (0.1, 12.9) </td>
   <td style="text-align:left;"> 3.87 (0.81, 8.38) </td>
   <td style="text-align:left;"> 1.94 (0.32, 12.2) </td>
   <td style="text-align:left;"> 1.78 (0.81, 14.7) </td>
   <td style="text-align:left;"> 2.02 (0.32, 3.56) </td>
   <td style="text-align:left;"> 4.83 (1.62, 12.9) </td>
   <td style="text-align:left;"> 3.54 (0.97, 17.4) </td>
   <td style="text-align:left;"> 1.46 (0.65, 6.12) </td>
   <td style="text-align:left;"> 1.45 (0.65, 2.9) </td>
   <td style="text-align:left;"> 6.76 (4.84, 12.6) </td>
   <td style="text-align:left;"> 2.26 (0.16, 8.54) </td>
   <td style="text-align:left;"> 1.94 (0.1, 17.4) </td>
  </tr>
</tbody>
</table>

# Aesthetics

```tablet``` tries to give rather exhaustive control
over formatting. Much can be achieved by replacing 
elements of 'fun', 'fac', 'num', and 'lab' (see ```?tablet.data.frame```).  For finer control,
you can replace these entirely.  In this example,
we will ...

* ignore categoricals (other than groups) by replacing 'fac' with something of length zero,

* drop the 'N =' header material by substituting in 'lab', and

* switch to '(min - max)' instead of '(min, max)'.


```r
x %<>% group_by(status)
x %>% 
  tablet(
    fac = NULL,
    lab ~ name,
    `Median (range)` ~ med + ' (' + min + ' - ' + max + ')'
  ) %>% 
  as_kable
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> _tablet_sort </th>
   <th style="text-align:left;"> Alive </th>
   <th style="text-align:left;"> Melanoma </th>
   <th style="text-align:left;"> Unrelated </th>
   <th style="text-align:left;"> All </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Age at Time of Operation (year)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 50 (15.9) </td>
   <td style="text-align:left;"> 55.1 (17.9) </td>
   <td style="text-align:left;"> 65.3 (10.9) </td>
   <td style="text-align:left;"> 52.5 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 52 (4 - 84) </td>
   <td style="text-align:left;"> 56 (14 - 95) </td>
   <td style="text-align:left;"> 65 (49 - 86) </td>
   <td style="text-align:left;"> 54 (4 - 95) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Tumor Thickness (mm)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2.24 (2.33) </td>
   <td style="text-align:left;"> 4.31 (3.57) </td>
   <td style="text-align:left;"> 3.72 (3.63) </td>
   <td style="text-align:left;"> 2.92 (2.96) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 1.36 (0.1 - 12.9) </td>
   <td style="text-align:left;"> 3.54 (0.32 - 17.4) </td>
   <td style="text-align:left;"> 2.26 (0.16 - 12.6) </td>
   <td style="text-align:left;"> 1.94 (0.1 - 17.4) </td>
  </tr>
</tbody>
</table>


# Note

The default presentation includes  "N = " under the header,
but also has percent characters in the table.  Considerable
gymnastics are required to make this work in latex.  For 
html, we simply replace the newline character (supplied by 'lab')
with a 'br' tag internally.

# Conclusion

```tablet``` gives a flexible way of summarizing tables
of observations.  It reacts to numeric columns, factors, and 
grouping variables.  Display order derives from 
the order of columns and factor levels in the data.
Result columns can be grouped arbitrarily deep by
supplying extra groups. 
Column labels and titles are respected.
Rendering is largely the 
responsibility of ```kableExtra``` and can be extended.
Further customization is possible by manipulating
data after calling tablet() but before calling as_kable().
Powerful results are possible with very little code.












