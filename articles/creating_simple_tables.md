# Creating simple tables

``` r

library(LStools)
```

*This vignette shows how to use the LStools package to create
composition tables of variables from educational data, whether used for
outputs or sanity checks within the SRS\`*

To use the function we require some raw data, where each row corresponds
to a unique student. We generate some data below where we have columns
for Free school meal (FSM) status, maths grades at KS1, KS2 and GCSE and
whether the student does a-level maths (or further maths).

``` r

set.seed(33)
KS1 = sample(c('3+','2A','2B', '2C-'), 10000, replace = T, prob = c(.21,.28,.26,.25))
KS2 = sample(c('6','5+', '5-','4','3-'), 10000, replace = T, prob = c(0.03, 0.15, 0.22, 0.46, 0.13))
GCSE = sample(c('9','8', '7', '6-'), 10000, replace = T, prob = c(0.03, 0.07, 0.10, 0.80))
A_level =  sample(c('Maths + Further',  'Maths', 'No Maths'), 10000, replace = T, prob = c(0.02, 0.09, 0.89))
FSM = sample(c('Yes', 'No'), 10000, replace = T, prob = c(.238, 1-.238))

raw = data.frame(FSM = factor(FSM),
                 KS1 = factor(KS1, levels = c('3+','2A','2B', '2C-')),
                 KS2 = factor(KS2, levels = c('6','5+', '5-','4','3-')),
                 GCSE= factor(GCSE, levels = c('9','8', '7', '6-')), 
                 A_level = factor(A_level, levels = c('Maths + Further',  'Maths', 'No Maths')))
```

Note that I have factored the data in such a way that the grades are
ordered from highest to lowest for each column (where applicable), this
isn’t required but will affect the ordering of values in the outputted
tables.

Generally, we work with pathways which is a simplified form of the raw
data consisting of only the unique combinations of values across all
columns together with the number of students who follow each set of
values.

``` r

pathways = LStools::raw_to_pathway(raw)
```

The first 10 rows of pathways are shown below

### Composition of columns

Within LStools
[`pathway_summary_columns()`](https://jake-powell.github.io/LStools/reference/pathway_summary_columns.md)
provides a useful sanity check is to look at the composition of each
column within pathways, to check if for errors potentially created
during the cleaning stage. The function creates a list of data frames
for each column within pathways detailing the number and percentage of
students split by the values in the column.

``` r

composition_columns = pathways |> LStools::pathway_summary_columns()
composition_columns
#> $FSM
#>     FSM No_students % (population)
#> 1    No        7653          76.53
#> 2   Yes        2347          23.47
#> 3 Total       10000           100%
#> 
#> $KS1
#>     KS1 No_students % (population)
#> 1    2A        2710           27.1
#> 2    2B        2724          27.24
#> 3   2C-        2475          24.75
#> 4    3+        2091          20.91
#> 5 Total       10000           100%
#> 
#> $KS2
#>     KS2 No_students % (population)
#> 1    3-        1277          12.77
#> 2     4        4650           46.5
#> 3    5-        2290           22.9
#> 4    5+        1494          14.94
#> 5     6         289           2.89
#> 6 Total       10000           100%
#> 
#> $GCSE
#>    GCSE No_students % (population)
#> 1    6-        7949          79.49
#> 2     7        1018          10.18
#> 3     8         748           7.48
#> 4     9         285           2.85
#> 5 Total       10000           100%
#> 
#> $A_level
#>           A_level No_students % (population)
#> 1           Maths         845           8.45
#> 2 Maths + Further         207           2.07
#> 3        No Maths        8948          89.48
#> 4           Total       10000           100%
```

------------------------------------------------------------------------

### Composition tables

A common output from the SRS will be composition tables of stages of the
maths pipeline. For example GCSE maths grades split by demographics of
students.

Within LStools
[`pathway_composition_tables()`](https://jake-powell.github.io/LStools/reference/pathway_composition_tables.md)
can be used to create the composition tables for multiple stages across
multiple demographic columns within pathways.

``` r

composition_by_FSM_KS1 = pathways |> pathway_composition_tables(describers = c('FSM','KS1'), stages = c('KS2', 'GCSE'))
composition_by_FSM_KS1
#> $KS2
#>   describer level   6   5+   5-    4  3- Total
#> 1       FSM    No 222 1161 1740 3565 965  7653
#> 2       FSM   Yes  67  333  550 1085 312  2347
#> 3       KS1    3+  58  317  460  992 264  2091
#> 4       KS1    2A  83  415  608 1242 362  2710
#> 5       KS1    2B  74  409  661 1241 339  2724
#> 6       KS1   2C-  74  353  561 1175 312  2475
#> 
#> $GCSE
#>   describer level   9   8   7   6- Total
#> 1       FSM    No 212 563 764 6114  7653
#> 2       FSM   Yes  73 185 254 1835  2347
#> 3       KS1    3+  59 151 212 1669  2091
#> 4       KS1    2A  85 202 282 2141  2710
#> 5       KS1    2B  71 211 258 2184  2724
#> 6       KS1   2C-  70 184 266 1955  2475
```

Prior to outputting from the SRS we need to apply statistical disclosure
control which can be turned on via a toggle in
[`pathway_composition_tables()`](https://jake-powell.github.io/LStools/reference/pathway_composition_tables.md).

``` r

composition_by_FSM_KS1_SDC = pathways |> pathway_composition_tables(describers = c('FSM','KS1'), stages = c('KS2', 'GCSE'),do_SDC = TRUE) # By default SDC is turned off (FALSE).
composition_by_FSM_KS1_SDC
#> $KS2
#>   describer level   6   5+   5-    4  3- Total
#> 1       FSM    No 220 1160 1740 3560 960  7650
#> 2       FSM   Yes  70  330  550 1080 310  2350
#> 3       KS1    3+  60  320  460  990 260  2090
#> 4       KS1    2A  80  420  610 1240 360  2710
#> 5       KS1    2B  70  410  660 1240 340  2720
#> 6       KS1   2C-  70  350  560 1180 310  2480
#> 
#> $GCSE
#>   describer level   9   8   7   6- Total
#> 1       FSM    No 210 560 760 6110  7650
#> 2       FSM   Yes  70 180 250 1840  2350
#> 3       KS1    3+  60 150 210 1670  2090
#> 4       KS1    2A  80 200 280 2140  2710
#> 5       KS1    2B  70 210 260 2180  2720
#> 6       KS1   2C-  70 180 270 1960  2480
```

We can then save the composition table to excel ready to be checked for
output. We will use
[`data_frame_to_workbook()`](https://jake-powell.github.io/LStools/reference/data_frame_to_workbook.md)
to allow each stage of the composition table to be unique sheets within
an xlsx workbook each with an individual title and source information.

``` r

data_frame_to_workbook(data = composition_by_FSM_KS1_SDC$KS2,
                            sheet = 'KS2',
                            title = 'Composition of KS2 maths grade by FSM status and KS1 maths grade',
                            source = 'Source: National Pupil Database') |>
  data_frame_to_workbook(data = composition_by_FSM_KS1_SDC$GCSE,
                              sheet = 'GCSE',
                              title = 'Composition of GCSE maths grade by FSM status and KS1 maths grade',
                              source = 'Source: National Pupil Database') |>
  openxlsx::saveWorkbook(file = 'composition.xlsx', overwrite = TRUE)
```

[`pathway_composition_tables()`](https://jake-powell.github.io/LStools/reference/pathway_composition_tables.md)
can also be used across multiple years of data allowing trends in
composition to be explored. For example if our raw data had another
column for the year the student was in year 11 (final year of secondary
school)

``` r

set.seed(33)
KS1 = sample(c('3+','2A','2B', '2C-'), 10000, replace = T, prob = c(.21,.28,.26,.25))
KS2 = sample(c('6','5+', '5-','4','3-'), 10000, replace = T, prob = c(0.03, 0.15, 0.22, 0.46, 0.13))
GCSE = sample(c('9','8', '7', '6-'), 10000, replace = T, prob = c(0.03, 0.07, 0.10, 0.80))
A_level =  sample(c('Maths + Further',  'Maths', 'No Maths'), 10000, replace = T, prob = c(0.02, 0.09, 0.89))
FSM = sample(c('Yes', 'No'), 10000, replace = T, prob = c(.238, 1-.238))
IDACI = sample(c('1st','2nd','3rd', '4th', '5th'), 10000, replace = T)
Year = sample(c('2022','2023'), 10000, replace = T, prob = c(.45, .55))
KS1[sample(1:10000,50)] = NA
raw_years = data.frame(Year = factor(Year),
                 FSM = factor(FSM),
                 IDACI = factor(IDACI),
                 KS1 = factor(KS1, levels = c('3+','2A','2B', '2C-')),
                 KS2 = factor(KS2, levels = c('6','5+', '5-','4','3-')),
                 GCSE= factor(GCSE, levels = c('9','8', '7', '6-')),
                 A_level = factor(A_level, levels = c('Maths + Further',  'Maths', 'No Maths'))
)

pathways_years = LStools::raw_to_pathway(raw_years)
```

we can use the variable `year_column` to specify the year in
[`pathway_composition_tables()`](https://jake-powell.github.io/LStools/reference/pathway_composition_tables.md)
resulting in the following composition tables being created (note that
this should only be used if the values (grades) remain the same between
the years)

``` r

composition_by_FSM_IDACI_2022_23 = pathways_years |> pathway_composition_tables(describers = c('FSM','IDACI'), stages = c('KS2', 'GCSE'),year_column = 'Year')
composition_by_FSM_IDACI_2022_23$KS2 # Look only at KS2 composition
#>    Year describer level   6  5+  5-    4  3- Total
#> 1  2022       FSM    No  90 524 786 1642 405  3447
#> 2  2022       FSM   Yes  33 153 237  508 151  1082
#> 3  2022     IDACI   1st  23 138 194  427 109   891
#> 4  2022     IDACI   2nd  26 130 211  442 110   919
#> 5  2022     IDACI   3rd  24 144 215  405  99   887
#> 6  2022     IDACI   4th  25 121 194  437 110   887
#> 7  2022     IDACI   5th  25 144 209  439 128   945
#> 8  2023       FSM    No 132 637 954 1923 560  4206
#> 9  2023       FSM   Yes  34 180 313  577 161  1265
#> 10 2023     IDACI   1st  42 171 253  514 156  1136
#> 11 2023     IDACI   2nd  29 152 249  506 144  1080
#> 12 2023     IDACI   3rd  31 145 259  469 164  1068
#> 13 2023     IDACI   4th  36 159 256  495 139  1085
#> 14 2023     IDACI   5th  28 190 250  516 118  1102
```

The table can then be used to look at trends over time of the
composition of stages in the pipeline. For example in our fake dataset
has the results of FSM students changed at KS2 between 2022 and 2023?

``` r

required_data = composition_by_FSM_IDACI_2022_23$KS2 |> dplyr::filter(describer == 'FSM' & level == 'Yes')
required_data[,4:8] = (required_data[,4:8] / required_data[,9] *100) |> round(digits = 2)
required_data
#>   Year describer level    6    5+    5-     4    3- Total
#> 1 2022       FSM   Yes 3.05 14.14 21.90 46.95 13.96  1082
#> 2 2023       FSM   Yes 2.69 14.23 24.74 45.61 12.73  1265
```

------------------------------------------------------------------------

### Composition of transitions

It might be of interest to explore the composition of students who
‘flow’ through a particular transition of the maths pipeline, such as
those who achieve level 6 at KS2 who go on to achieve a grade 6 or worse
at GCSE.

Within LStools
[`pathway_stage_transition_by_describer()`](https://jake-powell.github.io/LStools/reference/pathway_stage_transition_by_describer.md)
can be used to extract the demographics (or other quantiites) of
students at all transitions through the mathematics pipeline.

For example we can extract the FSM status of students through
transitions through KS1, KS2 and GCSE with the following code

``` r

transition_demographics = pathway_stage_transition_by_describer(pathways, describers = 'FSM', stages = c('KS1','KS2', 'GCSE'))
# transition_demographics contains 40 transitions in a list, show only two.
transition_demographics$`KS1: 3+ -> KS2: 6`
#> $FSM
#>     FSM No_students % (population)
#> 1    No          45          77.59
#> 2   Yes          13          22.41
#> 3 Total          58           100%
transition_demographics$`KS2: 5+ -> GCSE: 9`
#> $FSM
#>     FSM No_students % (population)
#> 1    No          23          74.19
#> 2   Yes           8          25.81
#> 3 Total          31           100%
```

By default the output is a list of transitions where each transition has
a list for the composition of each demographic. This format is useful
when wanting to concentrate on a single transition. To alter the output
format we can change the value of `return_type` to ‘compressed’ which
will instead return a data frame for each demographic showing the
composition of all the transitions. This format is easier to output from
the SRS.

``` r

transition_demographics = pathway_stage_transition_by_describer(pathways, describers = c('FSM', 'A_level'), stages = c('KS1','KS2', 'GCSE'), return_type = 'compressed')
lapply(transition_demographics, head)
#> $FSM
#>           Transition FSM: No FSM: Yes FSM: Total
#> 1  KS1: 3+ -> KS2: 6      45       13         58
#> 2 KS1: 3+ -> KS2: 5+     253       64        317
#> 3 KS1: 3+ -> KS2: 5-     345      115        460
#> 4  KS1: 3+ -> KS2: 4     749      243        992
#> 5 KS1: 3+ -> KS2: 3-     196       68        264
#> 6  KS1: 2A -> KS2: 6      60       23         83
#> 
#> $A_level
#>           Transition A_level: No Maths A_level: Maths + Further A_level: Maths
#> 1  KS1: 3+ -> KS2: 6                55                        2              1
#> 2 KS1: 3+ -> KS2: 5+               285                        9             23
#> 3 KS1: 3+ -> KS2: 5-               400                       10             50
#> 4  KS1: 3+ -> KS2: 4               891                       20             81
#> 5 KS1: 3+ -> KS2: 3-               235                        5             24
#> 6  KS1: 2A -> KS2: 6                67                        1             15
#>   A_level: Total
#> 1             58
#> 2            317
#> 3            460
#> 4            992
#> 5            264
#> 6             83
```

``` r

data_frame_to_workbook(data = transition_demographics$FSM,
                            sheet = 'FSM',
                            title = 'Transitions between KS1, KS2 and GCSE split by students FSM status',
                            source = 'Source: National Pupil Database') |>
  data_frame_to_workbook(data = transition_demographics$A_level,
                              sheet = 'A-level',
                              title = 'Transitions between KS1, KS2 and GCSE split by students A-level mathematics choices',
                              source = 'Source: National Pupil Database') |>
  openxlsx::saveWorkbook(file = 'transition_info.xlsx', overwrite = TRUE)
```
