# acutis

# acutis <img src="./hex/acutis-hex.png" align="right" width="120" alt="Hexagonal logo for the penguins package" /> 


`R` package providing useful tools for facilitating statistical analysis of data products. It provides the following functions for aiding in the munging and manipulation of standard data tables:
 - `affiche()` — displays an aesthetic table; from the French *affiche* to display
 - `count_na()` — generates a count table of missing values by column
 - `count_table()` — generates a standard count table for a given column
 - `describe()` — for summarizing dataset statistics; mimics `describe()` from {pandas}
 - `pasteurize()` — for cleaning a dataframe by standardizing column names and character fields
 - `fix_date_mdy()` — for converting MM/DD/YYYY date fields to proper dates
 - `fix_date_ymd()` —  for converting YYYYMMDD date fields to proper dates
 - `not.na()` — returns a logical vector indicating which elements are not `NA`; mimics `notna()` from {pandas}
 - `not_like()` — used to filter out rows where a specified column's value does not match a given pattern; mimics `NOT LIKE` from SQL
 - `%not%` — returns a logical vector indicating whether each element of one vector is not found in another vector

## Installation

You can install the development version of `acutis` with `devtools`:

```r
library(devtools)

devtools::install_github("CBurruss/acutis")
```

## Examples

### 1. `affiche()`

```r
library(acutis)
library(dplyr)

mtcars |> 
  head(5) |> 
  affiche()
```

```
╔═══════════════════╦══════╦═════╦══════╦═════╦══════╦═══════╦═══════╦════╦════╦══════╦══════╗ 
║ X.                ║ mpg  ║ cyl ║ disp ║ hp  ║ drat ║ wt    ║ qsec  ║ vs ║ am ║ gear ║ carb ║ 
╠═══════════════════╬══════╬═════╬══════╬═════╬══════╬═══════╬═══════╬════╬════╬══════╬══════╣ 
║ Mazda RX4         ║ 21   ║ 6   ║ 160  ║ 110 ║ 3.9  ║ 2.62  ║ 16.46 ║ 0  ║ 1  ║ 4    ║ 4    ║
║ Mazda RX4 Wag     ║ 21   ║ 6   ║ 160  ║ 110 ║ 3.9  ║ 2.875 ║ 17.02 ║ 0  ║ 1  ║ 4    ║ 4    ║
║ Datsun 710        ║ 22.8 ║ 4   ║ 108  ║ 93  ║ 3.85 ║ 2.32  ║ 18.61 ║ 1  ║ 1  ║ 4    ║ 1    ║
║ Hornet 4 Drive    ║ 21.4 ║ 6   ║ 258  ║ 110 ║ 3.08 ║ 3.215 ║ 19.44 ║ 1  ║ 0  ║ 3    ║ 1    ║
║ Hornet Sportabout ║ 18.7 ║ 8   ║ 360  ║ 175 ║ 3.15 ║ 3.44  ║ 17.02 ║ 0  ║ 0  ║ 3    ║ 2    ║ 
╚═══════════════════╩══════╩═════╩══════╩═════╩══════╩═══════╩═══════╩════╩════╩══════╩══════╝
```

### 2. `count_na()` 

```r
mtcars <- mtcars |> 
  mutate(vs = na_if(vs, 0), 
         am = na_if(am, 1))

mtcars |> 
  count_na() |> 
  affiche()
```

```
╔════╦══════╦══════════╦════════════╗ 
║ X. ║ col  ║ na_count ║ na_percent ║ 
╠════╬══════╬══════════╬════════════╣ 
║ 1  ║ vs   ║ 18       ║ 56%        ║
║ 2  ║ am   ║ 13       ║ 41%        ║
║ 3  ║ mpg  ║ 0        ║ 0%         ║
║ 4  ║ cyl  ║ 0        ║ 0%         ║
║ 5  ║ disp ║ 0        ║ 0%         ║
║ 6  ║ hp   ║ 0        ║ 0%         ║
║ 7  ║ drat ║ 0        ║ 0%         ║
║ 8  ║ wt   ║ 0        ║ 0%         ║
║ 9  ║ qsec ║ 0        ║ 0%         ║
║ 10 ║ gear ║ 0        ║ 0%         ║
║ 11 ║ carb ║ 0        ║ 0%         ║ 
╚════╩══════╩══════════╩════════════╝
```

### 3. `count_table()`

```r
mtcars |> 
  count_table(cyl) |> 
  affiche()
```

```
╔════╦═════╦═══════╦═════════╗ 
║ X. ║ cyl ║ count ║ percent ║ 
╠════╬═════╬═══════╬═════════╣ 
║ 1  ║ 8   ║ 14    ║ 44%     ║
║ 2  ║ 4   ║ 11    ║ 34%     ║
║ 3  ║ 6   ║ 7     ║ 22%     ║ 
╚════╩═════╩═══════╩═════════╝ 
```

### 4. `describe()`

```r
mtcars |> 
  describe() |> 
  affiche()
```

```
╔════════╦═══════════╦════════╦════════════╦══════════╦═══════════╦═════════╦══════════╦════╦════╦════════╦════════╗ 
║ X.     ║ mpg       ║ cyl    ║ disp       ║ hp       ║ drat      ║ wt      ║ qsec     ║ vs ║ am ║ gear   ║ carb   ║ 
╠════════╬═══════════╬════════╬════════════╬══════════╬═══════════╬═════════╬══════════╬════╬════╬════════╬════════╣ 
║ min    ║ 10.4      ║ 4      ║ 71.1       ║ 52       ║ 2.76      ║ 1.513   ║ 14.5     ║ 1  ║ 0  ║ 3      ║ 1      ║
║ max    ║ 33.9      ║ 8      ║ 472        ║ 335      ║ 4.93      ║ 5.424   ║ 22.9     ║ 1  ║ 0  ║ 5      ║ 8      ║
║ median ║ 19.2      ║ 6      ║ 196.3      ║ 123      ║ 3.695     ║ 3.325   ║ 17.71    ║ 1  ║ 0  ║ 4      ║ 2      ║
║ mean   ║ 20.090625 ║ 6.1875 ║ 230.721875 ║ 146.6875 ║ 3.5965625 ║ 3.21725 ║ 17.84875 ║ 1  ║ 0  ║ 3.6875 ║ 2.8125 ║
║ sd     ║ 6.03      ║ 1.79   ║ 123.94     ║ 68.56    ║ 0.53      ║ 0.98    ║ 1.79     ║ 0  ║ 0  ║ 0.74   ║ 1.62   ║
║ n      ║ 32        ║ 32     ║ 32         ║ 32       ║ 32        ║ 32      ║ 32       ║ 14 ║ 19 ║ 32     ║ 32     ║ 
╚════════╩═══════════╩════════╩════════════╩══════════╩═══════════╩═════════╩══════════╩════╩════╩════════╩════════╝
```

### 5. `pasteurize()`

```r
df <- data.frame(
  `First Name` = c(" Henry ", "BOB", "Joanne", "STEVEN "),
  `Birth-Day` = c("20031229", "19800515", "19950112", NA_character_),
   Age = c(22, NA_real_, 30, ""),
   Anniversary.date = c("02/13/2015", "10/20/1997", "Unknown", "05/01/2021") 
)

df = df |> pasteurize()

df |> affiche()
```

```
╔════╦════════════╦═══════════╦═════╦══════════════════╗ 
║ X. ║ first_name ║ birth_day ║ age ║ anniversary_date ║ 
╠════╬════════════╬═══════════╬═════╬══════════════════╣ 
║ 1  ║ Henry      ║ 20031229  ║ 22  ║ 02/13/2015       ║
║ 2  ║ Bob        ║ 19800515  ║ NA  ║ 10/20/1997       ║
║ 3  ║ Joanne     ║ 19950112  ║ 30  ║ Unknown          ║
║ 4  ║ Steven     ║ NA        ║ NA  ║ 05/01/2021       ║ 
╚════╩════════════╩═══════════╩═════╩══════════════════╝ 
```

### 6. `fix_date_mdy()` 

```r
df |> 
  mutate(anniversary_date_clean = fix_date_mdy(anniversary_date)) |>
  select(anniversary_date, anniversary_date_clean) |> 
  affiche()
```

```
╔════╦══════════════════╦════════════════════════╗ 
║ X. ║ anniversary_date ║ anniversary_date_clean ║ 
╠════╬══════════════════╬════════════════════════╣ 
║ 1  ║ 02/13/2015       ║ 2015-02-13             ║
║ 2  ║ 10/20/1997       ║ 1997-10-20             ║
║ 3  ║ Unknown          ║ Unknown                ║
║ 4  ║ 05/01/2021       ║ 2021-05-01             ║ 
╚════╩══════════════════╩════════════════════════╝ 
```

### 7. `fix_date_ymd()`

```r
df |> 
  mutate(birth_day_clean = fix_date_ymd(birth_day)) |>
  select(birth_day, birth_day_clean) |> 
  affiche()
```

```
╔════╦═══════════╦═════════════════╗ 
║ X. ║ birth_day ║ birth_day_clean ║ 
╠════╬═══════════╬═════════════════╣ 
║ 1  ║ 20031229  ║ 2003-12-29      ║
║ 2  ║ 19800515  ║ 1980-05-15      ║
║ 3  ║ 19950112  ║ 1995-01-12      ║
║ 4  ║ NA        ║ NA              ║ 
╚════╩═══════════╩═════════════════╝ 
```

### 8. `not.na()`

```r
df |> 
  filter(not.na(age)) |> 
  affiche()
```

```
╔════╦════════════╦═══════════╦═════╦══════════════════╗ 
║ X. ║ first_name ║ birth_day ║ age ║ anniversary_date ║ 
╠════╬════════════╬═══════════╬═════╬══════════════════╣ 
║ 1  ║ Henry      ║ 20031229  ║ 22  ║ 02/13/2015       ║
║ 2  ║ Joanne     ║ 19950112  ║ 30  ║ Unknown          ║ 
╚════╩════════════╩═══════════╩═════╩══════════════════╝ 
```

### 9. `not_like()`

```r
df |> 
  filter(not_like("en", first_name)) |> 
  affiche()
```

```
╔════╦════════════╦═══════════╦═════╦══════════════════╗ 
║ X. ║ first_name ║ birth_day ║ age ║ anniversary_date ║ 
╠════╬════════════╬═══════════╬═════╬══════════════════╣ 
║ 1  ║ Bob        ║ 19800515  ║ NA  ║ 10/20/1997       ║
║ 2  ║ Joanne     ║ 19950112  ║ 30  ║ Unknown          ║ 
╚════╩════════════╩═══════════╩═════╩══════════════════╝ 
```

### 10. `%not%`

```r
df |> 
  filter(age %not% c(10, 20, 30)) |> 
  affiche()
```

```
╔════╦════════════╦═══════════╦═════╦══════════════════╗ 
║ X. ║ first_name ║ birth_day ║ age ║ anniversary_date ║ 
╠════╬════════════╬═══════════╬═════╬══════════════════╣ 
║ 1  ║ Henry      ║ 20031229  ║ 22  ║ 02/13/2015       ║
║ 2  ║ Bob        ║ 19800515  ║ NA  ║ 10/20/1997       ║
║ 3  ║ Steven     ║ NA        ║ NA  ║ 05/01/2021       ║ 
╚════╩════════════╩═══════════╩═════╩══════════════════╝ 
```
