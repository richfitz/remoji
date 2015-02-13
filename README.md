# Emoji for R

[![Build Status](https://travis-ci.org/richfitz/remoji.png?branch=master)](https://travis-ci.org/richfitz/remoji)
[![Coverage Status](https://coveralls.io/repos/richfitz/remoji/badge.svg?branch=master)](https://coveralls.io/r/richfitz/remoji?branch=master)

I am sorry :crying_cat_face:.

```r
devtools::install_github("richfitz/remoji")
library(remoji)
message(emoji("cat"))
message(emoji(list_emoji(), TRUE))
message(sub_emoji("This is silly :frowning:"))
emoji_table(find_emoji("frown"))
emoji_table(find_emoji("frown", approximate=TRUE))
find_emoji("sun") %>% emoji_table()
```

I am so sorry :joy_cat:
