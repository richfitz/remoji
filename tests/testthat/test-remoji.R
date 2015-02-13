context("Emoji")

test_that("Corner cases", {
  expect_that(emoji("no_such_alias"), is_identical_to(NA_character_))
  expect_that(emoji(character(0)), equals(character(0)))
})

test_that("Basic use", {
  plus1 <- emoji("+1")
  expect_that(plus1, is_a("character"))
  expect_that(plus1, equals("\U0001f44d"))

  expect_that(emoji(c("+1", "+1")), equals(c(plus1, plus1)))

  expect_that(emoji("+1", TRUE), equals(paste0(plus1, " ")))
  expect_that(emoji("no_such_alias", TRUE), is_identical_to(NA_character_))
})

test_that("Invalid names", {
  expect_that(emoji("frown"), equals(NA_character_))
  expect_that(emoji("frown", must_work=TRUE),
              throws_error("frowning"))
  expect_that(emoji(c("frown", "bazzar"), must_work=TRUE),
              throws_error("frowning"))
  expect_that(emoji(c("frown", "bazzar"), must_work=TRUE),
              throws_error("no suggestion"))
})

test_that("Search", {
  expected <- c("person_with_blond_hair", "raised_hands", "pray",
                "information_desk_person", "raising_hand",
                "person_with_pouting_face", "person_frowning", "bow",
                "computer")
  expect_that(all(expected %in% find_emoji("person", "description")),
              is_true())

  expect_that(all(expected[grepl("person", expected)] %in%
                  find_emoji("person", "alias")),
              is_true())

  expect_that(find_emoji("person", "tag"), equals(character(0)))
})

test_that("regular expression is correct", {
  expect_that(all(grepl(emoji_re(), paste0(":", dat_alias$alias, ":"))),
            is_true())
})

test_that("Substitution", {
  str1 <- "hello :cat:!"
  expect_that(contains_emoji(str1), is_true())
  expect_that(sub_emoji(str1),
              equals(paste0("hello ", emoji("cat", TRUE),  "!")))
  expect_that(sub_emoji(str1, FALSE),
              equals(paste0("hello ", emoji("cat", FALSE), "!")))

  str2 <- "hello :cat::dog:!"
  expect_that(sub_emoji(str2),
              equals(paste0("hello ",
                            emoji("cat", TRUE),
                            emoji("dog", TRUE),
                            "!")))

  expect_that(sub_emoji(str2, FALSE),
              equals(paste0("hello ",
                            emoji("cat", FALSE),
                            emoji("dog", FALSE),
                            "!")))

  expect_that(sub_emoji(c(str1, str2)),
              equals(c(sub_emoji(str1),
                       sub_emoji(str2))))

  str1_e <- sub_emoji(str1)
  str12_e <- sub_emoji(c(str1, str2))
  expect_that(unsub_emoji(str1_e), equals(str1))
  expect_that(unsub_emoji(str12_e), equals(c(str1, str2)))


  str1_e <- sub_emoji(str1, FALSE)
  str12_e <- sub_emoji(c(str1, str2), FALSE)
  expect_that(unsub_emoji(str1_e), equals(str1))
  expect_that(unsub_emoji(str12_e), equals(c(str1, str2)))

  expect_that(unsub_emoji(str1_e, FALSE), equals(str1))
  expect_that(unsub_emoji(str12_e, FALSE), equals(c(str1, str2)))

  expect_that(sub_emoji("invalid emoji :frown:"),
              throws_error("Invalid emoji!"))
})

test_that("emoji_table", {
  expect_that(emoji_table(character(0)),
              throws_error("No aliases given"))
  expect_that(emoji_table(find_emoji("frown", approximate=TRUE)),
              shows_message("bullettrain_front"))
})

test_that("No missing emoji", {
  expect_that(!any(is.na(dat_core$emoji)), is_true())
  expect_that(!any(is.na(emoji(list_emoji()))), is_true())
})
