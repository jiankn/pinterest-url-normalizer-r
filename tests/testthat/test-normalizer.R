test_that("Pin URLs are normalized", {
  parsed <- parse_pinterest_url(
    "https://de.pinterest.com/pin/example--987654321/?utm_source=share"
  )
  expect_equal(parsed$kind, "pin")
  expect_equal(parsed$identifier, "987654321")
  expect_equal(
    parsed$normalized_url,
    "https://www.pinterest.com/pin/987654321/"
  )
})

test_that("short, profile, board, and Ideas URLs are recognized", {
  expect_equal(normalize_pinterest_url("https://pin.it/AbC123"), "https://pin.it/AbC123/")
  expect_equal(parse_pinterest_url("https://www.pinterest.com/savepinner/")$kind, "profile")
  expect_equal(
    parse_pinterest_url("https://pinterest.co.uk/savepinner/media-tools/")$kind,
    "board"
  )
  expect_equal(
    parse_pinterest_url("https://www.pinterest.com/ideas/space-wallpaper/926295399832/")$kind,
    "ideas"
  )
})

test_that("unsafe and lookalike URLs are rejected", {
  expect_false(is_pinterest_url("http://www.pinterest.com/pin/123/"))
  expect_false(is_pinterest_url("https://pinterest.com.example.org/pin/123/"))
  expect_false(is_pinterest_url("https://user@pinterest.com/pin/123/"))
  expect_false(is_pinterest_url("https://pinterest.com:444/pin/123/"))
  expect_false(is_pinterest_url("https://pinterest.com/pin/%31%32%33/"))
})
