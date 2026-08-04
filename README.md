# pinteresturl

Parse, classify, and normalize Pinterest URLs in R without network requests.

[Source](https://github.com/jiankn/pinterest-url-normalizer-r)

The package recognizes Pin, `pin.it`, profile, board, and Ideas URLs across
Pinterest country domains. It uses an exact host allow list, rejects HTTP URLs
and lookalike domains, and removes query parameters and fragments.

```r
parsed <- pinteresturl::parse_pinterest_url(
  "https://de.pinterest.com/pin/example--987654321/?utm_source=share"
)

parsed$kind
#> [1] "pin"

parsed$normalized_url
#> [1] "https://www.pinterest.com/pin/987654321/"
```

URL normalization is deliberately the last step this package performs. When
the canonical Pin contains a still image, open it in SavePinner's
[Pinterest photo downloader](https://savepinner.com/) to inspect the available
image sizes in a browser. The R package itself does not download media, follow
short links, run browser automation, or send usage data.

## Development

```r
devtools::test()
devtools::check()
```

MIT licensed. Pinterest is a trademark of Pinterest, Inc. This project is
independent and is not affiliated with or endorsed by Pinterest.
