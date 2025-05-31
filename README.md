
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyvader

<!-- badges: start -->

<!-- badges: end -->

A fast, clear, and tidy implementation of the rule-based sentiment
analysis algorithm VADER (Valence Aware Dictionary and Sentiment
Reasoner).

  - **It’s fast** because the code has been re-written from scratch and
    uses C++ for the core algorithm: in benchmarking it’s over 500 times
    faster than the competing R package.
  - **It’s clear** because it tries to make the rule-based algorithm and
    the dictionaries simple to read so that users can inspect and judge
    them.
  - **It’s tidy** because it tries to follow tidy design principles and
    works well with the %\>% pipe.

## Under Development

*Please note* that this package (and this documentation) is under active
development. At present it’s pretty well tested and functional, but
there are known limitations and there may yet be bugs. This is a
development package not yet on CRAN and things may change. Expect
more/better documentation and development as soon as time allows.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chris31415926535/tidyvader")
```

## What is VADER?

VADER’s authors describe it on their GitHub page as “a lexicon and
rule-based sentiment analysis tool that is specifically attuned to
sentiments expressed in social media” that was originally written in
Python ([link](https://github.com/cjhutto/vaderSentiment)).

Let’s break this definition down:

  - **A sentiment analysis tool** takes plain English text and decides
    whether it expresses positive or negative feelings.
  - **A lexicon** in this context is a dictionary that assigns words
    positive or negative scores.
  - **A set of rules** modifies these scores based on other words in the
    sentence. For example, one rule says that if a positive-score word
    has a negation in front of it, like “*not* happy,” that word should
    flip to have a negative score.
  - And text in **social media** is different from, for example, text in
    the New York Times, so VADER’s lexicon and rules are tailored for
    online communications.

This is notably different from two other common approaches to sentiment
analysis:

  - A **“bag-of-words” approach** like AFINN has a lexicon but no rules,
    so it can’t tell the difference between “I’m happy” and “I’m not
    happy” (much less “I’m not not unhappy”).
  - A **neural-net or other unsupervised approach** is essentially a
    “black box,” and it’s hard (or impossible) to understand and
    evaluate the way it generates outputs. It may have something
    approaching rules and a lexicon, but you can’t review them to judge
    how credible they are.

VADER has advantages over both of these approaches. First, it’s more
nuanced than a pure bag-of-words approach and so it should be more
accurate. Second, it’s more surveyable than an unsupervised approach and
so users can make informed decisions about when and how it’s appropriate
to use it.

## Why **tidyvader**?

## Example

This example shows how to send sentences in a dataframe through
`vader()`. It also shows how punctuation, capitalization, modifiers, and
negations all work together to affect a sentence’s compound score.

``` r
library(tidyvader)
library(tibble)
library(magrittr)
library(knitr)

# set up a tibble with some sentences
texts <- tibble(sentences = c("I feel happy today.",
                              "I feel happy today!",
                              "I feel HAPPY today!",
                              "I feel NOT HAPPY today!",
                              "I feel REALLY NOT HAPPY today!"))

# pipe the data to tidyvader::vader() and specify the column with text 
texts %>%
  tidyvader::vader(sentences) %>%
  knitr::kable()
```

| sentences                       | compound |    pos |    neu |    neg |
| :------------------------------ | -------: | -----: | -----: | -----: |
| I feel happy today.             |   0.5719 | 0.5522 | 0.4478 | 0.0000 |
| I feel happy today\!            |   0.6114 | 0.5709 | 0.4291 | 0.0000 |
| I feel HAPPY today\!            |   0.6932 | 0.6117 | 0.3883 | 0.0000 |
| I feel NOT HAPPY today\!        | \-0.5903 | 0.0000 | 0.5107 | 0.4893 |
| I feel REALLY NOT HAPPY today\! | \-0.6761 | 0.0000 | 0.5234 | 0.4766 |

If you want to score a single sentence in a length-1 character vector
you can use `vader_chr()`. This is good for quickly checking things, but
it’s much slower than `vader()` so I don’t recommend it for analysis at
scale. The results will come in a one-row tibble, like so:

``` r
tidyvader::vader_chr("I feel HAPPY today!") %>%
  knitr::kable()
```

| compound |    pos |    neu | neg |
| -------: | -----: | -----: | --: |
|   0.6932 | 0.6117 | 0.3883 |   0 |

You can also easily pull the VADER dictionaries and some test sentences
in a nested tibble using `get_vader_dictionaries()`. It’s easy to take a
look through RStudio’s viewer, and you can also pull them out and
inspect them as regular tibbles.

``` r
library(dplyr)

vader_dicts <- tidyvader::get_vader_dictionaries()

vader_sentiments <- vader_dicts %>%
  filter(name == "dict_sent_sorted") %>%
  pull(dictionary) %>% `[[` (1)

vader_sentiments[2968:2973,] %>%
  knitr::kable()
```

| word        | sentiment |
| :---------- | --------: |
| friendship  |       1.9 |
| friendships |       1.6 |
| fright      |     \-1.6 |
| frighted    |     \-1.4 |
| frighten    |     \-1.4 |
| frightened  |     \-1.9 |

## Known Limitations

  - Unicode emojis aren’t supported yet 😢
  - Several special cases in base VADER aren’t implemented yet:
    e.g. some multi-word phrases like “never so” and “without a doubt.”
  - Expressions and turns of phrase in base VADER like “bad ass” not yet
    supported.
  - Pos/Neu/Neg scores don’t match the Python implementation when ascii
    emojis and caps differences are present, although the compound score
    matches.s
  - There’s no support for custom dictionaries.
  - VADER’s lexicon is 6 years old so doesn’t reflect some currant usage
    (DEAD\!).
  - VADER’s lexicon includes some terms that will limit its
    applicability in some contexts and communities. Not all words are
    used in the same way by all people in all settings.

## Resources and References

  - VADER’s Python GitHub page:
    <https://github.com/cjhutto/vaderSentiment>

  - Citation for conference proceedings introducing VADER:
    
      - Hutto, C.J. & Gilbert, E.E. (2014). VADER: A Parsimonious
        Rule-based Model for Sentiment Analysis of Social Media Text.
        Eighth International Conference on Weblogs and Social Media
        (ICWSM-14). Ann Arbor, MI, June 2014.
