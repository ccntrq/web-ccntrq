---
title: Challenge 190 - "Regex Weekly Challenge" and "Chunkify and decode"
body-title: Challenge 190 - <em>Regex Weekly Challenge</em> and <em>Chunkify and decode</em>
---

[Deutsche Version](/pages/perl-weekly-challenge/challenge-190-de.html)

## Task #1 - _Regex Weekly Challenge_ (Capital Detection)

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-190/#TASK1)

**Submitted by** [Mohammad S Anwar](http://www.manwar.org)

You are given a string with alphabetic characters only: `A..Z` and `a..z`.

Write a script to find out if the usage of Capital is appropriate if it satisfies at least one of the following rules:

1. Only first letter is capital and all others are small.
2. Every letter is small.
3. Every letter is capital.

##### Example 1

```
Input: $s = 'Perl'
Output: 1
```

##### Example 2

```
Input: $s = 'TPF'
Output: 1
```

##### Example 3

```
Input: $s = 'PyThon'
Output: 0
```

##### Example 4

```
Input: $s = 'raku'
Output: 1
```

### Solution

Welcome to the _Regex Weekly Challenge_ where we translate requirements into a regex:

```perl
sub capital_detection ($s) {
    return $s =~ s/[[:^alpha:]]//gr =~ m/^(?:
      [[:upper:]][[:lower:]]+ | # 1) Only first letter is capital and all
                                #    others are small.
      [[:lower:]]+ |            # 2) Every letter is small.
      [[:upper:]]+ |            # 3) Every letter is capital.
      )$/x
      ? 1    #
      : 0;
}
```

The first regex `s/[[:^alpha:]]//gr` removes all non-alpha charaters from the
input string as we don't care for those characters for this challenge. The
second regex encodes the three rules in an alternating, non-capturing group -
commented for clarity:

1. Only first letter is capital and all others are small.
2. Every letter is small.
3. Every letter is capital.

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-190/alexander-pankoff/perl/ch-1.pl)

<br/>

---

<br/>

## Task #2 - _Chunkify and decode_ (Decoded List)

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-190/#TASK1)

**Submitted by** [Mohammad S Anwar](http://www.manwar.org)

You are given an encoded string consisting of a sequence of numeric characters: 0..9, $s.

Write a script to find the all valid different decodings in sorted order.

    Encoding is simply done by mapping A,B,C,D,… to 1,2,3,4,… etc.

##### Example 1

```
Input: $s = 11
Output: AA, K

11 can be decoded as (1 1) or (11) i.e. AA or K
```

##### Example 2

```
Input: $s = 1115
Output: AAAE, AAO, AKE, KAE, KO

Possible decoded data are:
(1 1 1 5) => (AAAE)
(1 1 15)  => (AAO)
(1 11 5)  => (AKE)
(11 1 5)  => (KAE)
(11 15)   => (KO)
```

##### Example 3

```
Input: $s = 127
Output: ABG, LG

Possible decoded data are:
(1 2 7) => (ABG)
(12 7)  => (LG)
```

### Solution

I solved the second challenge in a two step process:

1. Determine all possible _chunkifications_ of the input string with substrings
   of lengths 1 or 2, where substrings with a length of 2 have the additional
   constraint that they must be less than or equal to 26.
2. Convert these _chunkifications_ into the possible string decodings.

The first step is done by the recursive routine `possible_decodings`:

```perl
sub possible_decodings ( $str, $cur = 0, $acc = [] ) {
    return ($acc) if $cur >= length $str;
    # 1)
    my @decodings =
      possible_decodings( $str, $cur + 1, [ @$acc, substr( $str, $cur, 1 ) ] );

    # 2)
    my $next_two = substr( $str, $cur, 2 );

    # 3)
    if ( length $next_two == 2 && $next_two <= 26 ) {
        push @decodings,
          possible_decodings( $str, $cur + 2, [ @$acc, $next_two ] );
    }

    return @decodings;
}
```

In 1) the letter at the current position in the string is used as a possible
part of a decoding. By the recursive call for the next position (`$cur + 1`) we
obtain all further decodings that begin with this letter. In 2) and 3) we fetch
the 2 letters at the current and next position in the string, check whether
there are still 2 letters and whether the two letters are numerically less than
or equal to 26. If yes, we generate all decodings beginning with this bigram
with the recursive call for the position after the next (`$cur + 2`). We add
these to the end of the decodings from 1). By carefully choosing the recursion
order, this routine will return the decodes in the exact order required by the
task.

The 2nd step now consists of converting the decodings determined in step 1 into
strings. To do this, we add the offset of the letter 'A' in the ASCII table to
the determined values and convert the result into a letter via `chr`.

```perl
sub decoded_list ($encoded) {
    my $offset = ord('A') - 1;
    return map {
        join( '', map { chr( $_ + $offset ) } @$_ )
    } possible_decodings($encoded);
}
```

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-190/alexander-pankoff/perl/ch-2.pl)
