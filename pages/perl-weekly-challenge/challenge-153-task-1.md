---
title: "Challenge 153 Task #1 - Factorials left, factorials right, factorials everywhere!"
---

## Task #1 - Left Factorials

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-153/#TASK1)

Submitted by: _Mohammad S Anwar_

Write a script to compute Left Factorials of 1 to 10. Please refer [OEIS
A003422](https://oeis.org/A003422) for more information.

Expected Output:

```
1, 2, 4, 10, 34, 154, 874, 5914, 46234, 409114
```

### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-153/alexander-pankoff/perl/ch-1.pl)

On the linked OEIS site we can find an example for calculating a left factorial:

```
!5 = 0! + 1! + 2! + 3! + 4! = 1 + 1 + 2 + 6 + 24 = 34.
```

From this we can see that to calculate a left factorial `!n` for a number `n` we
need to sum all factorials from `0` up to `n-1`.

To do that we will first have to define a way to calculate a factorial. The
factorial `n!` of a number `n` is the product of all numbers form `1` up to `n`

For the summing and multiplying of ranges I will use `sum0` and `product` from
`List::Util`.

For a factorial we generate the range `1 .. $n` and pass it to the imported
`product` routine. For the left factorials we first generate a list of the
factorials that we need and sum the up.

```perl
sub fac($n) {
    product( 1 .. $n );
}

sub left_factorial($n) {
    return sum0( map { fac($_) } 0 .. ( $n - 1 ) );
}
```

Finally, some wrapper code to generate and print the requested range of
left_factorials

```perl
sub run() {
    say join( ', ', left_factorials( 1, 10 ) );
}

sub left_factorials ( $from, $to ) {
    return map { left_factorial($_) } $from .. $to;
}
```

### Haskell Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-153/alexander-pankoff/haskell/ch-1.hs)

In `Haskell` we can make use of lazy lists to define lists of all factorials and
all left factorials.

To do this we will define a starting value in both cases and then use `zipWith`
to combine the list itself with another list.

For the factorials the starting value will be `1` (as `0! = 1`). And then we zip
the `facs` list with a list off all integers > 1.

So the first element of this list will be `1` as we have defined. The second
will be `1 * 1 = 1` - the first element from this list (`1)` multiplied with the
first integer (`1`). The third element will be `1 * 2 = 2` - the second element
from this list (`1)` multiplied with the second integer (`2`). This process will
be repeated up to the maximum index that will be requested from this list.

```haskell
facs :: [Integer]
facs = 1 : zipWith (*) facs [1 ..]
```

For the left factorials we do something similar. The starting value is a `0`
this time, the operator will be `+` instead of `*`, and the list we will be
zipping with will be the list of factorials we just defined.

```haskell
leftFactorials :: [Integer]
leftFactorials = 0 : zipWith (+) leftFactorials facs
```

And again some driver code to print the `!1 .. !10`.

```haskell
main :: IO ()
main = drop 1 leftFactorials & take 10 & print
```

### Raku Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-153/alexander-pankoff/raku/ch-1.raku)

After solving it in Haskell I got curious if I could do the same in raku. There
are lazy lists/sequences in `raku`, so the only obstacle should be my lack of
skill in that language. I've never really tried `raku` out much and couldn't
figure out how to do it from reading the docs. A
[blog post by Andrew Shitov](https://andrewshitov.com/2021/01/31/computing-factorials-using-raku/)
about how to define the factorials helped me out. I still don't get all of it,
and I'm not able to explain, but I was able to adapt the solution to make it
work for left factorials

```raku
# I don't know how to write raku/perl6. With help from this blog post by Andrew
# Shitov I could figure out, how to generate a lazy, inifinite sequence of all
# factorials and adapt to the left factorial problem
# https://andrewshitov.com/2021/01/31/computing-factorials-using-raku/

my @facs = 1, * * ++$ ... *;
my @left_factorials = 0, 1, * + @facs[ ++$ ] ... *;

say @left_factorials[1..10];
```
