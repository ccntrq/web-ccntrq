---
title: "Challenge 152 Task #2 - Factorions and more factorials"
---

## Task #2 - Factorions

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-152/#TASK2)

Submitted by: *Mohammad S Anwar*

You are given an integer, `$n`.

Write a script to figure out if the given integer is factorion.

> A factorion is a natural number that equals the sum of the factorials of its digits.

Example 1:

```
Input: $n = 145
Output: 1

    Since 1! + 4! + 5! => 1 + 24 + 120 = 145
```

Example 2:

```
Input: $n = 123
Output: 0

    Since 1! + 2! + 3! => 1 + 2 + 6 <> 123
```


### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-152/alexander-pankoff/perl/ch-2.pl)

Similar to the first task of this week, this is about factorials. I first solved
it just with what is given in the description. Split the number into its digits,
calculate the factorial for each digit and sum that up. If the sum is equal to
the input we have a factorion.

First the same `fac` routine as in task 1.

```perl
sub fac($n) {
    product( 1 .. $n );
}
```

Here the code for the `is_factorion` routine.

```perl
sub is_factorion($n) {
    my @digits                      = split( m//, $n );
    my $sum_of_factorials_of_digits = sum0( map { fac($_) } @digits );

    return $n == $sum_of_factorials_of_digits;
}
```

Afterwards I wrote some drive code to run this for numbers up to 100000:

```perl
sub run() {
    my $max = 100000;
    say "Factorions <= $max:";
    for ( 1 .. $max ) {
        say $_
          if is_factorion($_);
    }
}
```

Output:

```
1
2
145
40585
```

With this little of output I started doing some online research for factorions
and found [Sequence A014080](https://oeis.org/A014080). Surprisingly the list
above is all factorions that exist.

I then also wrote a routine that uses this list to check if `$n` is a factorion.


```perl
sub is_factorion_a014080($n) {
    ## complete list of all factorians - see https://oeis.org/A014080
    state @A014080 = ( 1, 2, 145, 40585 );
    first { $n == $_ } @A014080;
}
```

And adapted my driver code a little to chose randomly between the two solutions
for each number that it checks.

```perl
sub run() {
    my $max = 100000;
    say "Factorions <= $max:";
    for ( 1 .. $max ) {
        my $fn = ( \&is_factorion, \&is_factorion_a014080 )[ int( rand(2) ) ];
        say $_ if $fn->($_);
    }
}
```

### Haskell Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-153/alexander-pankoff/haskell/ch-2.hs)

I also solved this task in `Haskell` by pattern matching on `n` and comparing it to the four possible results:

```haskell
isFactorion :: Integer -> Bool
isFactorion 1     = True
isFactorion 2     = True
isFactorion 145   = True
isFactorion 40585 = True
isFactorion _     = False
```

### Raku Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-153/alexander-pankoff/raku/ch-2.rakue)

In `raku` I used a `unit main` to make the whole script the main routine. This
main routine will also handle command line arguments for us. I use the utf-8
elem operator `∈` to check if the given number is a member of the A014080
sequence. I struggled a little with what looks like some typing errors. I had to
multiply the argument `$n` with `1` to coerce it into a number, otherwise the
code wouldn't work.

```raku
unit sub MAIN(Int $n);
my @A014080 = 1, 2, 145, 40585;

# Seems we have to coerce the argument $n into a number first by multiplying by
# 1. Didn't expect that.
say $n*1 ∈ @A014080;
```