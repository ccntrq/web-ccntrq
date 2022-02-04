---
title: "Challenge 150 Task #1 - Fibonacci words"
---

## [Task #1 - Fibonacci words]{#Task1}

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-150/#TASK1)

You are given two strings having same number of digits, $a and $b.

Write a script to generate Fibonacci Words by concatenation of the previous two
strings. Finally print 51st digit of the first term having at least 51 digits.

**Example:**

```
    Input: $a = '1234' $b = '5678'
    Output: 7

    Fibonacci Words:

    '1234'
    '5678'
    '12345678'
    '567812345678'
    '12345678567812345678'
    '56781234567812345678567812345678'
    '1234567856781234567856781234567812345678567812345678'

    The 51st digit in the first term having at least 51 digits
    '1234567856781234567856781234567812345678567812345678' is 7.
```

### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-150/alexander-pankoff/perl/ch-1.pl)

For this challenge we start by getting the two input words `$a` and `$b`
from the argument list.

```perl
my ( $a, $b ) = @ARGV;
```

After checking the length of the first string to make sure that we have any
input at all, we check if both strings are of the same length. We will not check
if only digits were used. It doesn't matter for the algorithm whether we
restrict the input to only digits or allow arbitrary characters. (Actually it
doesn't even matter that they are of the same length, but I will go with that
restriction)

```perl
die "Expect two input words of equal length!\n"
  unless length($a) && length($a) == ( length($b) // 0 );
```

Now we pass both words to the meat of this solution, the `fibonacci_word`
routine. We additionaly pass the minimum length of 51 charachters up to which we
will build the fibonnaci word

```perl
my $fibonacci_word = fibonacci_word( $a, $b, 51 );
```

Finally we extract the 51st charachter (at index 50) from the built word and
print it out as our result.

```perl
my $target = substr( $fibonacci_word, 50, 1 );
say $target;
```

The actual `fibonacci_word` routine cries for a recursive solution. As always
with a recursive approach we start by defining the exit condition, which is
fullfilled as soon as the `$a` string reaches at least the requested length
`$length`. In that case ` $a` is the final fibonacci word and we return it to
the caller.

Otherwise we continue the process, by passing `$b` as the new `$a` to the
fibonacci_word routine and accumulating the next fibonacci word (the
concatenation of `$a` and ` $b`) into `$b`. This swapping and concatenating of
the input words goes on until we reach the requested length.

```perl
sub fibonacci_word ( $a, $b, $length ) {
    return $a if length($a) >= $length;
    return fibonacci_word( $b, $a . $b, $length );
}
```
