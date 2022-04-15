---
title: "Challenge 160 Task #2 - Steps to recovering the Equilibrium in your lists"
---

## Task #2 - Equilibrium Index

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-160/#TASK2)

Submitted by: _Mohammad S Anwar_

You are give an array of integers, @n.

Write a script to find out the `Equilibrium Index` of the given array, if found.

> For an array A consisting n elements, index i is an equilibrium index if the sum
> of elements of subarray A[0…i-1] is equal to the sum of elements of subarray
> A[i+1…n-1].

Example 1:

```
Input: @n = (1, 3, 5, 7, 9)
Output: 3
```

Example 2:

```
Input: @n = (1, 2, 3, 4, 5)
Output: -1 as no Equilibrium Index found.
```

Example 3:

```
Input: @n = (2, 4, 2)
Output: 1
```

### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-160/alexander-pankoff/perl/ch-2.pl)

Phrased a little differently, an equilibrium index is an index, where the sum of
all elements left to it is equal to the sum of all elements to the right of it.

With negative numbers and zero being allowed in a list, there might be more than
one _Equilibrium Index_. `1 0 0 1` for example with index `1` and `2` being
equilibrium indices, or `0 0 0 0` with all indices being possible candidates. I
wrote a solution that will find the first equilibrium index, if there is one.

```perl
sub equilibrium_index(@xs) {
    for my $i ( 0 .. $#xs  ) {
        my $lower = sum0( @xs[ 0 .. $i - 1 ] );
        my $upper = sum0( @xs[ $i + 1 .. $#xs ] );
        return $i if $lower == $upper;
    }

    return -1;
}
```

The solution walks all indices in order and returns the first where the sum of
the left and right subarrays is equal. I'm using `sum0` from `List::Util` to
prevent warnings about uninitialized values that would pop up at the beginning
and at the end of the list if I were to use `sum`.
