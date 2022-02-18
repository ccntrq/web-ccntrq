---
title: "Challenge 152 Task #1 - Summing up minimums"
---

## [Task #1] - Triangle Sum Path

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-152/#TASK1)

You are given a triangle array.

Write a script to find the minimum sum path from top to bottom.

**Example 1:**

```
Input: $triangle = [ [1], [5,3], [2,3,4], [7,1,0,2], [6,4,5,2,8] ]

                1
               5 3
              2 3 4
             7 1 0 2
            6 4 5 2 8

Output: 8

    Minimum Sum Path = 1 + 3 + 2 + 0 + 2 => 8
```

**Example 2:**

```
Input: $triangle = [ [5], [2,3], [4,1,5], [0,1,2,3], [7,2,4,1,9] ]

                5
               2 3
              4 1 5
             0 1 2 3
            7 2 4 1 9

Output: 9

    Minimum Sum Path = 5 + 2 + 1 + 0 + 1 => 9
```

### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-152/alexander-pankoff/perl/ch-1.pl)

On first reading of the current challenge, I thought we had this taks before.
After some digging it turns out - not quite.
In
[task #2 of challenge 100](https://theweeklychallenge.org/blog/perl-weekly-challenge-100/#TASK2)
we had the same task with an additional restriction:

> When you are on index i on the current row then you may move to either index i
> or index i + 1 on the next row.

I solved this here
[here](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-100/alexander-pankoff/perl/ch-2.pl).

Without this restriction it becomes even easier. We have to calculate the sum of
the minimum values from each row of the triangle. To do this I'll use `sum0` and
`min` from the venerable `List::Util` and Perl's built-in `map`.

```perl
sub minimum_triangle_sum_path($triangle) {
    return sum0( map { min(@$_) } @$triangle );
}
```

In words:

We map the `min` function over the rows of the triangle and calculate the sum of that.

Done.
