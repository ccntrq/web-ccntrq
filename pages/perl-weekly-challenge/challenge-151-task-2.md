---
title: "Challenge 151 Task #2 - Rob the house"
---

## [Task #2] - Rob the house

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-151/#TASK2)

You are planning to rob a row of houses, always starting with the first and moving in the same direction. However, you can’t rob two adjacent houses.

Write a script to find the highest possible gain that can be achieved.

**Example 1:**

```
Input: @valuables = (2, 4, 5);
Output: 7

If we rob house (index=0) we get 2 and then the only house we can rob is house
(index=2) where we have 5.
So the total valuables in this case is (2 + 5) = 7.
```


**Example 2:**

```
Input: @valuables = (4, 2, 3, 6, 5, 3);
Output: 13

The best choice would be to first rob house (index=0) then rob house (index=3)
then finally house (index=5).
This would give us 4 + 6 + 3 =13.
```


### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-151/alexander-pankoff/perl/ch-2.pl)

To solve this task I will first generate all possible robbing tours, and then
find the one where we make the biggest haul. To reduce the risk of being caught,
we use the shortest tour if there are multiple options with the same return.

The following routine generates all possible tours. It takes the maximum index
and the position we are currently at. For each position we have two choices:

1. Rob the house and skip the next one
2. Don't rob and go to the next house

Each of these choices is represented by a recursive call to the `plan_tours`
routine with the position increased by 1 or 2 correspondingly. In the case were
we rob, we add the current index to all tours that we receive from the recursive
call.


```perl
sub plan_tours ( $max, $cur = 0 ) {
    return [] if $cur > $max;
    my @paths = (
        ( map { [ $cur, @$_ ] } plan_tours( $max, $cur + 2 ) ),
        plan_tours( $max, $cur + 1 )
    );

    return @paths;
}
```

With all tours in place, we go on to find the best one. This is done by reducing
the list of tours. The reducing routine calculates the sum of the values at the
corresponding indexes in the input, compares this to the value of the current
best tour, and returns a new best tour when the value is greater, or when its
equal and the tour is shorter than the current best. The starting value for the
reduction is an empty tour with a zero value.


```perl
sub rob_house (@valuables) {
    my @tours = plan_tours($#valuables);

    my $best_tour = reduce sub {
        my @tour       = @$b;
        my $tour_value = sum0 map { $valuables[$_] } @tour;
        if (   $tour_value > $a->{value}
            || $tour_value == $a->{value} && @tour < @{ $a->{tour} } )
        {
            return {
                value => $tour_value,
                tour  => [@tour],
            };
        }

        return $a;

    }, { value => 0, tour => [] }, @tours;

    return $best_tour->{value};
}
```