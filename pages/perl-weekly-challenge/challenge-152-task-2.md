---
title: "Challenge 152 Task #2 - Rectangle Area"
---

## [Task #2] - Rectangle Area

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-152/#TASK2)

You are given coordinates bottom-left and top-right corner of two rectangles in a 2D plane.

Write a script to find the total area covered by the two rectangles.

**Example 1:**

```
Input: Rectangle 1 => (-1,0), (2,2)
Rectangle 2 => (0,-1), (4,4)

Output: 22
```

**Example 2:**

```
Input: Rectangle 1 => (-3,-1), (1,3)
Rectangle 2 => (-1,-3), (2,2)

Output: 25
```

### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-152/alexander-pankoff/perl/ch-2.pl)

For the second task of this weeks challenge I will follow these steps

1. Calculate the area of the two given triangles
2. Find the corners of the overlapping rectangle
3. Calculate the overlap area.
4. Calculate the total area by adding the areas of the two given rectangles and
   subtract the area of the overlap.


This process is laid out in the following Perl routine:

```perl
sub rectangles_area ( $rectangle1, $rectangle2 ) {
    my $area1 = rectangle_area($rectangle1);
    my $area2 = rectangle_area($rectangle2);

    my $overlap_rectangle =
      find_overlapping_rectangle( $rectangle1, $rectangle2 );

    my $overlap_area = rectangle_area($overlap_rectangle);

    return $area1 + $area2 - $overlap_area;
}
```

To calculate the area of an rectangle were we know the lower left and upper
right corner, we first need to calculate the side lengths of the rectangle.
We find these by calculating the absolute difference of the `x` and `y`
coordinates of the given corners. With the side lengths in place we multiply
them to get the area.

```perl
sub rectangle_area($rectangle) {
    my ( $x1, $y1, $x2, $y2 ) = map { @$_ } @$rectangle;

    my $x = abs( $x1 - $x2 );
    my $y = abs( $y1 - $y2 );

    return $x * $y;

}
```

Now we need to find the corners of the overlapping area of the two rectangles.
For the lower left corner coordinates we take the maximum of the `x` and `y`
components of the lower left corners of the given rectangles. For the upper
right corner we do the same, using the minimum this time. If one of the
coordinates of the supposed lower left corner is bigger then the corresponding
coordinate from the upper right corner, there is no overlap at all and we return
an empty rectangle in the origin of our plane.


```perl
sub find_overlapping_rectangle ( $rectangle1, $rectangle2 ) {
    my ( $x11, $y11, $x12, $y12 ) = map { @$_ } @$rectangle1;
    my ( $x21, $y21, $x22, $y22 ) = map { @$_ } @$rectangle2;

    my $x1 = max( $x11, $x21 );
    my $y1 = max( $y11, $y21 );

    my $x2 = min( $x12, $x22 );
    my $y2 = min( $y12, $y22 );

    if ( $x1 > $x2 || $y1 > $y2 ) {
        ## no overlap, return rectangle with empty area
        return [ [ 0, 0 ], [ 0, 0 ] ];
    }

    return [ [ $x1, $y1 ], [ $x2, $y2 ] ];
}
```
