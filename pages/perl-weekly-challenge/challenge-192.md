---
title: Challenge 192 - Flipped Equilibrium
---

## Task #1 - Binary Flip

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-192/#TASK1)

**Submitted by** [Mohammad S Anwar](http://www.manwar.org)

You are given a positive integer, `$n`.

Write a script to find the binary flip.

##### Example 1

```
Input: $n = 5
Output: 2

First find the binary equivalent of the given integer, 101.
Then flip the binary digits 0 -> 1 and 1 -> 0 and we get 010.
So Binary 010 => Decimal 2.
```

##### Example 2

```
Input: $n = 4
Output: 3

Decimal 4 = Binary 100
Flip 0 -> 1 and 1 -> 0, we get 011.
Binary 011 = Decimal 3
```

##### Example 3

```
Input: $n = 6
Output: 1

Decimal 6 = Binary 110
Flip 0 -> 1 and 1 -> 0, we get 001.
Binary 001 = Decimal 1
```

### Solution

With the description being a little vague, I had to check out the examples to
figure out how to perform the flipping task.  Apparently, it is not required to
flip all bits in the binary representation, but only up to the most significant
set bit.

This can be done by `xor`ing the given number `$n` with another number, where all
bits up to the most significant bit in `$n` are set.

To find this number, I employed two different strategies.

#### 1.) In Perl and Raku

```Raku
-1 + 2 ** ( $n + 1 ).log2.ceiling;
```

This piece of Raku code finds the number of bits needed to represent `$n`. It
takes the logarithm of `$n + 1` to base two and rounds that up to the next integer.
The number where all bits are set is then one less than 2 to the power of the
number just found.

```
5 (101) ==> 2 (010)

log2(5).ceiling() -> 3
2**3 -1           -> 7 (111)
```


The only thing missing now is to `xor` `$n` and the found number together to
flip the bits.

```
5   101
xor
7   111
->2 010
```

```Raku
sub binary-flip(Int $n --> Int) {
    return $n +^ ( -1 + 2** ( $n + 1 ).log2.ceiling );
}
```

The Perl solutions follows the same scheme with minor differences.  Most notable
is the lack of a `log2` routine, which is why I emulated that by first
calculating the natural logarithm of `$n` and then dividing that by the natural
logarithm of 2.  Other differences are in the bitwise xor operator (`+^ <=> ^`),
the use of procedure calls instead of method calls for `log` and `ceil` and the
missing type signature. 

```Perl
sub binary_flip ($n) {
    $n ^ ( -1 + 2**ceil( log( $n + 1 ) / log(2) ) );
}
```

#### 2.) In Haskell

In Haskell there is a more straightforward way to find the number to `xor` `$n` with.

Equipped with
[`countLeadingZeros`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:countLeadingZeros),
[`oneBits`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:oneBits)
and
[`shiftR`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:shiftR)
from `Data.Bits`, we find the number by shifting `oneBits` right by the number of
leading zeros found in `$n`.

Afterwards the `xor`ing to flip the bits is the same as in the perl and Raku solution.

```Haskell
binaryFlip :: Word64 -> Word64
binaryFlip n = n `xor` shiftR oneBits (countLeadingZeros n)
```

#### Full Sources

- [Perl](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-192/alexander-pankoff/perl/ch-1.pl)
- [Raku](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-192/alexander-pankoff/raku/ch-1.raku)
- [Haskell](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-192/alexander-pankoff/haskell/ch-1.hs)

<br/>

---

<br/>

## Task #2 - Equal Distribution


[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-192/#TASK2)

**Submitted by** [Mohammad S Anwar](http://www.manwar.org)

You are given a list of integers greater than or equal to zero, `@list`.

Write a script to distribute the number so that each members are same. If you succeed then print the total moves otherwise print `-1`.

Please follow the rules (as suggested by **Niels van Dijke** [2022-11-21 13:00]

1) You can only move a value of '1' per move
2) You are only allowed to move a value of '1' to a direct neighbor/adjacent cell


##### Example 1:

```
Input: @list = (1, 0, 5)
Output: 4

Move #1: 1, 1, 4
(2nd cell gets 1 from the 3rd cell)

Move #2: 1, 2, 3
(2nd cell gets 1 from the 3rd cell)

Move #3: 2, 1, 3
(1st cell get 1 from the 2nd cell)

Move #4: 2, 2, 2
(2nd cell gets 1 from the 3rd cell)
```

##### Example 2:

```
Input: @list = (0, 2, 0)
Output: -1

It is not possible to make each same.
```

##### Example 3:

```
Input: @list = (0, 3, 0)
Output: 2

Move #1: 1, 2, 0
(1st cell gets 1 from the 2nd cell)

Move #2: 1, 1, 1
(3rd cell gets 1 from the 2nd cell)
```

### Solution

This task is pretty clear with the description and the given examples. 
I used the same two step-strategy in all three languages I submitted.

1. Figure out if the list is evenly distributable at all
2. Calculate the number of moves to evenly distribute the list

The empty list and lists with a single element are already evenly distributed,
so we return `0` as the number of moves for these cases.

An even distribution is only possible if the total of `@list` is divisible by
its length. If so, the quotient of that division is the target value for each
cell in `@list`. With the target value, the required number of moves can be
calculated in a single iteration of `@list`.

```
Input: @list = (1, 0, 5)
Target: 2

1) 1 0 5
   ^
Steps: 0
Carry: 0
```
<br/>

There is `1` missing to the target value in the first position. We can only get
it from the right neighbor. So we add `1` to the number of moves and carry a
*debt* of `1` over to the next position


```
2) 1 0 5
     ^
Steps: 1
Carry: -1
```
<br/>

There is `2` missing from the target value and we still have to pay the debt of
`1` from the previous steps. We don't want to ruin the equilibrium we just
created in the left neighbour, and take  the `3` from the right neighbor. This
ads `3` to the number of moves and we continue with a debt of `3` aswell.

```
3) 1 0 5
       ^
Steps: 4
Carry: -3
```
<br/>

At this position we are 3 above the target, and also have accumulated a debt of
3. This cancels out and no further moves are required to equalize the list.
Since we made sure that the list is evenly distributable, this has to be
the case in the last position.

The number of steps corresponds to the total of the absolutes of the carry over
values. So if we can find a way to calculate the carry over at each position,
pass the result to the next step, and collect all intermediate values in the end
we can use that as the core of our solution.

In Haskell the process, of iteratively applying a function to an element of a a
list and a starting value/accumalutor and collecting all intermediate results is
known as a `scan`
([`scanl`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:scanl)).
In Perl the left scan is called
[`reductions`](https://perldoc.perl.org/List::Util#reductions), and in Raku
[`produce`](https://docs.raku.org/routine/produce).

```Perl
reductions { $a + $b - $target } 0, @list;
```

Starting with a carry of `0` this gives us a list with the carries from the
process described above. We can that transform the value of this list to their
absolutes and calculate the total.

##### Perl

```Perl
sub equal_distribution (@list) {
    return 0 if @list <= 1;
    my $total = sum0(@list);
    return -1 if $total % @list != 0;
    my $target = $total / @list;

    sum0 map { abs($_) } reductions { $a + $b - $target } 0, @list;
}
```

##### Raku

```Raku
sub equal-distribution(Int @list --> Int) {
    if (@list.elems <= 1 ) {
        return 0;
    } 

    my $total = @list.sum;

    if ($total % @list.elems != 0) {
        return -1;
    }

    my $target = $total div @list.elems;
    return (0, |@list).produce({$^a + $^b - $target}).map(&abs).sum;
}
```

##### Haskell

In Haskell I decided to return `Nothing` instead of `-1` for undistributable
lists and a Natural number (>= 0) of steps

```Haskell
equalDistribution :: [Int] -> Maybe Natural
equalDistribution [] = Just 0
equalDistribution xs =
  let total = sum xs
      target = total `div` length xs
      isDistributable = total `mod` length xs == 0
  in if isDistributable
     then Just
       $ fromIntegral
       $ sum
       $ map abs
       $ scanl (\a b -> a + b - target) 0 xs
     else Nothing
```


#### Full Sources

- [Perl](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-192/alexander-pankoff/perl/ch-2.pl)
- [Raku](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-192/alexander-pankoff/raku/ch-2.raku)
- [Haskell](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-192/alexander-pankoff/haskell/ch-2.hs)
