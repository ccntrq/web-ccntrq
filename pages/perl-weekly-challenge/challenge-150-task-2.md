---
title: "Challenge 150 Task #2 - Squarefree integers"
---

## [Task #2]{#Task2}

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-150/#TASK2)


Write a script to generate all square-free integers <= 500.

> In mathematics, a square-free integer (or squarefree integer) is an integer
> which is divisible by no perfect square other than 1. That is, its prime
> factorization has exactly one factor for each prime that appears in it. For
> example, 10 = 2 ⋅ 5 is square-free, but 18 = 2 ⋅ 3 ⋅ 3 is not, because 18 is
> divisible by 9 = 3**2.
    
**Example**

```
The smallest positive square-free integers are
    1, 2, 3, 5, 6, 7, 10, 11, 13, 14, 15, 17, 19, 21, 22, 23, 26, 29, 30, ...
```


### Solution

[Full Source](https://github.com/ccntrq/perlweeklychallenge-club/tree/challenge-150/challenge-150/alexander-pankoff/perl/ch-2.pl)

From the description of the task we learn what we have to do for this challenge:

Search through the first 500 integers for the ones that are squarefree and print
out the result.

So how do we check if a number is squarefree?

According to the task description we will factorize the given number into its
prime components and check if there are any duplicates in the factors. We'll
start by laying out this framework and filling in the details later.

We will employ Perls builtin `grep` routine to do the filtering for us. The
filter predicate is a `is_squarefree` routine that we are about to define in the
next step. We then go on to print out the filtered lists a comma separated
values.

```perl
my @square_free = grep { is_squarefree($_) } 1 .. 500;
say join( ', ', @square_free );
```

`is_squarefree` is defined by a straightforward translation of the process described
above into Perl syntax.

```perl
sub is_squarefree($x) {
    my @prime_factors = prime_factors($x);
    return no_dupes(@prime_factors);
}
```

With this in place we can now start getting our hands dirty on the details.

Let's take care of the prime factors. First of all, it's important to know, that
every natural number has a unique prime factorization. In other words, we should
be able to define a routine that works for every integer input.
If we had an ordered list of all primes, we could walk this list, until
we find a prime that evenly divides our number. This will then be our first
prime factor. If we continue this process with the quotient of the division
until it reaches 1 we have found all prime factors. Since it's impossible to
calculate all primes and we don't know the maximum prime we will need, we need a
way to lazily get and calculate the `nth` prime. (Actually we know the maximum
prime we need for the task will be 499, but I think it's boring to pre-calculate
them and build a solution that will not work for inputs greater than 500)
In Haskell or Raku we would probably reach for a lazy list for our primes, but
in Perl we don't have that at hand. Instead we will use a subroutine that we can
ask for the `nth` prime and have it calculate it for us.

```perl
sub prime_factors($x) {
    my @factors;
    my $prime = 0;
    while ( $x > 1 ) {
        my $test_factor = primes($prime);
        $prime++;
        next unless $x % $test_factor == 0;
        push @factors, $test_factor;
        $x     = $x / $test_factor;
        $prime = 0;
    }
    return @factors;
}
```

For our makeshift lazy prime list we use a `state` list that will persist
previously calculated primes across different invocations of the `sub`. We can
then check if we already calculated the prime at the requested index `$n` and
only if we don't have it yet we kick of the process of generating the missing
primes up to the index. At last we return the requested index from our `@primes`
list.

```perl
sub primes($n) {
    state @primes = (2);

    for ( my $i = $primes[-1] + 1 ; $#primes < $n ; $i++ ) {
        push @primes, $i if is_prime($i);
    }

    return $primes[$n];
}
```

To check if a number is prime we hardcode the result for values up to 3. For all
other values we walk from 2 to the symmetry axis of division and check if we
find an even divisor of `$x`. If we didn't find a divisor we know our number is
a prime. This is by far not the fastest or most optimal process of finding prime
numbers. It's fast enough for our purposes though and also straightforward to
implement and easy to understand. That's why I'm using it here.

```perl
sub is_prime($x) {
    return 0 if $x <= 1;
    return 1 if $x <= 3;
    for ( my $i = 2 ; $i < sqrt($x) ; $i++ ) {
        return 0 if $x % $i == 0;
    }
    return 1;
}
```

The final piece needed to finish the implementation is the `no_dupes` routine
that checks if the given list is free of duplicates. We do this by iterating
through the list, incrementing a counter in a `%seen` hash for each value in the
list. In each iteration we also check if the counter gets bigger than one and
return 0 when that is the case, as it means we found a duplicate. If we make it
through the loop we know, that no duplicates have been found and we return a 1.

```perl
sub no_dupes(@xs) {
    my %seen;
    for my $x (@xs) {
        $seen{$x} += 1;
        return 0 if $seen{$x} > 1;
    }
    return 1;
}
```
