---
title: "Challenge 151 Task #1 - Binary Tree Depth"
---

## [Task #1] - Binary Tree Depth

### Description

[Original Description](https://theweeklychallenge.org/blog/perl-weekly-challenge-151/#TASK1)

You are given binary tree.

Write a script to find the minimum depth.

> The minimum depth is the number of nodes from the root to the nearest leaf
> node (node without any children).

**Example 1:**

```
Input: '1 | 2 3 | 4 5'

                1
               / \
              2   3
             / \
            4   5

Output: 2
```


**Example 2:**

```
Input: '1 | 2 3 | 4 *  * 5 | * 6'

                1
               / \
              2   3
             /     \
            4       5
             \
              6
Output: 3
```

### Solution

[Full Source](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-151/alexander-pankoff/perl/ch-1.pl)


This week's challenges were a little unusual for me. Usually the first task is
easier and takes less time to implement. This week I spent way more time one the
first than on the second task.

To make processing of the input easier, I tokenize it before starting to
calculate the minimum depth. After the tokenization process we are left with a
list of `Value`-, `PlaceHolder`- and `SeparatorToken`s. I won't illustrate this
process here further. It's pretty straightforward, if you're interested you can
check it out in the source linked above.

To determine the minimum depth we walk the list of tokens and built up our
binary tree. We stop processing on the first node without children or when we
run out if input. In the case were we used up all input, we have completely
parsed the whole tree and the wanted depth is the total depth of the tree.  To
represent the tree we're using an array of arrays, each representing a layer of
the tree. For each layer we want 2 to the power of the layer's depth elements.
If we run out of tokens or encounter a separator token before a layer is
complete, we fill it with placeholders. For `ValueToken`s we check if the parent
node is defined, and add their value to the tree. If the parent is missing the
process will be aborted with an error message indicating where the invalid value
was in the input. The placeholders will add an undefined value to the tree. For
placeholders on even positions we also check their left neighbor is also a
placeholder, and if there is a parent node for that position. If these
conditions hold we found a node without children (the current parent). We stop
processing and return the current depth as our result. 

Have some code:

```perl
sub minimum_binary_tree_depth ( @tokens) {
    my $depth = 0;
    my $tree  = [];

    while (@tokens) {
        push @$tree, [];
        my $num_elems = 2**$depth;
        for ( my $i = 0 ; $i < $num_elems ; $i++ ) {

            if ( !@tokens || $tokens[0]->isa('SeperatorToken') ) {
                ## fill row with dummy placeholder tokens.
                unshift @tokens,
                  map { PlaceHolderToken->new(-1) }
                  0 .. ( $num_elems - 1 - $i );
            }

            my $cur = shift @tokens;
            if ( $cur->isa('ValueToken') ) {
                if ( $depth && !defined( $tree->[-2][ int( $i / 2 ) ] ) ) {
                    die join( " ",
                        "Missing parent for node with value",
                        $cur->{lexeme},
                        "at position",
                        $cur->pos_human_readable(),
                        "in input\n" );
                }
                push @{ $tree->[-1] }, $cur->{lexeme};
            }
            elsif ( $cur->isa('PlaceHolderToken') ) {
                if (   $i % 2
                    && !defined $tree->[-1][-1]
                    && ( !$depth || defined $tree->[-2][ int( $i / 2 ) ] ) )
                {
                    return $depth;
                }
                push @{ $tree->[-1] }, undef;
                ## do nothing
            }
        }

        $depth += 1;

        # handle optional seperatortoken
        if ( @tokens && $tokens[0]->isa("SeperatorToken") ) {
            shift @tokens;
        }
    }

    return $depth;
}
```