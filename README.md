[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/QpjOED6Z)
# Map

The purpose of this assignment is to practice your understanding of higher-order functions in functional programming with Haskell. This includes the creation of lambda expressions, the composition of functions, point-free style programming, and partial application.

## Topic

The topic of the assignment is a functional map data structure. A map is a collection of key-value pairs. In this assignment, we will represent maps as functions that map characters to optional integers. For example, the map containing the entries `('a', 1)`, `('b', 2)`, and `('c', 3)` can be represented as the following function:

```haskell
-- The map {'a' -> 1, 'b' -> 2, 'c' -> 3}
m :: Char -> Maybe Int
m 'a' = Just 1
m 'b' = Just 2
m 'c' = Just 3
m _ = Nothing
```

This representation enables the definition of higher-order functions that can perform operations on these maps. This could include operations such as union, intersection, or difference. 

## Task

The task of the assignment is developing an API for a functional map data structure using Haskell. You are provided with a code skeleton that you need to complete. The goal is to turn this code into a module, `Map`, that exposes the required features while preserving encapsulation.

Below is a partial list of functions you will need to implement (see [`Map.hs`](src/Map.hs) for the complete list):

- `empty`: Creates an empty map.
- `singleton`: Creates a map with a single entry.
- `insert`: Adds an entry to the map.
- `delete`: Removes an entry from the map.
- `member`: Checks if a key is in the map.
- `size`: Returns the number of entries in the map.
- `union`: Returns the union of two maps.
- `intersection`: Returns the intersection of two maps.
- `difference`: Returns the difference of two maps.
- `toList`: Converts the map to a list.
- `fromList`: Converts a list to a map.

Additionally, you need to implement a mechanism allowing Haskell to automatically compare maps together and convert maps into strings when necessary. The expected string representation for maps should be a pair of curly braces with inside all the entries separated with commas. For example, an empty map should be represented as `{}`, a singleton map with the entry `('a', 1)` as `{a -> 1}`, and a map with the entries `('a', 1)` and `('b', 2)` as `{'a' -> 1, 'b' -> 2}`, and so on.

You may find the `intercalate` function from the [Data.List](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html) module useful when implementing the string representation feature. This function can be used to insert a specific string between each element in a list of strings.

Here's a simple example of how you might use `intercalate`:

```
ghci> import Data.List (intercalate)
ghci> xs = ["Hello", "world!"]
ghci> intercalate ", " xs
"Hello, world!"
```

You are free to use anything that was introduced or discussed in class for implementing the API. You can also use Haskell's extended standard library for lists if this can help you during implementation. However, you should minimize as much as possible converting functional maps into lists. Doing so goes against the whole purpose of the assignment. For some functionalities, there is no choice to do this conversion, but it can be done without for quite a lot of of them.

Given the fact that we shall represent maps as functions mapping characters to integers, this representation can induce significant performance overhead when dealing with large maps. To make things simple, we will restrict the set of possible map keys within the range of [ASCII](https://en.wikipedia.org/wiki/ASCII) code points. No need to implement checks for keys outside of this range.

## How-to

While working on your Haskell module, you don't need to compile it to test it. Instead, you can simply load it into the Haskell interpreter, GHCi. There are two ways to do this:

1. Start GHCi with the filename as a command-line argument.

```bash
% ghci Map.hs
```

2. Start GHCi without any arguments, and then load the source file using the `:l` command (which is short for load).

```bash
% ghci
ghci> :l Map.hs
```

In both cases, GHCi will load your module and you can then call your functions directly from the GHCi prompt to test them. Remember to reload your module with `:r` every time you make changes to the source file.

## Grading

The grading of the assignment will be based on the following criteria:

* __Correctness__: The code should compile without errors and perform the task as per the assignment's requirements. It should handle all edge cases and should not crash under any circumstances.
* __Efficiency__: The code should be optimized to run as efficiently as possible. This includes minimizing the time and space complexity of the code. It should not contain unnecessary computation or unused code.
* __Readability__: The code should be easy to read and understand. This includes proper indentation, the use of meaningful variable and function names, and the inclusion of comments to explain complex parts of the code.
* __Modularity__: The code should avoid code duplication and reuse implemented functionalities. This makes the code more maintainable, efficient, reliable, and readable.
* __Style__: The code should demonstrate an idiomatic functional programming style, employing suitable programming constructs to ensure the code is concise, modular, and easy to reason about.

Each criterion contributes a maximum of one point to the overall grade. The assignment grade is then computed using the following formula:

<p align="center">
$G = 1 + C_x + E_x + R_x + M_x + S_x$
</p>

where $C_x$, $E_x$, $R_x$, $M_x$, and $S_x$ represent the evaluation of your code based on each respective criterion.

## Deliverable

The deliverable for this assignment is a single Haskell file, named `Map.hs`. This file should include a header that identifies the author(s) of the code. The assignment can be completed individually or in pairs. However, it is crucial to ensure that the work submitted is original and not plagiarized. Copying code from the internet or using solutions from other students is strictly prohibited and will be considered a breach of academic integrity.

## Deadline

The deadline for the assignment is **Wednesday, October 30, 2024, at 23:55**. Once this deadline passes, the repository will be locked for grading purposes, and late submissions will not be accepted under any circumstances.
