# The Squee Language

## Overview

Some familiarity with ML-like languages (e.g. Ocaml, Haskell) will help a lot.

Queries are built out of the following core functions which are described below:

* `map : ({α} → {β}) → [{α}] → [{β}]`
* `filter : ({α} → ~bool) → [{α}] → [{α}]`
* `order : (Comparable β) ⇒ ({α} → β) → [{α}] → [{α}]`
* `natjoin : ({γ} = {α} ⋈ {β}) ⇒ [{α}] → [{β}] → [{γ}]`
* `join : ({α} → {β} → ~bool) → ({α} → {β} → {γ}) → [{α}] → [{β}] → [{γ}]`

## Types

**Database types** are prefixed by a tilde (`~`), e.g.

* `~int4`
* `~text`
* `~bool`

**Table rows** are surrounded by curly brackets, e.g.

`{a: ~int4, b: ~text}` is a table row containing fields `a` and `b` with types `~int4` and `~text` respectively.

**Type variables** are represented by Greek letters, e.g.

`α` and `β` can represent any type.

**Row polymorphic rows** are rows with a type variable at the end e.g.

`{a: ~int4, b: ~text, ..α}` is a table row containing fields `a` and `b` with types `~int4` and `~text` respectively, but can also contain additional fields.

**Query results** (containing multiple rows) are surrounded by square brackets, e.g.

`[{a: ~int4, b: ~text}]` is a query result containing rows of `{a: ~int4, b: ~text}`

**Functions** are represented by an arrow (`→`) e.g.

* `α → α` is the type of the identity function that takes any value and returns the same value.
* `~int4 → ~int4 → ~bool` can be considered a function that takes two integers and returns a boolean. Because the arrow is right associative (`~int4 → (~int4 → ~bool)`), it is technically a function that accepts an `~int4` and returns a new function `~int4 → ~bool` i.e. it can be partially applied.

**Type constraints** come before a double arrow (`⇒`) e.g.

* `(Num α) ⇒ α → α → α` takes two numbers (`~int4`, `~numeric`, etc.) and returns a number of the same type.
* `({γ} = {α} ⋈ {β}) ⇒ [{α}] → [{β}] → [{γ}]` takes two query results and returns their natural join. The intersection of the two rows must not be empty (they must have at least one field in common).

## Tables

Each table in your database is represented by its name. This is a complete query that will return the contents of the table (equivalent to `SELECT * FROM ...`) and its type is a set of rows containing the fields of the table.

Our example database will have the expressions:

```
example : [{a: ~int4, b: ~text}]
join_example : [{a: ~int4, c: ~text}]
```

## Lambdas

As in Haskell, lambdas are written:

```
\arg -> expression
```

For example, `\x -> x` is the identity function with the type `α → α`.

## Fields

Use dot notation to access fields, e.g.

```
\t -> t.x
```

is a function that takes a row and returns the value of its `x` field. It has type `{x: α, ..β} → α`. Note that the input row is row-polymorphic, i.e. this function can accept any row that has an `x` field.

## Row Syntax

Rows can be constructed using curly brackets, e.g.

```
{a: 1, b: 'Hello'}
```

constructs a row with type `{a: ~int4, b: ~text}`.

## The `filter` Function

The filter function has type `({α} → ~bool) → [{α}] → [{α}]`

```
SQUEE> filter (\t -> t.a = 1) example

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 1 | example1 |
```

## The `order` Function

The order function has type `(Comparable β) ⇒ ({α} → β) → [{α}] → [{α}]`

```
SQUEE> order (\t -> 0 - t.a) example

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 2 | example2 |
| 1 | example1 |
```

## The `map` Function

The map function has type `({α} → {β}) → [{α}] → [{β}]`

```
SQUEE> map (\t -> {a: t.a + 1, b: t.b}) example

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 2 | example1 |
| 3 | example2 |
```

## The `natjoin` Function

The natural join function has the type `({γ} = {α} ⋈ {β}) ⇒ [{α}] → [{β}] → [{γ}]`

```
SQUEE> natjoin example join_example

: [{a: ~int4, b: ~text, c: ~text}]

| a | b        | c             |
+---+----------+---------------+
| 1 | example1 | join_example1 |
| 2 | example2 | join_example2 |
```

## The `join` Function

The generalised inner join function has the type `({α} → {β} → ~bool) → ({α} → {β} → {γ}) → [{α}] → [{β}] → [{γ}]`.

The first argument is the condition on which to join, and the second argument merges the two rows in to one.

```
SQUEE> join (\a b -> a.a < b.a + 1) (\a b -> {a: a.a, b: a.b, c: b.c}) example join_example

: [{a: ~int4, b: ~text, c: ~text}]

| a | b        | c             |
+---+----------+---------------+
| 1 | example1 | join_example1 |
| 1 | example1 | join_example2 |
| 2 | example2 | join_example2 |
```

## Pipe Operator

The pipe (`|`) is an infix operator with type `α → (α → β) → β`:

```
SQUEE> example | filter (\t -> t.a = 1) | natjoin join_example

: [{a: ~int4, b: ~text, c: ~text}]

| a | b        | c             |
+---+----------+---------------+
| 1 | example1 | join_example1 |
```

## Assignment

Assign values and functions with the syntax:

```
def [name] := [expression]
```

For example,

```
SQUEE> def filteredExample := example | filter (\t -> t.a = 1)

filteredExample : [{a: ~int4, b: ~text}]

SQUEE> def identity := \x -> x

identity : α → α

SQUEE> identity filteredExample

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 1 | example1 |
```

## Exports

`export` has similar syntax to `def`. It signifies what queries should be included when generating code.

For example, if we have the file `example.squee`:

```
def notExported := example
export exportedExample := example
export filteredExportedExample a := example | filter (\t -> t.a = a)
```

Then the command `squee generate sql-prepare example.squee` will generate:

```sql
PREPARE exportedExample AS
  SELECT "a","b" FROM "example" AS _t;

PREPARE filteredExportedExample AS
  SELECT "a","b" FROM "example" AS _t WHERE ("a") = ($1);
```
