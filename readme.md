# Squee: A Typed, Composable Database Query Language

**Squee** is an experimental language that breaks relational queries down in to *composable* functions that can be fully inferred with a few extensions to Hindley Milner.

**WARNING**: Squee is **EXPERIMENTAL** and **INCOMPLETE**. Don't use this for anything important!

## Docs

* [The Language in Detail](docs/language.md)
* [The Command Line](docs/cli.md)

## Rationale

As much as I love languages with a good type system, integrating types with relational querying is often a weak point. It can be complicated, clunky, and result in poor error messages.

Squee's type system has been designed to handle relational concepts and give good, understandable error messages (WIP - error messages are currently terrible).

Unlike SQL, Squee breaks queries down in to composable parts so you can define and re-use joins, maps and filters across your code.

## Some Quick Examples

Given a PostgreSQL database with the following definition:

```sql
CREATE TABLE example (a int not null, b text not null);
INSERT INTO example VALUES (1, 'example1'), (2, 'example2');

CREATE TABLE join_example (a int not null, c text not null);
INSERT INTO join_example VALUES (1, 'join_example1'), (2, 'join_example2');
```

Squee will introspect the database and make tables available:

```
SQUEE> example

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 1 | example1 |
| 2 | example2 |

SQUEE> join_example

: [{a: ~int4, c: ~text}]

| a | c             |
+---+---------------+
| 1 | join_example1 |
| 2 | join_example2 |
```

Queries can be built out of `map`, `filter` and `natjoin`:

* `map : ({α} → {β}) → [{α}] → [{β}]`
* `filter : ({α} → ~bool) → [{α}] → [{α}]`
* `natjoin : ({γ} = {α} ⋈ {β}) ⇒ [{α}] → [{β}] → [{γ}]`

The types are explained in the [language docs](docs/language.md).

The filter function can be abstracted and used in multiple queries:

```
SQUEE> example | filter (\t -> t.a = 1)

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 1 | example1 |

SQUEE> def filterA1 := filter (\t -> t.a = 1)

filterA1 : [{a: ~int4, ..α}] → [{a: ~int4, ..α}]

SQUEE> filterA1 example

: [{a: ~int4, b: ~text}]

| a | b        |
+---+----------+
| 1 | example1 |

SQUEE> filterA1 join_example

: [{a: ~int4, c: ~text}]

| a | c             |
+---+---------------+
| 1 | join_example1 |
```

as well as the natjoin function:

```
SQUEE> example | natjoin join_example

: [{a: ~int4, b: ~text, c: ~text}]

| a | b        | c             |
+---+----------+---------------+
| 1 | example1 | join_example1 |
| 2 | example2 | join_example2 |

SQUEE> def joinExample := natjoin join_example

joinExample : ({β} = {a: ~int4, c: ~text} ⋈ {α}) ⇒ [{α}] → [{β}]

SQUEE> example | filterA1 | joinExample

: [{a: ~int4, b: ~text, c: ~text}]

| a | b        | c             |
+---+----------+---------------+
| 1 | example1 | join_example1 |
```

The queries can be exported to other languages.

Given the file `example.squee`:
```
export exportedExample := example
export filteredExporedExample a := example | filter (\t -> t.a = a)
```

The command `squee generate sql-prepare example.squee` will generate:

```sql
PREPARE exportedExample AS
  SELECT "a","b" FROM "example" AS x;

PREPARE filteredExporedExample AS
  SELECT "a","b" FROM "example" AS x WHERE ("a") = ($1);
```

## Notes and Limitations

* Squee doesn't currently handle nulls or nullable fields.
* There is a flat namespace for fields (as opposed to SQL's qualified field names i.e. `table.column`), so you can't join two tables containing the same field name without joining on those fields. This is a choice rather than a technical limitation - let's see how annoying it is!
* Very few SQL operators/functions are currently available. Basically just arithmetic operations (`=` `+` `-` `*` `/`) right now.
* Only natural joins are currently available. Arbitrary join conditions shouldn't be too hard, but left/right joins require null handling.
* I'm not sure how to handle aggregations yet.
* The next objective is quality error reporting.

## Interesting Links

* [The Links Programming Language](https://links-lang.org/)
* [The Ur/Web Programming Language](http://impredicative.com/ur/)
* [LINQ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)

* [Generalizing Hindley-Milner Type Inference Algorithms](https://www.researchgate.net/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms)
* [Row-based Effect Types for Database Integration](http://homepages.inf.ed.ac.uk/slindley/papers/corelinks.pdf)

If you know of any similar projects not listed here, please let me know!
