# Squee: A Typed, Composable Database Query Language

**Squee** is an experimental language that breaks relational queries down in to *composable* functions that can be fully inferred with a few extensions to Hindley Milner. It compiles to SQL and so can be used with relational databases.

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

Queries can be built out of `map`, `filter`, `order`, `natjoin`, `join` and `aggregate`:

* `map : ({α} → {β}) → [{α}] → [{β}]`
* `filter : ({α} → ~bool) → [{α}] → [{α}]`
* `order : (Comparable β) ⇒ ({α} → β) → [{α}] → [{α}]`
* `natjoin : ({γ} = {α} ⋈ {β}) ⇒ [{α}] → [{β}] → [{γ}]`
* `join : ({α} → {β} → ~bool) → ({α} → {β} → {γ}) → [{α}] → [{β}] → [{γ}]`
* `aggregate : ({β} = Agg {γ}) ⇒ ({α} → {β}) → [{α}] → [{γ}]`

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
export filteredExportedExample a := example | filter (\t -> t.a = a)
```

The command `squee generate sql-prepare example.squee` will generate:

```sql
PREPARE exportedExample AS
  SELECT "a","b" FROM "example" AS _t;

PREPARE filteredExportedExample AS
  SELECT "a","b" FROM "example" AS _t WHERE ("a") = ($1);
```

Since Squee is fully type inferred, it can also generate templates for languages that require type annotations:

`squee generate hs-postgresql-simple example.squee`:

```haskell
exportedExample :: Connection -> IO [(Int, String)]
exportedExample connection = do
  query_ connection "SELECT \"a\",\"b\" FROM \"example\" AS _t"

filteredExportedExample :: Connection -> Int -> IO [(Int, String)]
filteredExportedExample connection a = do
  query connection "SELECT \"a\",\"b\" FROM \"example\" AS _t WHERE (\"a\") = (?)" (Only a)
```

## Installation

Requirements:

- [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- PostgreSQL development libraries (e.g. on Debian/Ubuntu: `apt install libpq-dev`)
- ncurses development libraries (e.g. on Debian/Ubuntu: `apt install libtinfo-dev`)

Clone the repo, and then `stack install`.

## Notes and Limitations

* Squee doesn't currently handle nulls or nullable fields.
* Rows have a flat namespace, as opposed to SQL's qualified field names i.e. `table.column`. A row cannot have duplicate field names because there's no way to disambiguate them.
* Very few SQL operators/functions are currently available.
* Only natural and inner joins are currently available. Left/right joins require null handling.
* The error reporting is currently poor, but quality error reporting is a long-term goal of the project.

## Interesting Links

* [The Links Programming Language](https://links-lang.org/)
* [The Ur/Web Programming Language](http://impredicative.com/ur/)
* [LINQ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)

* [Generalizing Hindley-Milner Type Inference Algorithms](https://www.researchgate.net/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms)
* [Row-based Effect Types for Database Integration](http://homepages.inf.ed.ac.uk/slindley/papers/corelinks.pdf)

If you know of any similar projects not listed here, please let me know!
