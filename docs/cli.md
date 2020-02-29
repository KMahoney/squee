# The Squee Command Line

## Database Environment Variables

As with the default PostgreSQL CLI, Squee connects to the database described by the environment variables `PGHOST` `PGDATABASE` `PGPASSWORD` etc.

## REPL

```
squee repl
```

will start Squee REPL. 

## Check

```
squee check FILENAME
```

will typecheck the Squee file against the database.

## Generate

```
squee generate GENERATOR FILENAME
```

will generate code from all `export` declarations in `FILENAME`.

Currently available generators include:

### sql-prepare

Generates standard SQL `PREPARE` statements.

### hs-postgresql-simple

Generates Haskell for querying the database via the `postgresql-simple` library.
