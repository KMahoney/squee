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
