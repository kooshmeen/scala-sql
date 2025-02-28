# Scala SQL-like Database

A lightweight SQL-like database implementation in Scala using functional programming concepts.

## Core Components

- `Table`: Represents a database table with rows and columns
- `Database`: Collection of tables with operations like create, drop, and join
- `FilterCond`: Trait for filtering rows with implementations for various conditions

## Features

- Table operations: insert, delete, update, filter, sort, select columns
- Database operations: create/drop tables, join tables
- Query DSL with implicit conversions for cleaner syntax
- Functional implementation with pattern matching and composition
