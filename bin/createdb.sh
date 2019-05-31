#!/usr/bin/env bash

DB=purescript-httpure-registration-example

if [ "$1" == "R" ]; then
    dropdb "$DB"
    createdb "$DB"
fi
psql -d "$DB" < sql/createdb.sql
