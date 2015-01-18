#!/bin/bash
psql -d testapp -f create_events_table.sql
psql -d testapp -f create_users_table.sql
