# Modifying Bety schema

Changes to the Bety schema happen through the Ruby on Rails database migration system.
Here is a [detailed guide](http://edgeguides.rubyonrails.org/active_record_migrations.html) on database migration for Ruby on Rails.
What follows is a lightning guide to the essential steps.

1. If you have not done so already, `git clone` the Bety repository. If you are on the VM, it should already be present in `~/bety`.

2. `cd` into the `bety` repository and update to the latest version with `git pull`.

3. Make sure all Ruby gems are up to date. Running `bundle check` should do a quick check.
   If anything is out of date, update with `bundle install --path vendor/bundle`.
   (Adding the `--path` argument is strongly recommended to isolate these gems from the system install.)
   If this throws an error, see [Troubleshooting](#modifying-bety-troubleshooting) below.

4. Check the `config/database.yml` file to make sure that database parameters are set correctly.
   If you have multiple instances of Bety installed (e.g. `development`, `production`), adjust parameters accordingly.
   You can name each database environment whatever you want, but this guide assumes you are working with the `production` configuration (the default on the VM).
   The default VM configuration should look like this:

```
production:
  adapter: postgis
  encoding: utf-8
  reconnect: false
  database: bety
  pool: 5
  username: bety
  password: bety
```

5. Update the Bety database itself with the latest migrations by running `bundle exec rake db:migrate RAILS_ENV=production` (if you are using a different BETY instance, change `production` to whatever you are working on).
   If this command exits silently, then your bety instance is up to date.
   If it throws an error, see [Troubleshooting](#modifying-bety-troubleshooting) below.

6. Modifying the database involves adding new Ruby scripts to the `db/migrate` directory.
   In a nutshell, each migration describes a database revision (`up` -- how to make the change) and its inverse (`down` -- how to undo the change).
   There are lots of examples of previous migrations in this directory that can be used for reference.
   Database operations can be performed by passing SQL queries directly to an `execute %q{<YOUR QUERY HERE>}` statement, or through Ruby helper commands like `change_column`.
   Although you can name migrations whatever you want, because they are run in numerical-alphabetical order, the convention is to prefix your script with the current year, month, day, hour, minute, and second (you can generate this string by running `date +%Y%m%d%H%M%S` on the command line).

7. To apply the new database changes, re-run `bundle exec rake db:migrate RAILS_ENV=production` from the bety repository root directory.

8. To undo changes, use the `db:rollback` task (i.e. `bundle exec rake db:rollback RAILS_ENV=production`).
   Each call of `db:rollback` will undo one migration.
   Optionally, if you want to make multiple changes, add the `STEP=n` (e.g. `STEP=3`) flag to undo the last `n` migrations.
   If you want to modify the last migration, you can use the shortcut task `db:rollback:redo` (shorthand for `db:rollback`, then `db:migrate`).


## Troubleshooting {#modifying-bety-troubleshooting}

**Error related to `pg_dump` non-existent option `-i`**

This is a bug the results from versions mismatches between Rails and Postgres.
The _correct_ solution is to update to Rails 2.4.6 or greater.
The simpler solution is to manually `grep` through the `vendor/bundle` folder in your Bety instance, searching for instances of `pg_dump` and manually remove the `-i` arguments.
This should only be in a few (~3) files.
Before making these manual changes, be sure to run `bundle install --path vendor/bundle` to make sure you are modifying the most up-to-date gems.
Otherwise, your changes will be overwritten.
There is no need to re-run `bundle install` or any other command after these changes -- they should propagate to the Rails app immediately and automatically.

For reference, see [this Stack Overflow question](https://stackoverflow.com/questions/35999906/pg-dump-invalid-option-i-when-migrating/39130103).

**Error related to "can't activate <some gem>...already activated"**

This is related to mismatches between the "installed" and required versions of a gem, or a mismatch between a gem's version and the version expected by its dependencies.
The basic solution is to uninstall the offending gem with `bundle exec gem uninstall <gem>` ^[Note that this command may throw an error like "undefined method 'delete' for bundler". This is normal, and the gem was actually uninstalled.] and then re-install with `bundle install --path vendor/bundle`.
If this doesn't work, you may have to `rm -rf` the entire `vendor/bundle` directory and then re-install all gems with `bundle install --path vendor/bundle`.

For reference, see [this Stack Overflow question](https://stackoverflow.com/questions/4308070/gem-bundler-load-error-cant-activate-already-activated).
