# ScrapeBotR
Retrieve Data from a ScrapeBot Database

ScrapeBotR (with "R") allows to easily retrieve (large amounts of) data from a [ScrapeBot](https://github.com/MarHai/ScrapeBot) installation. The package provides easy-to-use functions to read and export instances, recipes, runs, log information, and data. Thereby, the package plugs neatly into the tidyverse as it makes heavy use of tibbles.

The [ScrapeBot](https://github.com/MarHai/ScrapeBot) (without "R") is a tool for so-called "agent-based testing" to automatically visit, modify, and scrape a defined set of webpages regularly. It was built to automate various web-based tasks and keep track of them in a controllable way for academic research, primarily in the realm of computational social science.


## Installation

Install the most recent development version using `devtools`:

```
devtools::install_github('MarHai/ScrapeBotR')
```


## Usage

Import the installed version ...

```
library(ScrapeBotR)
```

... and start using it by defining your [ScrapeBot](https://github.com/MarHai/ScrapeBot) database. Credentials to access your database need to be stored in an INI file somewhere in your computer's home directory (i.e., under `~/`, which usually translates  to `/home/my_user` under *nix or `C:\Users\my_user\Documents` under Windows). You can either create this file by hand or use the ScrapeBotR's helper function to create it:

```
write_credentials(
  host = 'path_to_my_database',
  user = 'database_username',
  password = 'database_password'
)
```

Alternatively, you can create the INI file manually. Ideally, the file is located directly within your home directory and named `.scrapebot_database.ini` (where the leading `.` prevents it from being shown in the file browser most of the time). The INI file is essentially just a raw-text file with a so-called _section_ name and some _key-value pairs_, each of which cannot contain spaces between a key and its value. Any unnecessary settings can be omitted (e.g., the port number). Here's how the INI file could look like:

```
[a name for me to remember]
host=localhost
port=3307
user=my_personal_user
password=abcd3.45d:cba!
database=scrapebot
```

Once you got that out of the way, try connecting to your database, using the _section_ name again (this is because you can have multiple sections referring to multiple ScrapeBot installations):

```
connection <- connect('a name for me to remember')
```

If this doesn't yield an error, you are good to go. And you could start, for example, by ...

- listing the available recipes through `get_recipes()`
- listing the available instances through `get_instances()`
- get information about specific runs through `get_runs()`
- Collect data via `get_run_data()`
- bulk-download-and-compress screenshots from S3 via `collect_screenshots_from_s3()`
- ...

Detailed documentation is available for every function inside R.
