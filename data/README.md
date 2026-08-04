# Data

Place raw party text files here:

```text
data/raw/
  AfD/     *.csv
  Linke/   *.csv
  CDU/     *.csv
  CSU/     *.csv
  Grüne/   *.csv
  SPD/     *.csv
```

## Expected columns

Each CSV should include at least:

| Column | Description |
|--------|-------------|
| `Date` (or `created_at` / `Datum`) | Document date |
| `Text` (or `tweet` / `content`) | Full text |

Encoding: **UTF-8**.

## Privacy / licensing

- Do **not** commit full social-media dumps if the course data or platform ToS forbid redistribution.
- For the public GitHub repo, either:
  1. ship a **small anonymized sample**, or
  2. document how the data were collected and keep `data/raw/` gitignored.

Add to `.gitignore` if needed:

```gitignore
data/raw/**
!data/raw/.gitkeep
```
