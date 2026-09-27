# Rice Retail Price Monitor

A small pipeline that pulls well-milled rice retail prices from the [PSA OpenSTAT](https://openstat.psa.gov.ph) database on a schedule, and serves the latest numbers as a static dashboard on GitHub Pages.

```
.github/workflows/update-data.yml   the scheduled job
scripts/fetch_prices.py             fetches + reshapes the PSA data
docs/                               the GitHub Pages site
  index.html / style.css / app.js   the dashboard
  data/                             written by the workflow, read by the dashboard
```

## How it fits together

1. GitHub Actions runs `scripts/fetch_prices.py` on a schedule (and on demand).
2. The script writes `docs/data/latest_produce_prices.csv`, `latest_produce_prices.json`, and `meta.json`.
3. The workflow commits those files if they changed.
4. Because GitHub Pages is set to serve the `docs/` folder, that push alone triggers Pages to rebuild. There's no separate deploy step.
5. `docs/index.html` fetches the JSON at runtime and renders it.

## Setup

1. Copy everything in this folder into the root of a GitHub repo, so `.github/`, `scripts/`, and `docs/` sit at the top level, not nested one level down. Commit and push.
2. In the repo, go to **Settings → Pages**. Under "Build and deployment", set **Source** to "Deploy from a branch", pick the `main` branch and the `/docs` folder, then save.
3. Still in Settings, go to **Actions → General → Workflow permissions** and confirm "Read and write permissions" is selected. The workflow also requests this itself via its `permissions:` block, so you likely won't need to touch this. It's here in case your organization enforces a stricter default.
4. Go to the **Actions** tab, select "Update rice price data", and click **Run workflow** to populate real data right away instead of waiting for the schedule.
5. Once it finishes, open the URL shown on the Pages settings page (`https://<username>.github.io/<repo>/`). The placeholder-data notice should be gone and the numbers should match what the workflow printed in its log.

## Customizing

- **Schedule**: edit the `cron` line in the workflow. It's in UTC; `0 22 * * *` is 6am Philippine time. [crontab.guru](https://crontab.guru) helps if you want a different cadence.
- **Commodity or regions**: change the `Commodity` or `Geolocation` filters in `fetch_prices.py`'s `query_payload` to track a different item or a subset of regions.
- **History**: the script currently keeps only the latest month per region, matching the original script this was built from. Since it now pulls the whole year range anyway, it wouldn't take much more to also keep the full series in a second JSON file and add a trend line to the dashboard, if that'd be useful later.

## Known rough edges

- PSA's OpenSTAT server is a government stats portal and is occasionally slow or briefly down. The script retries three times with a short backoff before giving up, but a run can still fail outright. If the workflow reports a failure, check the Actions log for the printed status code first.
- Scheduled workflows are paused by GitHub automatically after 60 days with no repository activity at all. If the dashboard suddenly stops updating and there's no failed run in the Actions tab, this is the usual reason. Push anything, or trigger the workflow manually, to wake it back up.
