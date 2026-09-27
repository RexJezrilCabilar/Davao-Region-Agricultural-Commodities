"""
Fetches the PSA OpenSTAT retail-price table and writes the latest price per
region into docs/data/, where the static dashboard in docs/ picks it up.
Runs on a schedule via .github/workflows/update-data.yml.
"""

import io
import json
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd
import requests

API_URL = "https://openstat.psa.gov.ph:443/PXWeb/api/v1/en/DB/2M/2018NEW/0042M4ARN01.px"

query_payload = {
    "query": [
        {"code": "Geolocation", "selection": {"filter": "all", "values": ["*"]}},
        {"code": "Commodity", "selection": {"filter": "item", "values": ["0"]}},  # RICE, WELL-MILLED, 1 KG
        # "all" instead of a hardcoded list of year codes: PSA appends a new code
        # every year, so pinning specific codes (e.g. "6,7,8") quietly stops
        # picking up new years once they roll past whatever's hardcoded here.
        # Fetching everything costs a slightly bigger response but guarantees
        # "latest" always really is the latest.
        {"code": "Year", "selection": {"filter": "all", "values": ["*"]}},
        {"code": "Period", "selection": {"filter": "all", "values": ["*"]}},
    ],
    "response": {"format": "csv"}  # json-stat2 is broken for this table server-side; csv works
}

OUT_DIR = Path("docs/data")
CSV_PATH = OUT_DIR / "latest_produce_prices.csv"
JSON_PATH = OUT_DIR / "latest_produce_prices.json"
META_PATH = OUT_DIR / "meta.json"

MONTH_MAP = {
    "January": 1, "February": 2, "March": 3, "April": 4, "May": 5, "June": 6,
    "July": 7, "August": 8, "September": 9, "October": 10, "November": 11, "December": 12
}


def fetch_with_retries(url, payload, attempts=3, timeout=60):
    """POST to the PXWeb endpoint, retrying on network errors and non-200 responses."""
    last_err = None
    for attempt in range(1, attempts + 1):
        try:
            print(f"📡 Contacting OpenSTAT server (attempt {attempt}/{attempts})...")
            resp = requests.post(url, json=payload, timeout=timeout)
            if resp.status_code == 200:
                return resp
            print(f"❌ Target server refused transmission. Status: {resp.status_code}")
            print(resp.text[:500])
            last_err = f"HTTP {resp.status_code}"
        except requests.RequestException as e:
            print(f"🔌 Request error on attempt {attempt}: {e}")
            last_err = str(e)
        if attempt < attempts:
            wait = 5 * attempt
            print(f"   retrying in {wait}s...")
            time.sleep(wait)
    raise RuntimeError(f"giving up after {attempts} attempts ({last_err})")


def main():
    response = fetch_with_retries(API_URL, query_payload)

    # The CSV comes back wide: one row per Geolocation, one column per "Year Period"
    # combo (e.g. "2024 January", "2024 Annual", "2025 January", ...).
    # PSA/PX-Axis marks missing/not-yet-published cells with "." or "..".
    df_wide = pd.read_csv(io.StringIO(response.text), na_values=[".", "..", ":", "-"])

    id_cols = ["Geolocation", "Commodity"]
    value_cols = [c for c in df_wide.columns if c not in id_cols]

    df = df_wide.melt(id_vars=id_cols, value_vars=value_cols,
                       var_name="YearPeriod", value_name="Price_PHP")

    # Split "2024 January" -> Year=2024, Month=January
    split_cols = df["YearPeriod"].str.extract(r"^(\d{4})\s+(.+)$")
    df["Year"] = split_cols[0].astype(int)
    df["Month"] = split_cols[1]

    # Drop the yearly "Annual" summary column, keep only real months
    df = df[df["Month"] != "Annual"].copy()

    # Drop rows with no published price yet (future months, etc.)
    df["Price_PHP"] = pd.to_numeric(df["Price_PHP"], errors="coerce")
    df = df.dropna(subset=["Price_PHP"])

    if df.empty:
        raise RuntimeError("parsed 0 rows with a published price — PSA may have changed the table's format")

    df["Month_Num"] = df["Month"].map(MONTH_MAP)
    df = df.sort_values(by=["Geolocation", "Year", "Month_Num"])

    latest_prices_df = df.groupby("Geolocation").last().reset_index()
    latest_prices_df = latest_prices_df.drop(columns=["Month_Num", "YearPeriod"])

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    latest_prices_df.to_csv(CSV_PATH, index=False)
    latest_prices_df.to_json(JSON_PATH, orient="records", indent=2, force_ascii=False)
    META_PATH.write_text(json.dumps({
        "last_updated_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "rows": len(latest_prices_df),
    }, indent=2))

    print(f"✅ Filter operation complete! Wrote {len(latest_prices_df)} rows to {CSV_PATH} and {JSON_PATH}.")
    print(latest_prices_df.head(10))


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"🔌 Critical pipeline error: {e}")
        sys.exit(1)  # non-zero exit so the Actions run actually shows as failed
