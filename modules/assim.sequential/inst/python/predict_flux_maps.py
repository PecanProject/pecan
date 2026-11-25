"""Run the installed PEcAn flux-map backend as an independent Python process."""

import argparse
import csv
import json
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("preflight", "run", "report"))
    parser.add_argument("--config", required=True)
    parser.add_argument("--resume", action="store_true")
    parser.add_argument("--inventory", help="Write the completed map inventory as CSV")
    args = parser.parse_args()
    if args.resume and args.command != "run":
        parser.error("--resume requires run")
    if args.inventory and args.command == "preflight":
        parser.error("--inventory requires run or report")
    try:
        if not sys.platform.startswith("linux"):
            raise RuntimeError("Flux map prediction requires Linux")
        from pecan_downscaling.configuration import load
        from pecan_downscaling.predict.engine import execute

        result = execute(args.command, load(args.config), resume=args.resume)
        if args.inventory:
            rows = result["products"]
            with open(args.inventory, "w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
                writer.writeheader()
                writer.writerows(rows)
        else:
            print(json.dumps(result))
    except (ImportError, ValueError, RuntimeError, OSError) as exc:
        parser.exit(1, f"Flux map prediction failed: {exc}\n")


if __name__ == "__main__":
    main()
