import requests
import pkg_resources
from pathlib import Path


def path_to_data(year: int, day: int) -> Path:
    return Path(
        pkg_resources.resource_filename("advent", f"data/{year:04d}/{day:02d}")
    ).absolute()


def get_data(year: int, day: int, *, overwrite: bool = False) -> str:
    path = path_to_data(year, day)
    if path.exists() and not overwrite:
        return path.read_text()

    key_path = Path.home() / ".config" / "aoc" / "key"
    if not key_path.exists():
        raise FileNotFoundError(f"No session key file found at {key_path}")
    key = key_path.read_text().strip()

    url = f"https://adventofcode.com/{year}/day/{day}/input"
    r = requests.get(url, cookies={"session": key})
    r.raise_for_status()

    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(r.text)
    return path.read_text()
