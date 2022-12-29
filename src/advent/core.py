import importlib
from typing import Optional
from pathlib import Path

import click

from advent.io import get_data


@click.command()
@click.option("-y", "--year", default=2015, help="The year.")
@click.option("-d", "--day", help="The day.")
@click.option("-t", "--test", is_flag=True, help="Run the tests.")
def cli(year: int, day: Optional[int], test: bool):
    module_path = Path(__file__).absolute().parent

    click.echo(f"Year: {year}\n")
    year_path = module_path / f"year{year:04d}"

    if day is None:
        days = list(int(s.name[3:5]) for s in year_path.glob("day??.py"))
    else:
        days = [int(day)]

    tests = []
    for day in sorted(days):
        if test:
            tests.append(year_path / f"day{day:02d}.py")
        else:
            click.echo(f"Day: {day}")
            run_one(year, day)
            click.echo()

    if test:
        import pytest

        pytest.main(tests)


def run_one(year: int, day: int):
    try:
        module = importlib.import_module(f"advent.year{year:04d}.day{day:02d}")
    except (ImportError, ModuleNotFoundError):
        click.echo(f"No solution found for {year:04d}-{day:02d}")
        return
    data = get_data(year, day)

    if hasattr(module, "part1"):
        result = module.part1(str(data))
        click.echo(f"Part 1: {result}")
    else:
        click.echo("Part 1: not implemented")

    if hasattr(module, "part2"):
        result = module.part2(str(data))
        click.echo(f"Part 2: {result}")
    else:
        click.echo("Part 2: not implemented")
