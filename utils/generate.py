from shapes import Figure, solve
from plformat import writelines, to_prolog_lines

from sympy import geometry as geo
from random import choice, choices, uniform
from string import ascii_lowercase
from itertools import starmap, chain
from functools import partial
from sys import stdout
from typing import (
    Tuple, Iterator, Dict, List, Type, Iterable,
    TypeVar, Any, Optional, TextIO, Callable
)

T = TypeVar('T')



def shapes() -> Iterator[Type[Figure]]:
    while True:
        yield choice(Figure.__subclasses__())

def names(length: int) -> Iterator[str]:
    while True:
        yield ''.join(choices(ascii_lowercase, k=length))

def points(max: float) -> Iterator[Tuple[float, float]]:
    while True:
        yield uniform(-max, max), uniform(-max, max)

def radii(max: float) -> Iterator[float]:
    while True:
        yield uniform(0.0, max)

def figures(max_distance: float, max_radius: float, name_length: int) -> Iterator[Figure]:
    def from_shape(shape, name: str, center: Tuple[float, float], radius: float) -> Figure:
        fig = shape(name, center[0], center[1], radius)
        return Figure.any_from_str(str(fig))

    figure_parts = zip(shapes(), names(name_length), points(max_distance), radii(max_radius))
    return starmap(from_shape, figure_parts)


def take(n: int, iterable: Iterable[T]) -> Iterator[T]:
    for _, val in zip(range(n), iterable):
        yield val



def args_parse(N=10, L=5, D=10.0, R=1.0) -> Dict[str, Any]:
    from argparse import ArgumentParser, FileType

    parser = ArgumentParser(description='Generate random circles and squares.')

    parser.add_argument('output', type=FileType('w'), default='-', nargs='?',
        help='output file to write the generated shapes (default: stdout)')
    parser.add_argument('solution', type=FileType('w'), nargs='?',
        help="output file to write the solution for the intersections of shapes (default: don't solve)")

    parser.add_argument('-n', '--amount', metavar='INT', type=int, default=N,
        help=f'amount of shapes to generate (default: {N})')
    parser.add_argument('-l', '--name-length', metavar='INT', type=int, default=L, dest='name_length',
        help=f'length of the generated names (default: {L})')
    parser.add_argument('-d', '--max-dist', metavar='NUM', type=float, default=D, dest='max_distance',
        help=f'maximum distance from origin (default: {D:.1f})')
    parser.add_argument('-r', '--max-radius', metavar='NUM', type=float, default=R, dest='max_radius',
        help=f'maximum radius for any shape (default: {R:.1f})')
    parser.add_argument('--prolog', action='store_const', const=to_prolog_lines, default=partial(map, str), dest='format',
        help='format shapes as a prolog list')

    return vars(parser.parse_args())


def main():
    args = args_parse()

    output = args.pop('output')
    solution = args.pop('solution')
    amount = args.pop('amount')
    formatting = args.pop('format')


    shapes = list(take(amount, figures(**args)))
    writelines(output, formatting(shapes))

    if solution:
        def aggregate(names: Tuple[str, str]) -> str:
            return f'{names[0]}-{names[1]}'

        solutions = sorted(map(aggregate, solve(shapes)))
        writelines(solution, formatting(solutions))


if __name__ == "__main__":
    main()
