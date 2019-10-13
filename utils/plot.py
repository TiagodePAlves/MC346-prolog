from shapes import Figure
from plformat import read_prolog_list

from itertools import cycle
from matplotlib import pyplot
from matplotlib.patches import Patch
from matplotlib.colors import to_rgb, TABLEAU_COLORS as colors
from typing import (
    Tuple, Optional, TextIO, Optional,
    Union, Iterable, Iterator
)



def read_figures(file: Union[str, TextIO]) -> Iterator[Figure]:
    if isinstance(file, str):
        file = open(file, 'r')

    lines = iter(file)

    try:
        line = next(lines)
        if line.startswith('['):
            raise ValueError
        yield Figure.any_from_str(line)
    except StopIteration:
        pass
    except ValueError:
        file.seek(0)
        lines = read_prolog_list(file)

    for line in lines:
        yield Figure.any_from_str(line.strip())


def shapes_to_patches(shapes: Iterable[Figure], facealpha: float=0.3, edgealpha: float=0.5) -> Iterator[Patch]:
    for shape, color in zip(shapes, map(to_rgb, cycle(colors))):
        fc = color + (facealpha,)
        ec = color + (edgealpha,)

        yield shape.as_patch(facecolor=fc, edgecolor=ec)



def args_parse() -> Tuple[TextIO, Optional[TextIO]]:
    from argparse import ArgumentParser, FileType

    parser = ArgumentParser(description='Draw circles and squares.')

    parser.add_argument('input', type=FileType('r'), default='-', nargs='?',
        help='input file to read shapes (default: stdin)')
    parser.add_argument('output', type=FileType('wb'), nargs='?',
        help="output file to write the drawing (default: show on a window)")

    args = parser.parse_args()
    return args.input, args.output


def main():
    infile, outfile = args_parse()

    axes = pyplot.axes()

    def name(figure: Figure) -> str:
        return figure.name

    for patch in shapes_to_patches(sorted(read_figures(infile), key=name)):
        axes.add_patch(patch)

    pyplot.legend()
    pyplot.autoscale(True)
    pyplot.axis(option='equal')
    pyplot.grid(which='both')
    pyplot.minorticks_on()

    if outfile:
        pyplot.savefig(outfile)
    else:
        pyplot.show()


if __name__ == "__main__":
    main()
