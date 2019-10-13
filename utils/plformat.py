from functools import partial
from pyswip import Prolog
from typing import (
    Iterable, Iterator, TypeVar, Optional,
    Callable, Union, TextIO, Tuple, List
)

T = TypeVar('T')


def writelines(file: TextIO, iterable: Iterable[T]):
    for val in iterable:
        print(val, file=file)


def maplines(func: Callable[[Iterable[str]], Iterable[str]], inp: TextIO, out: TextIO):
    writelines(out, func(map(str.strip, inp)))


def mapfile(func: Callable[[str], str], inp: TextIO, out: TextIO):
    maplines(partial(map, func), inp, out)


def take_and_peek(iterable: Iterable[T]) -> Iterator[Tuple[T, Optional[T]]]:
    iterator = iter(iterable)
    try:
        this = next(iterator)
    except StopIteration:
        return

    for val in iterator:
        yield this, val
        this = val
    yield this, None


def to_prolog_lines(iterable: Iterable[T], indent: int=4) -> Iterator[str]:
    space = " " * indent

    yield '['
    for val, next_val in take_and_peek(iterable):
        val_str = space + str(val)
        if next_val is not None:
            val_str += ','
        yield val_str
    yield '].'


def read_prolog_list(file: Union[str, TextIO], parser: Callable[[str], T]=str) -> List[T]:
    if isinstance(file, str):
        file = open(file, 'r')

    text = ''.join(map(str.strip, file.readlines()))
    file.close()

    elems = next(Prolog().query(f'X = {text}'))['X']
    return [parser(x.value) for x in elems]



def args_parse() -> Tuple[bool, TextIO, Optional[TextIO]]:
    from argparse import ArgumentParser, FileType

    parser = ArgumentParser(description='Read a text file and write a prolog list.')

    parser.add_argument('input', type=FileType('r'), default='-', nargs='?',
        help='input file to read shapes (default: stdin)')
    parser.add_argument('output', type=FileType('w'), default='-', nargs='?',
        help='output file to write the drawing (default: stdout)')

    parser.add_argument('--from-prolog', action='store_true', dest='frompl',
        help='read a prolog list and write a text file instead')

    args = parser.parse_args()
    return args.frompl, args.input, args.output


def main():
    from_prolog, infile, outfile = args_parse()

    if from_prolog:
        writelines(outfile, read_prolog_list(infile))

    else:
        maplines(to_prolog_lines, infile, outfile)


if __name__ == '__main__':
    main()
