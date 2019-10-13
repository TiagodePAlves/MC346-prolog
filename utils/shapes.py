from __future__ import annotations

from sympy import Rational, geometry as geo
from numpy import sqrt
from matplotlib import patches, axes
from itertools import combinations
from typing import Tuple, Iterator, Type, Union, Iterable



class Figure:
    shape_name: str = None

    def __init__(self, name: str, shape):
        self.name = name
        self.shape = shape

    @property
    def center(self) -> Tuple[float, float]:
        return tuple(float(x) for x in self.shape.center)

    @property
    def radius(self) -> float:
        return float(self.shape.radius)

    def distance(self, point: Tuple[float, float]) -> float:
        raise NotImplementedError

    def intersect_with(self, other: Figure) -> bool:
        raise NotImplementedError

    @staticmethod
    def parse_float(value: float, prec=5) -> str:
        parsed = f'{value:.{prec}f}'
        parsed = parsed.rstrip('0')
        parsed = parsed.rstrip('.')
        return parsed

    @staticmethod
    def parse_rational(value: Union[str, float]) -> str:
        if not isinstance(value, str):
            value = Figure.parse_float(value)
        return Rational(value)

    def __repr__(self) -> str:
        measures = [self.parse_float(x) for x in self.center + (self.radius,)]
        return f'{self.shape_name}({", ".join([self.name] + measures)})'

    @classmethod
    def from_str(cls: Type[Figure], text: str) -> Figure:
        shape_name, args = text.strip().split('(')
        if shape_name != cls.shape_name:
            raise ValueError("invalid shape")

        args = [arg.strip() for arg in args.strip('( )').split(',')]

        name = args.pop(0)
        return cls(name, *args)

    @staticmethod
    def any_from_str(text: str) -> Figure:
        for cls in Figure.__subclasses__():
            try:
                return cls.from_str(text)
            except ValueError:
                pass
        raise ValueError(f"unknown shape: '{text}'")

    def as_patch(self, **kwargs) -> patches.Patch:
        raise NotImplementedError



def solve(shapes: Iterable[Figure]) -> Iterator[Tuple[str, str]]:
    def name(figure: Figure) -> str:
        return figure.name

    for this, other in combinations(sorted(shapes, key=name), 2):
        if this.intersect_with(other):
            yield this.name, other.name



class Circle(Figure):
    shape_name = "circ"

    def __init__(self, name, *args):
        x, y, radius = map(self.parse_rational, args)
        super().__init__(name, geo.Circle(geo.Point(x, y), radius))

    def as_patch(self, **kwargs) -> patches.Patch:
        kwargs['label'] = self.name
        return patches.Circle(self.center, self.radius, **kwargs)

    def distance(self, point: Tuple[float, float]) -> float:
        cx, cy = self.center
        px, py = point

        dist = sqrt((px - cx)**2 + (py - cy)**2)
        dist = max(0, dist - self.radius)
        return dist

    def intersect_with(self, other: Figure) -> bool:
        return other.distance(self.center) <= self.radius


class Square(Figure):
    shape_name = "quad"

    def __init__(self, name, *args):
        x, y, radius = map(self.parse_rational, args)
        super().__init__(name, geo.RegularPolygon(geo.Point(x, y), radius/2, 4))

    @property
    def radius(self):
        return float(2 * self.shape.radius)

    def as_patch(self, **kwargs) -> patches.Patch:
        kwargs['label'] = self.name
        return patches.Rectangle(tuple(x - self.radius/2 for x in self.center), self.radius, self.radius, **kwargs)

    @staticmethod
    def closest_dim(limit: float, center: float, val: float) -> float:
        if val > center + limit:
            return center + limit
        elif val < center - limit:
            return center - limit
        else:
            return val

    def closest_point(self, point: Tuple[float, float]) -> Tuple[float, float]:
        r = self.radius
        cx, cy = self.center
        px, py = point

        dx = self.closest_dim(r/2, cx, px)
        dy = self.closest_dim(r/2, cy, py)
        return dx, dy

    def distance(self, point: Tuple[float, float]) -> float:
        cx, cy = self.closest_point(point)
        px, py = point

        return sqrt((cx - px)**2 + (cy - py)**2)

    def intersect_with(self, other: Figure) -> bool:
        cx, cy = self.closest_point(other.center)
        ox, oy = other.center
        dx, dy = cx - ox, cy - oy

        if other.shape_name == self.shape_name:
            dist = max(abs(dx), abs(dy))
            return dist <= other.radius/2
        else:
            dist = sqrt(dx**2 + dy**2)
            return dist <= other.radius
