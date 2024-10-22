from typing import List
from typing import Callable, Tuple


class Ctx[T]:
    def __init__(self, value: T):
        self.value = value

    

class InitCtx(Ctx[None]):
    def __init__(self, value: None = None):
        self.value = value

class Yield[T]:
    def __init__(self, value: T):
        self.value = value

class Stop(Yield[None]):
    def __init__(self, value: None = None):
        super().__init__(value)

type Generator[T] = Callable[[Ctx[T]], Tuple[Ctx[T], Yield]]


def range_(start: int, step: int, stop: int) -> Generator[int]:
    def gen(ctx: Ctx) -> Tuple[int, int]:
        value = ctx.value
        if isinstance(ctx, InitCtx):
            return (Ctx(start), Yield(start)) if start < stop else (InitCtx(), Stop())
        if isinstance(ctx, Ctx):
            return (Ctx(value + step), Yield(value + step)) if (value + step) < stop else (InitCtx(), Stop())
    return gen

def take(n:int, gen: Generator) -> List[int]:
    ctx = InitCtx()
    generated_list =[]
    for _ in range(n):
        ctx, res = gen(ctx)
        if isinstance(res, Stop):
            break
        
        generated_list.append(res.value)

    return generated_list


if __name__ == "__main__":
    gen = range_(0, 1, 10)
    print(take(15, gen))

