from string import ascii_lowercase


def make_types(i: int) -> str:
    return ' '.join([x+'_'+str(i) for x in ascii_lowercase][:i])


def make_chain(i: int) -> str:
    return ' '.join([x+'_'+str(i)+' -> ' for x in ascii_lowercase][:i])


def make_yoneda(i: int, x: bool) -> str:
    types = make_types(i)
    chain = make_chain(i)
    return f"""{"" if x else "else "}instance yo{i} :: Yonedable ({chain} (term -> term) -> (f term)) ({chain} f term) where
  yo ctr {types} = ctr {types} identity
"""


if __name__ == '__main__':
    n_free = 20
    s = ''
    s = s + '\n' + \
        ('module Control.Monad.Freer.Yo where')
    s = s + '\n' + ('import Prelude')
    s = s + """
import Control.Monad.Free (Free)

type AsFree = forall func end. ((end -> end) -> func end) -> Free func end
class Yonedable d (a :: Type) | d -> a, a -> d where
  yo :: d -> a
"""
    for x in range(n_free):
        s = s + '\n' + (make_yoneda(x, x == 0))
    with open('src/Control/Monad/Freer/Yo.purs', 'w', encoding='utf8') as w:
        w.write(s)
