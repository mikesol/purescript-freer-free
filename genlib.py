from string import ascii_lowercase


def make_types(i: int) -> str:
    return ' '.join([x+'_'+str(i) for x in ascii_lowercase][:i])


def make_chain(i: int) -> str:
    return ' '.join([x+'_'+str(i)+' -> ' for x in ascii_lowercase][:i])


def make_yoneda(i: int) -> str:
    types = make_types(i)
    chain = make_chain(i)
    return f"""___{i} :: forall trans {types} terminus functor. (functor terminus -> Free trans terminus) -> ((forall exists. {chain} (terminus -> exists) -> functor exists)) -> {chain} (Free trans terminus)
___{i} _t ctr {types} = _t $ ctr {types} identity"""


def make_thunk(i: int) -> str:
    types = make_types(i)
    chain = make_chain(i)
    return f"""___{i}' :: forall trans {types} functor. (functor Unit -> Free trans Unit) -> ((forall exists. {chain} exists -> functor exists)) -> {chain} (Free trans Unit)
___{i}' _t ctr {types} = _t $ ctr {types} unit
"""


def make_yoneda_sig(i: int) -> str:
    types = make_types(i)
    chain = make_chain(i)
    return f"""forall {types} terminus. ((forall exists. {chain} (terminus -> exists) -> functor exists)) -> {chain} (Free trans terminus)
"""


def make_thunk_sig(i: int) -> str:
    types = make_types(i)
    chain = make_chain(i)
    return f"""{f"forall {types}." if i != 0 else " "}((forall exists. {chain} exists -> functor exists)) -> {chain} (Free trans Unit)
"""


def free_monster_type(n_free: int) -> str:
    thunks = ',\n'.join(['m\'' + str(x)+' :: ' + make_thunk_sig(x)
                         for x in range(n_free)])
    yonedas = ',\n'.join(['m' + str(x)+' :: ' + make_yoneda_sig(x)
                          for x in range(n_free)])
    return """type FreeMonster trans functor = {
  %s , %s
}
""" % (thunks, yonedas)


def free_monster_impl(n_free: int) -> str:
    thunks = ',\n'.join(['m\'' + str(x)+' : ___' + str(x)+"' trans"
                         for x in range(n_free)])
    yonedas = ',\n'.join(['m' + str(x)+' : ___' + str(x)+" trans"
                          for x in range(n_free)])
    return """freer :: forall trans functor. (forall terminus. functor terminus -> Free trans terminus) -> FreeMonster trans functor
freer trans = {
  %s , %s
}
""" % (thunks, yonedas)


if __name__ == '__main__':
    n_free = 20
    s = ''
    s = s + '\n' + \
        ('module Control.Monad.Freer.Free (freer, FreeMonster) where')
    s = s + '\n' + ('import Prelude')
    s = s + '\n' + ('import Control.Monad.Free(Free)')
    for x in range(n_free):
        s = s + '\n' + (make_thunk(x))
        s = s + '\n' + (make_yoneda(x))
    s = s + '\n' + (free_monster_type(n_free))
    s = s + '\n' + (free_monster_impl(n_free))
    with open('src/Control/Monad/Freer/Free.purs', 'w', encoding='utf8') as w:
        w.write(s)
