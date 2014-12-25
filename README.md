blazeMarker
===========
Following the shell monad ( http://hackage.haskell.org/package/shell-monad ) the ASM monad ( http://wall.org/~lewis/2013/10/15/asm-monad.html ) and the brainfuck monad ( http://hackage.haskell.org/package/brainfuck-monad ) here is a very experimental FreeMarker monad.
Example use:

    test = FM.renderFM $ do
        bar <- FM.dec $ FM.litS "string blablabla"
        lol <- FM.und "varName" $ FM.litN 123
        FM.exlitl
        FM.litH $ do
            B5.div $ do
                R$<< bar
                th "test2"
        FM.lit "test"
        foo <- FM.dec $ FM.true
        FM.ifel (foo =~ FM.false)
            (do
                FM.litH $ do
                    B5.div $ do
                        th "ok true div"
            )
            (FM.lit "false")
        FM.list ( FM.litL $ litN 123 >: litN 321 >: [] ) $ \vi -> do
            D$< vi
        FM.list ( FM.litNL [12,321,32] ) $ \vi -> do
            D$< vi
