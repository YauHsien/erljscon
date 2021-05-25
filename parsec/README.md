parsec
=====
實作 Graham Hutton 的論文 "Higher-Order Functions for Parsing" ，也就是眾所周知的 Parser combinators 。

模組
----

```
% Parser 型別
-type parser(FromType, ToType)
    :: fun(([FromType]) -> [{ToType,[FromType]}]).
```
```
% 基本構成
primitives:curry/1
  % 將幾個非 Curry 函數轉換為 Curry 函數。
primitives:fail/0
  % 無條件解析失敗的解析器
primitives:succeed/1,2
  % 無條件成功解析的解析器
primitives:satisfy/1,2
  % 滿足基本條件解析器；基本條件式只判定第一個輸入元素
```

Lazy Evaluation 的技巧
----
閱讀[這種文獻](https://github.com/YauHsien/erljscon/blob/master/doc/10.1.1.63.3555.pdf)通常先碰到的往往文中以 Miranda 或 ML 本位提出 Lazy Evaluation 的基本機能，例如由 Haskell ，隨便寫一個函數，都是 lazily-evaluated 。但在任何一套原本 eagerly-evaluated 的程式語言，只要我們跟著本文一路寫到 `many P` 就會遭遇一些問題：如 Unit-test 會遇到 time-out 。

以前的實作，我的作法大概是類似[有人的處理方式](https://briones.io/posts/2016-10-08-getting-lazy-with-erlang/)，看起來很正規：將型態與操作寫得很漂亮、工整，

```
-type stream(A) :: fun(() -> halt | {A, stream(A)}.
```
```
-spec naturals() -> stream(integer()).
naturals() -> naturals(0).

-spec naturals(integer()) -> stream(integer()).
naturals(N)
  fun() ->
    {N, naturals(N + 1)}
  end.
```

我在[早期摸索的作法](https://github.com/YauHsien/erljscon/blob/master/include/lazy.hrl)，與上述的處理類似：
```
-record(lazy, { function :: function(),
                args :: list() }).
```

但是，在此我用了另一種思維：所謂遞迴定義、循環定義，首先能字面上將循環部份表達得清楚：
```
many(RecFunc, Inp) ->
    LazilyRecF = {rec, fun() -> RecFunc end},
    [ {X,Y,Rest2} || {X,Rest1} <- (unwrap(RecFunc))(Inp),
                     {Y,Rest2} <- (unwrap(LazilyRecF))(Rest1)
    ].
```
而 `unwrap/1` 則是做為函數呼叫的重要環節：
```
unwrap({rec, Wrap}) ->
    Wrap();
unwrap(Func) ->
    Func.
```

Lazy evaluation 的具體用處，見於 [src/combinators.erl](src/combinators.erl) 。
