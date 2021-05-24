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
