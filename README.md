# basic-of-programing

F#による「プログラミングの基礎」の演習問題解答

## リンク

[「プログラミングの基礎」 浅井健一 著 サイエンス社](http://www.saiensu.co.jp/?page=book_details&ISBN=ISBN978-4-7819-1160-1&YEAR=2007)

[著者によるサポートページ](http://pllab.is.ocha.ac.jp/~asai/book/Top.html)

## 環境

* macOS High Sierra
* .NET Core 2.2.301
* Mono 6.0.0.313

## 使い方

* 演習問題

```bash
fsharpi scripts/ChapXX.fsx # XX には章番号を入力する
```

* メトロネットワーク最短路問題

```bash
dotnet run --project src/MetroNetwork/
```