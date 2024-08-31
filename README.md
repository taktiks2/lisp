Lisp practice

以下でサーバーを起動する
https://github.com/vlime/vlime?tab=readme-ov-file#quickstart
```
sbcl --load ~/.local/share/nvim/lazy/nvlime/lisp/start-vlime.lisp
```

デバッガがおかしくなったら以下を実行
```
abort
```

sbclを終了する方法
```
(exit)
```

vim起動前に以下を実行してサーバーを起動する
```
sls
```

vim起動後に`<leader>rr`でサーバーと接続すると補完が効くようになる


`.lisp`ファイルの実行方法
```
clisp {file}.lisp
```
