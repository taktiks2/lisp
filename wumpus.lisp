(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; 指定された範囲内でランダムな整数を生成する関数
;; exp: (random-node) => 1
(defun random-node ()
  (1+ (random *node-num*)))

;; 2つのノードを結ぶエッジを生成する関数
;; exp: (edge-pair 'a 'b) => ((a . b) (b . a))
(defun edge-pair (a b)
  (unless (eql a b) ; unless: 条件が満たされない場合にのみ実行される
    (list (cons a b) (cons b a)))) ; list: 引数をリストにまとめる

;;; funcallとapplyの違い
;;; funcall exp: (funcall #'max 1 2 3) => 3
;;; apply exp: (apply #'max '(1 2 3)) => 3
;;; funcallは引数を個別に列挙する
;;; applyは引数にリストを渡す

;; ランダムな数字のエッジのリストを生成する関数
;; exp: (make-edge-list) => ((2 . 3) (3 . 2) (5 . 1) (1 . 5) (4 . 7) (7 . 4))
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* ; append: 複数のリストを結合して一つのリストにする
                        collect (edge-pair (random-node) (random-node))))) ; loop repeat {num} collect: {num}回繰り返し、collectでリストを生成する

;; nodeが始点であるエッジだけを抽出する関数
;; exp: (direct-edges 'a '((a . b) (a . c) (b . a) (c . a)) => ((a . b) (a . c))
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) ; remove-if-not: 指定された条件に合わない要素をリストから除外する
                   (eql (car x) node)) ; eql: ２つのオブジェクトが同一であるか検証する
                 edge-list))

;;; mapcとmapの違い
;;; mapc exp: (mapc #'print '(1 2 3)) =>
;;; 1
;;; 2
;;; 3
;;; (1 2 3)
;;; map exp: (map 'list' #'1+ '(1 2 3)) => (2 3 4)
;;; mapcは関数の戻り値を無視して、引数のリストを返す
;;; mapは関数の戻り値をリストにして返す

;; あるnodeに接続しているnodeをすべて取得する関数
;; exp: (get-connected 'a '((a . b) (a . c) (b . a) (c . a))) => (a b c)
(defun get-connected (node edge-list)
  (let ((visited nil)) ; visited: 訪れたノードを記録する変数を宣言
    (labels ((traverse (node) ; traverse: 深さ優先探索を行う再帰関数を宣言
               (unless (member node visited) ; member: リストに指定された要素が含まれているか検証する
                 (push node visited) ; push: リストの先頭に要素を追加する
                 (mapc (lambda (edge) ; mapc: リストの各要素に対して関数を適用する
                         (traverse (cdr edge)))
                       (direct-edges node edge-list))))) ; 各エッジペアに対してtraverseを適用する
      (traverse node)) ; 宣言したtraverseを実行
    visited)) ; 訪れたノードを返す

;; 互いに接続されているノードのグループを取得する関数
;; exp: (find-islands '(1 2 3 4) '((1 . 2) (2 . 1) (3 . 4) (4 . 3)) => ((3 4) (2 1))
(defun find-islands (nodes edge-list)
  (let ((islands nil)) ; islinds: 互いに接続されたnodeのグループを記録する変数を宣言
    (labels ((find-island (nodes) ; find-island: 互いに接続されたnodeのグループを取得する再帰関数を宣言
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected))) ; set-difference: ２つのリストの差分を取得する
                 (push connected islands) ; connectedをislandsに追加
                 (when unconnected ; unconnectedが存在する場合にfind-islandを再帰実行
                   (find-island unconnected)))))
      (find-island nodes)) ; 宣言したfind-islandを実行
    islands)) ; 互いに接続されたnodeのグループを返す

;; 島同士をつなぐエッジを生成する関数
;; exp: (connect-with-bridges '((1 2) (3 4))) => ((1 . 3) (3 . 1))
(defun connect-with-bridges (islands)
  (when (cdr islands) ; islandsが2つ以上の要素を持つ場合
    (append (edge-pair (caar islands) (caadr islands)) ; append: 複数のリストを結合して一つのリストにする
            (connect-with-bridges (cdr islands)))))

;; すべての島をつなぐ橋を生成する関数
;; exp: (connect-all-islands '(1 2 3 4) '((1 . 2) (2 . 1) (3 . 4) (4 . 3))) => ((3 . 2) (2 . 3) (3 . 4) (4 . 3) (1 . 3) (3 . 1))
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))
