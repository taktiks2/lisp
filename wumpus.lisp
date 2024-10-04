(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
;; NOTE: 必要？
;; (defparameter *player-pos* nil)
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

;;; eqlとequalの違い
;;; eql: 数値やシンボルの比較に使う
;;; equal: リストや文字列の比較に使う
;;; (equal "hello" "hello") => T (内容が同じ)
;;; (eql "hello" "hello") => NIL (異なるメモリアドレス)

;; 各ノードに直接接続されているエッジのリストを値とするalistを生成する関数
;; exp: (edges-to-alist '((1 . 2) (2 . 1) (3 . 4) (4 . 3))) => ((1 (2)) (2 (1)) (3 (4)) (4 (3) (3)))
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1) ; mapcar: リストの各要素に対して関数を適用してリストを生成する
            (cons node1
             (mapcar (lambda (edge)
                       (list (cdr edge)))
                     (remove-duplicates (direct-edges node1 edge-list) ; remove-duplicate: リストから重複を削除する
                                        :test #'equal)))) ; :test: 比較関数を指定する
          (remove-duplicates (mapcar #'car edge-list))))
(edges-to-alist '((1 . 2) (2 . 1) (3 . 4) (4 . 3) (2 . 3)))

;; ノードのalistにcopsを追加する関数
;; exp: (add-cops '((1 (2)) (2 (1)) (3 (4)) (4 (3))) '((1 . 2) (2 . 1) (3 . 4) (4 . 3))) => ((1 (2 cops)) (2 (1 cops)) (3 (4)) (4 (3)))
;; exp: (intersection '(1 2 3) '(2 3 4)) => (2 3)
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2) ; intersection: ２つのリストの共通部分を取得する
                                                  edges-with-cops
                                                  :test #'equal) ; :test: 比較関数を指定する
                                    (list node2 'cops) ; 共通部分が存在する場合はcopsを追加
                                    edge))) ; 共通部分が存在しない場合はそのまま追加
                            node1-edges))))
          edge-alist))

;; 都市のエッジを生成する関数
;; exp: (make-city-edges) => ((1 (2 cops)) (2 (1 cops)) (3 (4)) (4 (3)))
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* ; 1から*node-num*までの数値の入ったリストを生成
                      collect i))
         (edge-list (connect-all-islands nodes (make-edge-list))) ; ランダムなエッジリストを生成して、すべての島をつなぐ橋を生成
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*))) ; 1/*cop-odds*の確率でcopを配置
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; alistから指定したノードの値を取得する関数
;; exp: (neighbors 1 '((1 (2) (3)) (2 (1)))) => (2 3)
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

;; 2つのノードが隣接しているかを判定する関数
;; exp: (within-one 1 2 '((1 (2)) (2 (1)))) => (2)
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist))) ; member: リストに指定された要素が含まれているか検証する

;; 2つのノードが2つのエッジ以内を介して隣接しているかを判定する関数
;; exp: (within-two 2 4 '((1 (2) (3)) (2 (1)) (3 (1) (4)) (4 (3)))) => NIL
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist) ; or: 引数のいずれかがTの場合にTを返す
      (some (lambda (x) ; some: リストの各要素に対して関数を適用し、一つでも真の場合にTを返す
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

;; 街の各ノードにwumpusやglow-wormsを配置する関数
;; exp: (make-city-nodes '((1 (2 cops)) (2 (1 cops)) (3 (4)) (4 (3)))) =>
;((1 SIRENS!) (2 SIRENS!) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12) (13) (14)
; (15 GLOW-WORM) (16) (17) (18) (19) (20) (21) (22) (23 GLOW-WORM) (24) (25)
; (26 GLOW-WORM) (27) (28) (29 WUMPUS) (30) 
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node)) ; wumpusをランダムなノードに配置
        (glow-worms (loop for i below *worm-num* ; loop for {num} below {num} collect: {num}回繰り返し、collectでリストを生成する
                     collect (random-node)))) ; glow-wormsを規定数ランダムなノードに配置
    (loop for n from 1 to *node-num* ; 街の各ノードにたいしてループ
          collect (append (list n) ; append: リストに要素を追加する
                          (cond ((eql n wumpus) '(wumpus)) ; wumpusがいるノードには`wumpusを配置
                                ((within-two n wumpus edge-alist) '(blood!))) ; nとwumpusとの距離が2以内の場合は'blood!を配置
                          (cond ((member n glow-worms) ; member: リストに指定された要素が含まれているか検証する
                                 '(glow-worm)) ; glow-wormがいるノードには'glow-wormを配置
                                ((some (lambda (worm) ; some: リストの各要素に対して関数を適用し、一つでも真の場合にTを返す
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                 '(lights!))) ; glow-wormの隣のノードには'lights!を配置
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!)))))) ; 接続しているエッジにcopsがいるときには'sirens!を配置

;; シンボルのないノードを探索する関数
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node) ; 何のシンボルもないノードが見つかるまで再帰実行
        x))) ; 何のシンボルもないノードを返す

(defun known-city-nodes ()
  (mapcar (lambda (node) ; mapcar: リストの各要素に対して関数を適用してリストを生成する
           (if (member node *visited-nodes*) ; すでに訪れたノードかどうかを検証
               (let ((n (assoc node *congestion-city-nodes*))) ; cityから指定したノードのalistを取得
                 (if (eql node *player-pos*) ; nodeとプライヤーの場所が一致しているか検証
                     (append n '(*)) ; 一致している場合は'*'を追加
                     n)) ; 一致していない場合はそのまま追加
               (list node '?))) ; 未訪問のノードには'?を付与
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car
                               (cdr (assoc node *congestion-city-edges*))))
                     *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges)) ; フィールドエッジの生成
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*)) ; フィールドノードの生成
  (setf *player-pos* (find-empty-node)) ; プレイヤーの初期値を設定
  (setf *visited-nodes* (list *player-pos*)) ; プレイヤーの初期値を訪れたリストに追加
  (draw-city)
  (draw-known-city))
