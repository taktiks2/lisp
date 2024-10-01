;(load "graph-util")

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

(loop for i from 1 to 10 collect i)

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

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                     collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms)
                                 '(glow-worm))
                                ((some (lambda (worm)
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                 '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))
