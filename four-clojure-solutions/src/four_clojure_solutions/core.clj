(ns four-clojure-solutions.core)

; #19 last element in a sequence

(defn my-last [xs]
	(comp first reverse)
)

(defn my-last2 [xs]
	( cond
		(empty? xs ) xs
		(= 1 (count xs)) (first xs)
		:else (my-last2 (rest xs))
	)
)

; #20 penultimate element in a sequence

(defn penultimate [xs]
	(comp my-last butlast)
)

; #21 nth element in a sequence

(defn mynth [xs n] 
  ( cond (empty? xs)  nil
  		 (= 0 n ) (first xs)
  		 :else (mynth (rest xs) (dec n) )
  )
)	

;#22 count a sequence

(defn my-count [xs]
 (loop  [xs2 xs cnt 0]
    (cond  (empty? xs2 ) cnt
           :else (recur (rest xs2) (inc cnt))
     )
 )
)

(defn mycount2 [xs]
  ( cond (empty? xs) 0
         :else ( inc  (mycount2 (rest xs)))
  )
 ) 
 

 
; #24  sum up a sequence
 (defn mysum [ coll ]
   ( reduce + coll )
 )
 
 (defn mysum2 [ coll ]
   ( apply + coll )
 )
 
(defn mysum3 [ coll ]
   ( cond (empty? coll ) 0
          (= 1 (count coll)) (first coll)
          :else (+ (first coll)
                   (mysum3 (rest coll))
                )
    )
 )
 
 (defn mysum4 [coll]
   (loop [sum 0  mycoll coll ]
       (if (empty? mycoll) sum
           (recur (+ (first mycoll) sum) 
                  (rest mycoll)
           )
       )
   )
 )
 
; #25 find odd numbers in a seq
(defn my-odd [xs]
	(filter odd? xs)
)

;#26 fibonacci sequence
;Write a function which returns the first X fibonacci numbers.

(defn fib [n]
  ( map first
     (take n
         (iterate  
             (fn  [ [ a b ] ]   (vector b (+' a b ) )   ) [ 1 1 ]   
          )
     )
   )
)

; #27 palindrome detector

(defn palindrome [xs]
	(=  xs (reverse xs))
)

 ; #28  Flatten a Sequence
; Write a function which flattens a sequence
; restriction: flatten 

(defn my-flatten [ xs]
		(cond  (empty? xs) xs
			   ( not (coll? (first xs)))  (cons (first xs) (my-flatten (rest xs)) )
			   :else  (concat (my-flatten (first xs)) (my-flatten (rest xs)) )
		)
)

; #29  Get the Caps
; Write a function which takes a string and returns a new string containing only the capital letters

(defn my-caps [s]
	(apply str 
	     (filter #(Character/isUpperCase %) (seq s) )
	)
 )
 
; #30 compress a sequence
;Write a function which removes consecutive duplicates from a sequence.

(defn my-compress [ xs]

	( let [ a  (first xs) b (second xs) ]
	  (cond  (empty? xs ) xs
	  		 (=  a b )  (my-compress (rest xs))
	  		 :else (cons a (my-compress (rest xs)))
	  )
	)
)

;#31 pack a sequence

(defn mypackseq [xs]
   (partition-by identity xs)
)

; #32
; Duplicate a Sequence
; Write a function which duplicates each element of a sequence

(defn dupeseq [xs]
   (cond (empty? xs) xs
         :else 
            (cons (first xs) ( cons     ( first xs) 
         								(dupeseq (rest xs))
         					 )
         	)
    )
)

(defn dupeseq2 [xs]
	(mapcat #(list % % ) xs)
)

; #33 replicate a sequence
;Write a function which replicates each element of a sequence a variable number of times. 
;[1 2 3] 2) '(1 1 2 2 3 3)
; [:a :b] 4) '(:a :a :a :a :b :b :b :b)

(defn my-replicate [ xs n ]
	(mapcat #( take n (repeat %)) xs)
)

; #34  Implement range
; Write a function which creates a list of all integers in a given range
; restrictions: range

(defn my-range [x y ]
   
	(loop [ lst [] cnt x ]
	
		(cond (= cnt y ) lst
			:else (recur (conj lst cnt ) (inc cnt) )
		)
	)
)
; #38 max value of variable number of parameters

(defn my-max [ & args ]
	(reduce #( if (> %1 %2 ) %1 %2 ) args)

)

; #39 Interleave Two Seqs
; Write a function which takes two sequences and returns the first item from each,
; then the second item from each, then the third, etc.
; restriction: interleave

(defn my-interleave [xs ys]
	(mapcat vector xs ys)
)

; #40 separate an item in a sequence by an arbitrary value  

(defn sepa [ sep xs ] 
    ( cond (empty? xs) xs
           (= 1 (count xs)) xs
           :else
              (cons (first xs) (cons sep (sepa sep (rest xs)))) 
	)
)

(defn sepa2 [sep xs ]
	(butlast (mapcat #( list %1 sep) xs ))
)

(defn sepa3 [ sep xs ]
   (let  [n (count xs) ]
	   (butlast (flatten (map list xs (take n (repeat sep)))))
	)
)

;#41 drop every nth item from a sequence
;[1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]

(defn my-drop-helper [xs n cnt]
    ( cond  (empty? xs) xs
            :else 
             	(cond (= 0 (mod cnt n))         (my-drop-helper (rest xs) n (inc cnt))
             	      :else (cons (first xs)    (my-drop-helper (rest xs) n (inc cnt))  )
             	)
    )
 )
 
(defn my-drop [xs n]
	(my-drop-helper xs n 1)
)

; #42 factorial

(defn factorial [n]
 	(cond (= 1 n)  1
         :else (* n (factorial (dec n)))
    )
)

(defn factorial2 [n]
	(reduce * (range 1 (inc n)))
)

; #44: Write a function which can rotate a sequence in either direction.
; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))

(defn rotate-helper [xs]
	 (into []( concat (rest xs) (list(first xs))))
)

(rotate-helper [1 2 3 4 5])

(take 5 (iterate rotate-helper [1 2 3 4 5]))

(defn rotate [n xs]
  (last(take n (iterate rotate-helper xs)))
)

(rotate 3 [1 2 3 4 5 6])

;#45  The iterate function can be used to produce an infinite lazy sequence.
;  (take 5 (iterate #(+ 3 %) 1))
(defn my-iterate [ f seed]
    (cons  
       seed 
          (lazy-seq (my-iterate f (f seed) ))
    )
 )
 
; #49 Write a function which will split a sequence into two parts.
;    3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]
;    1 [:a :b :c :d]) [[:a] [:b :c :d]]      can't use split at

(defn my-split [ n xs ]
    ( cond (empty? xs ) xs
	  :else ( list (take n xs )  (drop n xs))
    )
)
#50 split by type
;Write a function which takes a sequence consisting of items with different types 
; and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained,
 ;but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
 
 (defn split-by-type [ xs]
   (map #(second %1)(group-by class xs ))
 )
 ;#53
 ;Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. 
 ;If two sub-sequences have the same length, use the one that occurs first.
 ; An increasing sub-sequence must have a length of 2 or greater to qualify.


(defn gen-subseq [xs]
 (loop  [cnt (count xs) result [] ]
   
   (cond (= 0 cnt) result
         :else (recur (dec cnt) 
                      (concat (partition cnt 1 xs) result)
                )
   )
 ) 
)

(defn increasing [xs]
    (cond 
        (= xs (range (first xs) (inc(last xs))
           )
        ) true 
          
      :else false
    )
)

(defn get-max-sub-seq [xs]
  
  (let [ myseq  (filter #(>(count %1) 1) (filter increasing (gen-subseq xs)))] 
    
    (into [](first(filter #(= (count %1 ) (reduce max(map count myseq))  )   myseq)))
  )
)



; #55: Write a function that returns a map containing the number of occurences 
; of each distinct item in a sequence.
; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
; forbidden: frequencies

(defn my-freq [xs]
 ( into {}
  	(map #(assoc {} (first %1) (count %1) ) 
      (partition-by identity (sort xs))
  	)
 )
)
; second version
; I found update-values here : http://blog.jayfields.com/2011/08/clojure-apply-function-to-each-value-of.html

(defn update-values [m f & args]
 (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn my-freqencies [xs]
	(update-values  ( group-by identity (sort xs)) count)
)

;#56  Find distinct items
;  Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
;   [1 2 1 3 1 2 4]) [1 2 3 4])

; remove all 
(defn remover [item xs]
	(filter  #( not (= item %))  xs )

)
(defn my-distinct [xs]
   (cond  (= 1 (count xs) )  xs
   		  :else ( contains? (set (rest xs)) (first xs))
   		            (cons  (first xs) 
   		                   (my-distinct (remover (first xs) (rest xs) )  )
   		            )
   		         )
   )
)

; #61 map construction from two input sequnces

(defn my-zipmap [xs ys]
	( cond (or (empty? xs) (empty? ys)) { }
			:else (assoc (my-zipmap (rest xs) (rest ys) ) (first xs) (first ys) )
	)
)

(defn my-zipmap2 [ xs ys]
	(apply  merge (map #( assoc {} %1 %2 ) xs ys))
)

; #66 greatest common divisor

;  gcd(a,0) = a
;  gcd(a,b) = gcd(b, a  mod b )


(defn gcd [ a b]
	(cond (= 0 b ) a
		  :else (gcd b (mod a b))
	)
)
; #67 prime numbers
; calculate a sequence of all primes up to n

; how many divisors of n are there (excluding 1 and n. Primes should have zero

 (defn count-divisors [ n ]
   (count (filter true? (map #(= 0 (rem n %1)) (range 2 n) )))
  )
  
  (defn is-prime [n]
  	(cond (= 0 (count-divisors n)) true
  		  :else false
  	)
  )
  
  (defn calc-primes [ n ]
  	(filter is-prime (range 2 n))
  )
  
;#73 A tic-tac-toe board is represented by a two dimensional vector.
; X is represented by :x, O is represented by :o, and empty is represented by :e.
;  A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row.
;   Write a function which analyzes a tic-tac-toe board and returns :x if X has won,
;    :o if O has won, and nil if neither player has won.

(def board [[:x :x :x]
           [:e :e :e]
           [:x :e :o]]
) 
(def board2 [[:x :e :x]
             [:x :e :e]
             [:x :e :o]]
) 
(defn same-across [player xs ]
	(= player (first xs) (second xs) (last xs))
)

(defn horizontal-win [player board]
	( or  (same-across  player (first board))
	      (same-across  player (second board))
	      (same-across  player (last board))
	)
) 

(defn get-column [n board]
	(let [ row1 (first board)
	       row2 (second board)
	       row3 (last board)
	     ]
	     ( cond (= 1 n) (vector (first row1) (first row2) (first row3))
	     	    (= 2 n) (vector (second row1) (second row2) (second row3))
	     	    :else   (vector (last row1) (last row2) (last row3))
	     )
	)
)
(
(defn vertical-win [player board]
	( or  (same-across  player (get-column 1 board))
	      (same-across  player (get-column 2 board))
	      (same-across  player (get-column 3 board))
	)
)
defn get-diag [n board]
	(let [ row1 (first board)
	       row2 (second board)
	       row3 (last board)
	     ]
	     ( if   (= 1 n) 
	              (vector (first row1) (second row2) (last row3))
	     	      (vector (last row1) (second row2) (first row3))
	     )
	)
)
(defn diag-win [player board]
	( or  (same-across  player (get-diag 1 board))
	      (same-across  player (get-diag 2 board))
	)
)
(defn is-winner  [player board]
	(or (diag-win player board) (vertical-win player board) (horizontal-win player board))
)
(defn get-winner [ board]
	(cond (is-winner :o board) :o
	      (is-winner :x board) :x
	      :else nil
    )
)

  ; #74 filter perfect squares
  ;  Given a string of comma separated integers, 
  ;write a function which returns a new comma separated string that only contains the numbers which are perfect squares.
  ;test not run	   "4,5,6,7,8,9" yields  "4,9"
  
  ; this takes "4,5,6,7,8,9" and returns the seq ( 4 5 6 7 8 9 )
  (defn str-to-seq [ s ]
    (map read-string (clojure.string/split s #","))
  )
  
  (defn is-square? [n]
    (let [ x  (int (Math/sqrt n)) ]
    	(= (* x x) n)
    )
  )
  
  (defn filter-perfect-squares [ s ]
     (clojure.string/join ", " 
          (filter is-square? (str-to-seq s))
     )
  )
  
  
; #77  Write a function which finds all the anagrams in a vector of words.
; A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y.
;  Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. 
;  Each sub-set should have at least two words. Words without any anagrams should not be included in the result.

;  ["veer" "lake" "item" "kale" "mite" "ever"])
;     #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

(defn big-set? [xs]
   (>  (count xs ) 1)
)

(defn anagram [xs]
  (filter big-set?
     (set (    map set (vals (group-by sort xs ))
        )   
     )
   )
)

; #80 perfect numbers
;A number is "perfect" if the sum of its divisors equal the number itself.
; 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.
; is y a divisor of x

(defn is-divisor [x y]
   (= 0 (rem x y)) 
)

; Note that this particular definition
; does NOT include the number itself

(defn get-divisors [ n ]
      ( filter #( is-divisor n %1 ) (range 1 n ))
)

(defn sum-of-divisors [n]
   (apply + (get-divisors n))
)

(defn perfect? [n]
  (= n (sum-of-divisors n))
)

; find the perfect numbers between 1 and 500
(filter perfect? (range 1 500) )

; #81 set intersection

(defn my-intersection [xs ys]
  (cond 
        (empty? xs ) #{  }
        (= 1 (count xs))
           (cond
               (contains? ys (first xs)) 
                   (set (list (first xs)))
               :else #{ }
           )
           
          (contains? ys (first xs)) 
             (conj    ( my-intersection (rest xs) ys )    (first xs) )
          :else
             ( my-intersection (rest xs) ys)
   )
)
; #85: Write a function which generates the power set of a given set. The power 
; set of a set x is the set of all subsets of x, including the empty set and x 
; itself.
;  #{1 :a}    yields       #{#{1 :a} #{:a} #{} #{1}})
 
 ; in enhance, myset is a set of sets - we put the same item in each
 
(defn enhance [item myset]
		(map #(conj %1 item) myset)
)

(defn power-set [ myset ]
   (cond (empty? myset )  #{ }
   		 (= 1 (count myset)) #{  #{ } #{ (first myset) }   }
         :else
            (clojure.set/union  (power-set (rest myset))
                    (enhance (first myset) (power-set (rest myset)) )
            )
    ) 
)
 ; #88 Write a function which returns the symmetric difference of two sets. 
 ;The symmetric difference is the set of items belonging to one but not both of the two sets.
 ; ex.  #{1 2 3 4 5 6} #{1 3 5 7}    yields  #{2 4 6 7}
 
 (defn symmetric-diff [set1 set2]
  ( let [inter (clojure.set/intersection set1 set2) 
         unyun (clojure.set/union set1 set2)
         ]
    (set(filter (complement  inter) unyun))
   )
)
; #90 Cartesian product of two sets
;	(= (__ #{1 2 3} #{4 5})
;  #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
 
(defn cartesian [ set1 set2]
  (let [ p  (into [] set1) q (into[] set2)]
   (for [x p y q] [x y])
  )
)
;#95   Write a predicate which checks whether or not a given sequence represents a binary tree. 
Each node in the tree must have a value, a left child, and a right child.

(defn is-bintree [ xs ]
  (if  (not= (count xs) 3 )  false  
         (let [ value (first xs)   lson (second xs)   rson ( first (rest( rest xs))) ]
           (cond 
	            (coll? value) false
              ( and  (coll? lson) (coll? rson) )
                   (and (is-bintree lson) (is-bintree rson))
             
              (and (not (coll? lson))  (not (coll? rson)) ) true
              (not (coll? lson)) (is-bintree rson)
              :else (is-bintree lson)
           )
        )
	)
)

; #92 recognize roman numerals, given a string

(defn replace-several [content & replacements]
      (let [replacement-list (partition 2 replacements)]
        (reduce #(apply clojure.string/replace %1 %2) content replacement-list)))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn subtr-roman [s]   
  (replace-several s      #"IV" "-4-"
						  #"IX" "-9"
						  #"XL" "-40-"
						  #"XC" "-90-"
						  #"CD" "-400-"
						  #"CM" "-900-" 
  )
)

(defn convert-letter [s]
  (replace-several s      #"I" "-1-"
						  #"V" "-5-"
						  #"X" "-10-"
						  #"L" "-50-"
						  #"C" "-100-"
						  #"D" "-500-" 
						  #"M" "-1000-" 
  )
)

(defn roman [s]
   (reduce + 
       (map parse-int
          (filter (complement empty? )    
             (clojure.string/split(convert-letter (subtr-roman s)) #"-")
          )
       )
   )
)

(roman "MCMLIV")
(roman "MCMXC")
(roman "MMXIV")
(roman "XIII")

; #99 Write a function which multiplies two numbers and returns the result as a sequence of its digits.

(defn my-mult [x y]
	(map 
	   #(- (int %1) (int \0) )   (seq ( str (* x y)))   
	)
)

;  #102  convert string to camel case
(defn camel-case[ s ]
	(let [ words (clojure.string/split s #"-") ]
			(reduce str 
               ( first words) (map clojure.string/capitalize (rest words))
       )
	)
)

; #104  This is the inverse of Problem 92, but much easier.
; Given an integer smaller than 4000, return the corresponding roman numeral in uppercase,
;  adhering to the subtractive principle.

(def roman-map  	
			{ 
              			  1000 "M",    900  "CM",  
						  500  "D",    400  "CD",
						  100  "C",     90  "XC",
						  50   "L",     40  "XL",
						  10   "X",      9  "IX", 
						   5   "V",      4  "IV",
               			   1   "I"
            }				  
)

(def values  (reverse(sort (keys roman-map))))

; find the largest value in the sequence that can be subtracted from a number x

(defn candidate [x xs]
  (if (<= x 0) 0
      (apply max(filter #(<= %1 x) xs))
  )
 )

(defn roman-convert [n]
  (let
  	[x (candidate n values)]
  	(cond (= 0 n) ""
  	  :else (str (roman-map x) (roman-convert (- n x)) )
  	)
  )
)

;  #107 simple closure

(defn my-closure [base]
   (fn [exp]  (Math/pow base exp))
)

;  #118 Map is one of the core elements of a functional programming language. 
;  Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.
;   re-implement map     (map f coll1 coll2 etc. )

(defn my-map [f xs]
     (lazy-seq
	 	(cond  (empty? xs ) xs
	       :else (cons  (f (first xs)) (my-map f (rest xs)) )
	 	) 
	 )     
)
;#115 A balanced number is one whose component digits have the same sum on the left and right halves of the number.
; Write a function which accepts an integer n, and returns true iff n is balanced.
;  ex.    false 88099     true  89098

(defn explode-digits [n]
  (map #(Character/digit %1 10) (str n))
)

(defn balanced? [n]
  (let [digits (explode-digits n)  cnt (quot (count digits) 2 )  ]
     (let [x (split-at cnt digits) y (split-at cnt (reverse digits))]
        (= 
           (apply + (first x)) 
           (apply + (first y))
        )
     )
  )
)

;#122 Convert a binary number, provided in the form of a string, to its numerical value.

(defn my-power [n]
    (int (Math/pow 2 (dec n)))
)

(defn bin-convert [s]
  ( let [n (count s)]
      (cond (= 1 n )  (convert-bit (first s))
            :else  (+  (*  (my-power n)  (bin-convert (list(first s))) 
                        )
                       (bin-convert(rest s))
                   )
     )
  )
 )
 
;#128  Recognize Playing Cards
; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs -
;and thirteen cards in each suit. 
;Two is the lowest rank, followed by other integers up to ten; then the jack, queen, king, and ace.

;It's convenient for humans to represent these cards as suit/rank pairs, 
;such as H5 or DQ: the heart five and diamond queen respectively. 

;But these forms are not convenient for programmers, 
;so to write a card game you need some way to parse an input string into meaningful components. 
;For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)
;Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9}.

; A ten will always be represented with the single character "T", rather than the two characters "10".
;    {:suit :diamond :rank 10}    "DQ"
;    {:suit :heart :rank 3}       "H5"
;    {:suit :club :rank 12}       "CA"

(defn recog-card [card]
  (let [suit (first (seq card)) rank (second (seq card)) 
        suit-map { \D "diamond" , \H "heart" , \C "club", \S "spade"}
        rank-map { \2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6 , \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12 }
       ]
     { :suit (suit-map suit) :rank (rank-map rank)}
  )
  
)
  
;#135 infix calculator    left to right, no precedence

(= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))

(defn infix 
  (
   [ op1 operator op2 ]
       (operator op1 op2)
   )
  
   (
     [op1 operator op2 & therest ] 
    
     ( apply infix (cons (operator op1 op2) therest ))
   )
)

; #137 Write a function which returns a sequence of digits of a non-negative number
; (first argument) in numerical system with an arbitrary base (second argument). 
 ; Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10,
 ; [1 1 1 1] in base 2 and [15] in base 16. 

(defn base-conv [number base]
   (let [ q (quot number base)]
     (cond (= 0  q )  (list(rem number base))
        :else (concat(base-conv q base) (list(rem number base)))
      )
    )
 )
 
 
; #144 Write an oscillating iterate: a function that takes an initial value
; and a variable number of functions. 
; It should return a lazy sequence of the functions applied to the value in order, 
; restarting from the first function after it hits the end.
	
;(= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
	
;(= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
	
;(= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
 

;#157  Transform a sequence into a sequence of pairs containing the original elements along with their index.
;  (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])     

 (defn my-index-seq [ xs]
   (map  #(  vector %1 (.indexOf xs %1)) xs )
 )

;#177  Balancing Brackets
 
;When parsing a snippet of code it's often a good idea to do a sanity check 
;to see if all the brackets match up. Write a function that takes in a string 
;and returns truthy if all square [ ] round ( ) and curly { } brackets are properly 
;paired and legally nested, or returns falsey otherwise.

(defn open-bracket? [c]
	(let [  open-brackets #{  \[  \{   \( }  ]
	   (contains? open-brackets c)
	)
)
(defn closed-bracket? [c]
	(let [  open-brackets #{  \]  \}   \) }  ]
	   (contains? open-brackets c)
	)
)

(defn partner [c]
	(cond  (= c \] ) \[
	       (= c \) ) \(
	       (= c \} ) \{
	)
)

(defn balanced [s stack]
	( cond   (empty? s) 
			     (cond
			     	(empty? stack) true
			     	:else false
			     )
			  (open-bracket?  (first s))
			       (balanced (rest s)    (conj stack (first s)) )
			  (closed-bracket? (first s))
			  
			     (if  ( not=     (peek stack)    (partner (first s))    ) false 
			           (balanced (rest s) (pop stack))
			     )
			  
			  :else (balanced (rest s) stack)    
	)
)

(defn balance-brackets [s]
	(balanced s [])
)


