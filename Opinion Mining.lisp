;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Pour que ce code fonctionne correctement on doit placer les fichiers texte des commentaires et les fichiers SWN.txt et stopwords.txt dans le disque local D:\
;;;-----------------------------------------------------------------------------------------------------------------------------------------------------------



;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Definition du necessaire pour l'utilisation de CAPI
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
(defpackage "MY-PACKAGE"
	(:add-use-defaults t)
	(:use "CAPI")
)
(in-package "MY-PACKAGE")
;;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Definition des variables compteur de nombres de fois qu'on a executé les fonction pré-traitement, post-tagging1 et post-tagging2
;;;Les noms des fichiers qu'on crée sont concatenés à ces variables pour ne pas avoir à supprimer à chaque fois les fichiers créés pour faire un traitement sur un autre produit
;;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(setq compteur-pre-traitement 0)
(setq compteur-post-tagging1 0)
(setq compteur-post-tagging2 0)

;;;------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Definition des variables compteur des nombres de commentaires positives, négatives et neutres
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
(setq neg_line 0)
(setq pos_line 0)
(setq neu_line 0)

;;;-----------------------------------------------------------------------------------------------------------------------------------------------------
;;;Fonction effectuant le pré-traitement
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------

(defun FonctionPreTraitement (input_txt)
(setq output_pre_txt (format nil "D:\resultpretreatment~A.txt" compteur-pre-traitement))
(setq stopwords_txt "D:\StopWords.txt")
(defun delimiterp (c) (position c " ,.;/!:?*~"))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(setq stopwords (make-sequence '(vector string) 
   0
   ))


(let ((in (open stopwords_txt :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
       
       while line do (setq stopwords (concatenate 'CONS stopwords (my-split line)))

       
(terpri)  


)

      (close in)
   )
)


(let ((in (open input_txt :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)

       while line do (setq lineseq (my-split line))
       
       while line do (loop for sword in stopwords
                              do (loop for word in lineseq

                                  do(if (string-equal sword word) (setq lineseq (remove word lineseq)) )

                                 )
                                 
                     
                      )
       (defvar resultline "")
      (loop for word in lineseq
      do(setq resultline (concatenate 'string resultline (concatenate 'string word " ")))
       )
      while line do (with-open-file (stream output_pre_txt :direction :output :if-exists :append :if-does-not-exist :CREATE )
   (format stream resultline)
   (terpri stream)
  
)

      
(setq resultline "")


)

      (close in)
   )
)
(setq compteur-pre-traitement (+ compteur-pre-traitement 1)))

;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Fonction effectuant le post-tagging (traitement final qui vient après le tagging qu'on effectue à travers le code java
;;;Version 2 : cette version distingue entre 2 types de commentaires : positifs et négatifs
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
(defun FonctionPostTagging2 ()
(setq input_postagged (format nil "D:\resultpostagged~A.txt" compteur-post-tagging2))
(setq output_adjectives (format nil "D:\resultfinalver2adj~A.txt" compteur-post-tagging2))
(setq SWN_txt "D:\SWN.txt")

(defun delimiterp1 (c) (position c "	"))

(defun my-split1 (string &key (delimiterp1 #'delimiterp1))
  (loop :for beg = (position-if-not delimiterp1 string)
    :then (position-if-not delimiterp1 string :start (1+ end))
    :for end = (and beg (position-if delimiterp1 string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp2 (c) (position c " "))

(defun my-split2 (string &key (delimiterp2 #'delimiterp2))
  (loop :for beg = (position-if-not delimiterp2 string)
    :then (position-if-not delimiterp2 string :start (1+ end))
    :for end = (and beg (position-if delimiterp2 string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp3 (c) (position c "#"))

(defun my-split3 (string &key (delimiterp3 #'delimiterp3))
  (loop :for beg = (position-if-not delimiterp3 string)
    :then (position-if-not delimiterp3 string :start (1+ end))
    :for end = (and beg (position-if delimiterp3 string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))






 
(setq POS_score (make-hash-table :test 'equal ))
(setq NEG_score (make-hash-table :test 'equal ))
(setq norm (make-hash-table :test 'equal )) 

(let ((in (open SWN_txt :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
       while line do  (setq lineseq (my-split1 line))
       while line do  (setq words (my-split2 (elt lineseq 4)))
       while line do (loop for word in words

                     while word do(setf (gethash (elt (my-split3 word) 0) POS_score) (+ (* (/ 1 (with-input-from-string (in (elt (my-split3 word) 1)) (read in))) (with-input-from-string (in (elt lineseq 2)) (read in))) (if (gethash (elt (my-split3 word) 0) POS_score) (gethash (elt (my-split3 word) 0) POS_score) 0)))
					 
					 while word do(setf (gethash (elt (my-split3 word) 0) NEG_score) (+ (* (/ 1 (with-input-from-string (in (elt (my-split3 word) 1)) (read in))) (with-input-from-string (in (elt lineseq 3)) (read in))) (if (gethash (elt (my-split3 word) 0) NEG_score) (gethash (elt (my-split3 word) 0) NEG_score) 0)))
					 while word do(setf (gethash (elt (my-split3 word) 0) norm) (+ (/ 1 (with-input-from-string (in (elt (my-split3 word) 1)) (read in))) (if (gethash (elt (my-split3 word) 0) norm) (gethash (elt (my-split3 word) 0) norm) 0))
                      
						)
  
                        )
  
)

      (close in)
   )
)
(loop for key being the hash-keys of POS_score
        while key do (setf (gethash key POS_score) (/ (gethash key POS_score) (gethash key norm) ))
		while key do (setf (gethash key NEG_score) (/ (gethash key NEG_score) (gethash key norm) ))
		
)

(defun delimiterp (c) (position c " ,.;/!:?*~"))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
	
(setq adjectives (make-sequence '(vector string) 
   0
   ))
(defvar resultline "")


(setq c 0)

(let ((in (open input_postagged :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
       while line do (setq lineseq (my-split line))
       while line do (setq neg_words 0)
	   while line do (setq pos_words 0)
	   while line do (setq notword nil)
       while line do (loop for word in lineseq 
	                   
                       while word do (setq neg 0)
					   while word do (setq pos 0)
					   
                       while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (setq resultline (concatenate 'string resultline (concatenate 'string (subseq word 0 (- (length word) 3) ) " "))))
                       while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (setq pos (gethash (string-downcase (subseq word 0 (- (length word) 3) )) POS_score))))
                       while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (setq neg (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if notword (setq c neg))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if notword (setq neg pos))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if notword (setq pos c))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if (> pos neg) (setq pos_words (+ 1 pos_words)))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if (> neg pos) (setq neg_words (+ 1 neg_words)))))
					   while word do(if (string-equal word "not_RB") (setq notword t) (setq notword nil))
					   
                        )

	    (terpri)


	   (terpri)
	   (terpri)
	   
       while line do (if (> pos_words neg_words) (setq pos_line (+ 1 pos_line)))
	   while line do (if (< pos_words neg_words) (setq neg_line (+ 1 neg_line)))
       while line do (with-open-file (stream output_adjectives :direction :output :if-exists :append :if-does-not-exist :CREATE )
   (format stream resultline)
   (terpri stream)
  
)
     while line do(setq resultline "")
 

)
(format t "positive : ~A~%" pos_line)
(format t "negative : ~A~%" neg_line)
      (close in)
   )
)
(setq compteur-post-tagging2 (+ compteur-post-tagging2 1)))

;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Fonction effectuant le post-tagging (traitement final qui vient après le tagging qu'on effectue à travers le code java
;;;Version 1 : cette version distingue entre 3 types de commentaires : positifs, négatifs et neutres
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------

(defun FonctionPostTagging1 ()
(setq input_postagged (format nil "D:\resultpostagged~A.txt" compteur-post-tagging1))
(setq output_adjectives (format nil "D:\resultfinalver1adj~A.txt" compteur-post-tagging1))
(setq SWN_txt "D:\SWN.txt")


(defun delimiterp1 (c) (position c "	"))

(defun my-split1 (string &key (delimiterp1 #'delimiterp1))
  (loop :for beg = (position-if-not delimiterp1 string)
    :then (position-if-not delimiterp1 string :start (1+ end))
    :for end = (and beg (position-if delimiterp1 string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp2 (c) (position c " "))

(defun my-split2 (string &key (delimiterp2 #'delimiterp2))
  (loop :for beg = (position-if-not delimiterp2 string)
    :then (position-if-not delimiterp2 string :start (1+ end))
    :for end = (and beg (position-if delimiterp2 string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp3 (c) (position c "#"))

(defun my-split3 (string &key (delimiterp3 #'delimiterp3))
  (loop :for beg = (position-if-not delimiterp3 string)
    :then (position-if-not delimiterp3 string :start (1+ end))
    :for end = (and beg (position-if delimiterp3 string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))






(setq POS_score (make-hash-table :test 'equal ))
(setq NEG_score (make-hash-table :test 'equal ))
(setq norm (make-hash-table :test 'equal )) 

(let ((in (open SWN_txt :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
       while line do  (setq lineseq (my-split1 line))
       while line do  (setq words (my-split2 (elt lineseq 4)))
       while line do (loop for word in words

                     while word do(setf (gethash (elt (my-split3 word) 0) POS_score) (+ (* (/ 1 (with-input-from-string (in (elt (my-split3 word) 1)) (read in))) (with-input-from-string (in (elt lineseq 2)) (read in))) (if (gethash (elt (my-split3 word) 0) POS_score) (gethash (elt (my-split3 word) 0) POS_score) 0)))
					 while word do(setf (gethash (elt (my-split3 word) 0) NEG_score) (+ (* (/ 1 (with-input-from-string (in (elt (my-split3 word) 1)) (read in))) (with-input-from-string (in (elt lineseq 3)) (read in))) (if (gethash (elt (my-split3 word) 0) NEG_score) (gethash (elt (my-split3 word) 0) NEG_score) 0)))
					 while word do(setf (gethash (elt (my-split3 word) 0) norm) (+ (/ 1 (with-input-from-string (in (elt (my-split3 word) 1)) (read in))) (if (gethash (elt (my-split3 word) 0) norm) (gethash (elt (my-split3 word) 0) norm) 0))
                        )
  
                        )
  
)

      (close in)
   )
)
(loop for key being the hash-keys of POS_score
        while key do (setf (gethash key POS_score) (/ (gethash key POS_score) (gethash key norm) ))
		while key do (setf (gethash key NEG_score) (/ (gethash key NEG_score) (gethash key norm) ))
		
)

(defun delimiterp (c) (position c " ,.;/!:?*~"))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
	
(setq adjectives (make-sequence '(vector string) 
   0
   ))
(defvar resultline "")


(setq c 0)
(let ((in (open input_postagged :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
       while line do (setq lineseq (my-split line))
       while line do (setq neg_words 0)
	   while line do (setq pos_words 0)
	   while line do (setq neu_words 0)
	   while line do (setq notword nil)
       while line do (loop for word in lineseq 
	                   
                       while word do (setq neg 0)
					   while word do (setq pos 0)
					   
                       while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (setq resultline (concatenate 'string resultline (concatenate 'string (subseq word 0 (- (length word) 3) ) " "))))
                       while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (setq pos (gethash (string-downcase (subseq word 0 (- (length word) 3) )) POS_score))))
                       while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (setq neg (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if notword (setq c neg))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if notword (setq neg pos))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if notword (setq pos c))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if (> pos neg) (if(> pos (- 1 (+ pos neg))) (setq pos_words (+ 1 pos_words))))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if (> neg pos) (if(> neg (- 1 (+ pos neg))) (setq neg_words (+ 1 neg_words))))))
					   while word do(if (string-equal (subseq word (- (length word) 2)) "JJ") (if (gethash (string-downcase (subseq word 0 (- (length word) 3) )) NEG_score) (if (> (- 1 (+ pos neg) neg)) (if(> (- 1 (+ pos neg)) pos) (setq neu_words (+ 1 neu_words))))))
					   while word do(if (string-equal word "not_RB") (setq notword t) (setq notword nil))
                        )

	   (terpri)
	   (terpri)
	   
       while line do (if (> pos_words neg_words) (if (> pos_words neu_words) (setq pos_line (+ 1 pos_line))))
	   while line do (if (> neu_words neg_words) (if (> neu_words pos_words) (setq neu_line (+ 1 neu_line))))
	   while line do (if (> neg_words pos_words) (if (> neg_words neu_words) (setq neg_line (+ 1 neg_line))))
       while line do (with-open-file (stream output_adjectives :direction :output :if-exists :append :if-does-not-exist :CREATE )
   (format stream resultline)
   (terpri stream)
  
)
     while line do(setq resultline "")
 

)
(format t "positive : ~A~%" pos_line)
(format t "negative : ~A~%" neg_line)
(format t "neutre : ~A~%" neu_line)

      (close in)
   )
)
(setq compteur-post-tagging1 (+ compteur-post-tagging1 1)))

;;;--------------------------------------------------------------------------------------------------------------------------------------------------------						
;;;Definition de l'interface du Pré-traitement
;;;--------------------------------------------------------------------------------------------------------------------------------------------------------						
(capi:define-interface InterfacePreTraitement ()
  ()
  (:panes
      (SelectFile capi:push-button
               :text "Parcourir"
			   :visible-min-width 149
               :selection-callback #'(lambda (&rest args) 
                        (FonctionPreTraitement (prompt-for-file "Entrer un fichier") )))
      (EnterPath capi:push-button
                 :text "Entrer le chemin du fichier"
				 :visible-min-width 149
                 :selection-callback  #'(lambda (&rest args) 
                        (FonctionPreTraitement (prompt-for-string "Entrer le chemin :") )))
      (acer capi:push-button
                 :text "Acer"
				 :visible-min-width 73
                 :selection-callback #'(lambda (&rest args) 
                        (FonctionPreTraitement "D:\Acer C720P Chromebook (11.6-Inch Touchscreen, 2GB) Moonstone White.txt" )))
	  (nokia capi:push-button
				 :text "Nokia"
				 :visible-min-width 73
				 :selection-callback #'(lambda (&rest args) 
                        (FonctionPreTraitement "D:\Nokia Lumia 920 Black Factory Unlocked 32GB phone 4G LTE.txt" )))
	  (samsung capi:push-button
				 :text "Samsung"
				 :visible-min-width 73
				 :selection-callback #'(lambda (&rest args) 
                        (FonctionPreTraitement "D:\Samsung Galaxy Note II, Titanium Gray 16GB.txt" )))
	  (asus capi:push-button
				 :text "ASUS"
				 :visible-min-width 73
				 :selection-callback #'(lambda (&rest args) 
                        (FonctionPreTraitement "D:\ASUS Transformer Book T100TA-C1-GR 10.1 Detachable 2-in-1 Touchscreen Laptop, 64GB.txt" )))
	  (Redirect capi:push-button
				 :text "Passez au traitement final"
				 :selection-callback #'(lambda (&rest args) 
                        (capi:display (make-instance 'InterfacePostTagging))))
	  (viewer capi:editor-pane
              :text "Si vous avez effectué le tagging à travers le programme java passez à l'étape Traitement Post Tagging en cliquant sur le bouton -Passez au traitement final-"
              :visible-min-height '(:character 8)
              :reader viewer-pane)
	  )
   (:layouts
     (main-layout capi:column-layout 
                  '(main-layout1 main-layout2))
     (main-layout2 capi:column-layout
                     '(viewer Redirect)
				  :title "Redirection vers le traitement final")
     
     (main-layout1 capi:column-layout 
                  '(row-of-buttons1 row-of-buttons2))
     (row-of-buttons1 capi:row-layout
                     '(SelectFile EnterPath)
				  :title "Choisir un fichier")
     (row-of-buttons2 capi:row-layout
                     '(acer nokia samsung asus)
					 :title "Les 4 produits donnés")
	 )
   (:default-initargs :title "Evaluation de commentaires - Pré-traitement"))
   
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Definition de l'output-pane où on affiche le diagramme de traitement final (version 1)
;;;--------------------------------------------------------------------------------------------------------------------------------------------------------						
   (defun DrawChart1 ()
   

 
 (setq output-pane
    (contain
         (make-instance 'output-pane)
         :best-width 300
         :best-height 300))
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 30 30 30 250)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 30 250 250 250)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 70 265 80 265)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 75 260 75 270)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 130 265 140 265)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 192 260 192 270)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 197 260 197 270)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 192 260 197 270)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-rectangle output-pane 60 (- 250 pos_line) 30 pos_line
 :foreground :red)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-rectangle output-pane 120 (- 250 neg_line) 30 neg_line
 :foreground :blue)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-rectangle output-pane 180 (- 250 neu_line) 30 neu_line
 :foreground :green)
 
 
		 )
		 
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Definition de l'output-pane où on affiche le diagramme de traitement final (version 2)
;;;--------------------------------------------------------------------------------------------------------------------------------------------------------			 
		 (defun DrawChart2 ()
   

 
 (setq output-pane
    (contain
         (make-instance 'output-pane)
         :best-width 300
         :best-height 300))
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 30 30 30 250)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 30 250 250 250)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 70 265 80 265)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 75 260 75 270)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-line output-pane 130 265 140 265)
  (capi:apply-in-pane-process 
 output-pane 'gp:draw-rectangle output-pane 60 (- 250 pos_line) 30 pos_line
 :foreground :red)
 (capi:apply-in-pane-process 
 output-pane 'gp:draw-rectangle output-pane 120 (- 250 neg_line) 30 neg_line
 :foreground :blue)
 
 
		 )
	
;;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;;Definition de l'interface du traitement final qui se fait après le tagging
;;;--------------------------------------------------------------------------------------------------------------------------------------------------------	   
 (capi:define-interface InterfacePostTagging ()
  ()
  (:panes
      
	  (ver1 capi:push-button
				 :text "Trier les commentaires selon la version 1"
				 :selection-callback #'(lambda (&rest args) 
                        (FonctionPostTagging1)
						
						(DrawChart1)
						(capi:apply-in-pane-process
								viewer #'(setf capi:editor-pane-text) (format nil "- Nombre des commentaires positives : ~A~%- Nombre des commentaires négatives : ~A~%- Nombre des commentaires neutres : ~A~%" pos_line neg_line neu_line) viewer)
						(setq neg_line 0)
						(setq pos_line 0)
						(setq neu_line 0)))
	  (ver2 capi:push-button
				 :text "Trier les commentaires selon la version 2"
				 :selection-callback #'(lambda (&rest args) 
                        (FonctionPostTagging2)
						(capi:apply-in-pane-process
								viewer #'(setf capi:editor-pane-text) (format nil "- Nombre des commentaires positives : ~A~%- Nombre des commentaires négatives : ~A~%" pos_line neg_line) viewer)
						(DrawChart2)
						(setq neg_line 0)
						(setq pos_line 0)
						(setq neu_line 0)))
	  (ver1Text capi:display-pane
				 :text "Cette version distingue entre trois types de commentaires : Positif, Négatif et Neutre"
				 )
				 
	  (ver2Text capi:display-pane
				 :text "Cette version distingue entre deux types de commentaires : Positif et Négatif"
				 )
				 (viewer capi:editor-pane
              :title "Résultat:"
              :text "Aucun traitement n'a été effectué."
              :visible-min-height '(:character 8)
              :reader viewer-pane))
   (:layouts
     (main-layout capi:column-layout 
                  '(ver1-layout ver2-layout row-with-editor-pane))
     (ver1-layout capi:column-layout
                     '(ver1Text ver1)
				  :title "Version 1")
     (ver2-layout capi:column-layout
                     '(ver2Text ver2)
					 :title "Version 2")
					 (row-with-editor-pane capi:row-layout
                           '(viewer)))
   (:default-initargs :title "Evaluation de commentaires - Post-tagging"))
   
;;;-------------------------------------------------------------------------------------------------------------------------------------------------  
;;;Appel de la première interface d'accueil (de pré-traitement)
;;;------------------------------------------------------------------------------------------------------------------------------------------------- 
(capi:display (make-instance 'InterfacePreTraitement))


   
   
   
   
   
 