# locations
(setq *Nodes
      '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
        (garden (you are in a beautiful garden. there is a well in front of you.))
        (attic (you are in the attic. there is a giant welding torch in the corner.))))

# get description of a location
(de describe-location (Location Nodes)
  (cadr (assoc Location Nodes)))

# connections between locations
(setq *Edges
      '((living-room (garden west door) (attic upstairs ladder))
        (garden (living-room east door))
        (attic (living-room downstairs ladder))))

# get description of connection
(de describe-path (Edge)
  (let ((_ @Direction @Passage) Edge)
    (fill '(there is a @Passage going @Direction from here.))))

# get description of all connections
(de describe-paths (Location Edges)
  (apply 'append (mapcar 'describe-path (cdr (assoc Location Edges)))))

# objects
(setq *Objects '(whiskey bucket frog chain))

# location of objects
(setq *ObjectLocations
      '((whiskey living-room)
        (bucket living-room)
        (chain garden)
        (frog garden)))

# get objects at location
(de objects-at (Loc Objs ObjLoc)
  (let (is-at '((Obj) (= (cadr (assoc Obj ObjLoc)) Loc)))
    (filter 'is-at Objs)))

# describe objects
(de describe-objects (Loc Objs ObjLoc)
  (let (describe-obj '((@Obj) (fill '(you see a @Obj on the floor.))))
     (apply 'append (mapcar 'describe-obj (objects-at Loc Objs ObjLoc)))))

# initial location
(setq *Location 'living-room)

# 'look' action
(de look ()
  (append (describe-location *Location *Nodes)
          (describe-paths *Location *Edges)
          (describe-objects *Location *Objects *ObjectLocations)))

# 'walk' action
(de walk (Direction)
  (let (correct-way '((Edge) (= (cadr Edge) Direction))
        Next (find 'correct-way (cdr (assoc *Location *Edges))))
    (if Next
      (prog
        (setq *Location (car Next))
        (look))
      '(you cannot go that way.))))

# 'pickup' action
(de pickup (Obj)
  (cond
    ((member Obj (objects-at *Location *Objects *ObjectLocations))
       (push '*ObjectLocations (list Obj 'body))
       (fill '(you are now carrying the Obj) 'Obj))
    (T '(you cannot get that.))))

# 'inventory' action
(de inventory ()
  (cons 'items- (objects-at 'body *Objects *ObjectLocations)))

# check if user has object
(de have (Obj)
  (member Obj (cdr (inventory))))

# game-read function for the custom game-repl
(de game-read ()
  (let (Command (in NIL (str (pack (line))))
        quote-it '((X) (cons 'quote X)))
    (cons (car Command) (mapcar 'quote-it (cdr Command)))))

# allowed commands
(setq *AllowedCommands '(look walk pickup inventory))

# game-eval function for the custom game-repl
(de game-eval (Sexp)
  (if (member (car Sexp) *AllowedCommands)
    (eval Sexp)
    '(i do not know that command.)))

# text tweaking function
(de tweak-text (List Caps? Lit?)
  (when List
    (let ((Item . Rest) List)
      (cond
        ((= Item " ") (cons Item (tweak-text Rest Caps? Lit?)))
        ((member Item '("!" "?" ".")) (cons Item (tweak-text Rest T Lit?)))
        ((= Item "\"") (tweak-text Rest Caps? (not Lit?)))
        (Lit? (cons Item (tweak-text Rest NIL Lit?)))
        (Caps? (cons (uppc Item) (tweak-text Rest NIL Lit?)))
        (T (cons (lowc Item) (tweak-text Rest NIL NIL)))))))

# game-print function for the custom game-repl
(de game-print (List)
  (prinl (pack (tweak-text (chop (str List)) T NIL))))

# custom game-repl
(de game-repl ()
  (let (Command (game-read))
    (unless (= (car Command) 'quit)
      (game-print (game-eval Command))
      (game-repl))))

# generic game action
# (since picolisp has no macros, this becomes a normal function)
(de game-action (@Cmd @Subj @Obj @Place . @Body)
  (set @Cmd (fill '((Subject Object)
                    (if (and (= *Location '@Place)
                        (= Subject '@Subj)
                        (= Object '@Obj)
                        (have '@Subj))
                      (prog ~'@Body)
                      '(i cant @Cmd like that.)))))
  (push1 '*AllowedCommands @Cmd))

# define 'weld' action
(setq *ChainWelded NIL)
(game-action 'weld 'chain 'bucket 'attic
  (if (and (have 'bucket)
           (not *ChainWelded))
    (prog (setq *ChainWelded T)
      '(the chain is now securely welded to the bucket.))
    '(you do not have a bucket.)))

# define 'dunk' action
(setq *BucketFilled NIL)
(game-action 'dunk 'bucket 'well 'garden
  (if *ChainWelded
    (prog (setq *BucketFilled 'T)
      '(the bucket is now full of water.))
    '(the water level is too low to reach.)))

# define 'splash' action
(game-action 'splash 'bucket 'wizard 'living-room
  (cond
    ((not *BucketFilled) '(the bucket has nothing in it.))
    ((have 'frog) '(the wizard awakens and sees that you stole his frog. he is so upset he banishes you to the netherworlds- you lose! the end.))
    (T '(the wizard awakens from his slumber and greets you warmly. he hands you the magic low-carb donut- you win! the end.))))

# start
(game-repl)
