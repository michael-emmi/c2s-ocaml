Done
----
* Write an preliminary parser & printer for Boolean programs.
* Check out the Bebop examples to get the parsing and printing right.
   - Run the output through moped or getafix to check that everything is OK.
* Expand the syntax with concurrency constructs: post, yield.
* Write a first concurrent to sequential translation.
* Introduce (bounded) integers for the LR-vectorization.
* Write BFS/FIFO translation for posts.
* Set up regression testing, so we can have an idea that the code is stable.
  - test the parsing, printing, and re-parsing, both within the same language
    and between translations; check equivalence of original and output
    program.
  - test front-to-back verification results with Boogie, and perhaps other
	back-ends too.
* Use (Boogie) functions to enhance readability of the translations.

Still ToDo
----------

1. FiFo sequentialization should separate processes?

1. Write *unbounded* rounds & depth translations.  (Preliminary tests on a
   simple program are able to report bugs with a *symbolic* bound K.)
   - should probably insert the constant K into the program.
   - bound quantified variables on the vectors between 0 and K.

1. Notice that procedures which don't access global variables need not be
   transformed... Right now this is only taken advantage of when there are 
   no global variables. 

1. Compare FIFO-seq with an encoding which adds bounded queues.

1. Asynchrony example illustrating both unbounded queues and important order.

   - initiating many (one-shot) file reads to a single file system
     -- K reads and 2 parties => at least K messages queued, between the read
	 and read-reply messages 

   - initiating a very-large file read
     -- read-reply messages come back *in order*

   - mix both?

   - the example can be a Node.js web server.

   - but we need a compelling reason these reads are important for the program
     behavior... these call-backs are not obviously crucial *control-flow* of
     an algorithm.. 

   * Asynchronous quicksort revisited *
     - cannot pass the whole list each time.
	 - must pass segments of the list.. 
	 - and the segments must be ordered (in the reply-direction)
	 - does this make any sense?
