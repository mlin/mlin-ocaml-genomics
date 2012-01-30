(** Mike Lin's Genomics library

Note: there were some lousy hacks getting this documentation generated. There is no useful
[GenomicsDocs] module; this is an artifical construction listing the contents of the top-level
[Genomics] module. Furthermore, when you click on a sub-module below, the documentation lists some
more modules as "included" when in fact they are sub-sub-modules. For example, on the page for
[Alignment] you will find [include Block]. This actually leads to the documentation for the module
[Genomics.Alignment.Block].

Sorry about this -- ocamldoc is not able to deal with hierarchically packed modules properly.

*)

module IntervalOps = IntervalOps
module Position = Position

module Sequence = struct
	(**/**)
	module DB = Sequence.DB
	(**/**)
	
	include DB
	
module Annotation = struct
	(**/**)
	module GxF = Annotation.GxF
	(**/**)
	
	include GxF
	
module Alignment = struct
	(**/**)
	module Block = Alignment.Block
	module MAF = Alignment.MAF
	module WGA = Alignment.WGA
	(**/**)

	include Block
	include MAF
	include WGA
