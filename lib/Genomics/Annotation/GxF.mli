(** GFF/GXF annotations *)

type feature = {
	seqid : string;
	lo : int;
	hi : int;
	source : string;
	ty : string;
	score : float option;
	strand : [`plus | `minus | `none | `unknown];
	phase : int option;
	id : string;
	attributes : (string*string) list; (** key,value *)
	
	unparsed : string option (** original tab-delimited text of this feature (can be None if the
	                             feature was constructed programmatically) *)
}

(** parse a single feature from one line of the GxF file
@param choose_id given the attributes, returns what should be placed in the [id] field
*)
val parse_feature : ?choose_id:((string*string) list -> string) -> string -> feature

(** the default [choose_id] tries to make some reasonable choice (currently by trying to find the
attribute named "ID", then "Parent", then "transcript_id", before raising [Not_found]).*)
val choose_id_default : (string*string) list -> string

(** serialize a feature as one GxF line.

@param format_attributes serializes the attributes (see below) *)
val feature_to_string : ?format_attributes:(feature -> string) -> feature -> string

(** returns the feature ID only, as in old versions of GFF. any other attributes are ignored *)
val format_attributes_idonly : feature -> string

(** formats the attributes as [key1 "value1"; key2 "value2"; ...], as in GTF. Does not automatically
escape attribute values. *)
val format_attributes_gtf : feature -> string

(** formats the attributes as [key=value1;key2=value2;...], as in GFF3. Does not automatically
URL-encode attribute values. *)
val format_attributes_gff : feature -> string

(** default behavior: if [feature.attributes] is the empty list, returns the feature ID only, as in
old versions of GFF. Otherwise uses the newer GFF style.*)
val format_attributes_default : feature -> string

(** storage of GxF annotations in an SQLite database *)
module DB : sig
	type db
	
	val db_open : ?lockless:bool -> string -> db
	val db_close : db -> unit
	
	val with_db : ?lockless:bool -> string -> (db -> 'a) -> 'a
	
	val fold : ?sort:bool -> (feature -> 'a -> 'a) -> db -> 'a -> 'a
	val ids : db -> string list
	val seqids : db -> string list
	val seqid_ids : db -> string -> string list
	
	val query_id : ?limit:int -> db -> string -> feature list
	val query_alias : ?limit:int -> db -> string -> feature list
	val query_seqid : ?limit:int -> db -> string -> feature list
	val query_locus : ?limit:int -> db -> string -> int -> int -> feature list
	
	val insert : ?aliases:(string list) -> db -> feature -> unit
	val analyze : db -> unit

	val transact : db -> (db -> 'a) -> 'a
