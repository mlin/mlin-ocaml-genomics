type sequence = {
	src : string;
	start : int;
	size : int;
	strand : [`plus | `minus];
	src_size : int;
	text : string;
}

type block = {
	attributes : (string*string) list;
	sequences : sequence array;
	
	unparsed : string;
}

let block_sources { sequences = sequences } = Array.map (fun { src = src } -> src) sequences
let block { sequences = sequences } = Array.map (fun { text = text } -> text) sequences

