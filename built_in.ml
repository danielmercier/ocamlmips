open Ast

let built_in = [
	("+",(Tarrow(Tint,(Tarrow(Tint,Tint)))));
	("fst",(Tarrow((Tproduct([(Tvar {id = "b1"; def = None} );(Tvar {id = "b2"; def = None} )])),(Tvar {id = "b1"; def = None} ))));
	("snd",(Tarrow((Tproduct([(Tvar {id = "b3"; def = None} );(Tvar {id = "b4"; def = None} )])),(Tvar {id = "b4"; def = None} ))));
]
