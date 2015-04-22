Nonterminals plist entry.
Terminals integer string ';' '-'.
Rootsymbol plist.
plist -> entry plist : ['$1' | '$2'].
plist -> entry : ['$1'].
entry -> integer ';' string : ['$1'].
entry -> integer '-' integer ';' string : ['$1', '$3'].
