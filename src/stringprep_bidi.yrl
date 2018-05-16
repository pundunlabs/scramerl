Nonterminals plist entry.
Terminals integer '-'.
Rootsymbol plist.
plist -> entry plist : ['$1' | '$2'].
plist -> entry : ['$1'].
entry -> integer : ['$1'].
entry -> integer '-' integer : ['$1', '$3'].
