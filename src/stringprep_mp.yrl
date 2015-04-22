Nonterminals mappings mapping integers.
Terminals integer string ';'.
Rootsymbol mappings.
mappings -> mapping mappings : ['$1' | '$2'].
mappings -> mapping : ['$1'].
mapping -> integer ';' integers ';' string : ['$1', '$3'].
mapping -> integer ';' ';' string : ['$1'].
integers -> integer integers : ['$1' | '$2'].
integers -> integer : ['$1'].
