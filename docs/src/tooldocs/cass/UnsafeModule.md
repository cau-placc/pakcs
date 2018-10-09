Analyzing module for importing `Unsafe` module
----------------------------------------------

The `UnsafeModule` analysis returns information whether a module is unsafe,
i.e., it imports directly or indirectly the module `Unsafe`.
Such modules might hide dangerous operations in
purely functional operations.

The result of this analysis is the list of the names of all modules
which directly imports the module `Unsafe`.
Thus, a module is safe if the analysis result is the empty list.
