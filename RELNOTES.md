PAKCS: Release Notes
====================

Release notes for PAKCS Version 3.8.0 (January 22, 2025)
--------------------------------------------------------

Changes to version 3.7.2:

  * REPL command `:info` added to get more detailed information
    about operations, types, or classes. This command requires
    the installation of the Curry package `cpm-query`.
  * Front-end updated to support multi-parameter type classes.
    For this purpose, the front-end supports the language extensions
    `MultiParamTypeClasses`, `FunctionalDependencies` and
    `FlexibleInstances` (similarly to Haskell).
    An example can be found in `testsuite/TypeclassTests/TestMPTCCoerce.curry`.
  * As a consequence of supporting multi-parameter type classes,
    the structure of Curry interface files (ending with `.icurry`)
    and AbstractCurry files (ending with `.acy`) has been slightly changed:
    type class constraints have now a list of type parameters
    instead of a single one. This can be seen in the new versions
    of the Curry packages `abstract-curry` (version 4.x) and
    `curry-interface` (version 4.x).
    Moreover, the names of internal operations generated for
    operations defined in type classes (e.g., instance operations)
    have been slightly changed (this is only visible in FlatCurry files).
  * `Prelude`: value generator for floats added


Release notes for PAKCS Version 3.7.2 (September 20, 2024)
-----------------------------------------------------------

Changes to version 3.7.0:

  * Command `:interface` use the new interface pretty printer
    based on the Curry package `curry-interface` so that also
    information about type classes are shown
  * Update front end:
    - new option `--origin-pragmas` (only for use in the Curry Language Server)
    - incomplete cases are always extended with explicit calls to
      `Prelude.failed` in missing branches. For instance,

          head (x:_) = x

      is translated into the FlatCurry definition

          head xs = case xs of x:_ -> x
                               []  -> Prelude.failed


Release notes for PAKCS Version 3.7.0 (April 14, 2024)
------------------------------------------------------

Changes to version 3.6.0:

  * Changes in case mode: the case modes Haskell, Prolog, and Gödel
    are stronger so that they emit error messages instead of warnings,
    the default case mode is Curry, which is like Haskell but emit
    only warnings (see Section 3.7 of the PAKCS User Manual).
  * Front end does not include `Prelude` in imports of FlatCurry files
    when it is not necessary (e.g., if the language option `NoImplicitPrelude`
    is set)
  * Small bug fixes
  * CPM: add `--dependencies` option to clean command in order to
    clean all dependencies in the current package (useful to clean
    the standard homepackage)


Release notes for PAKCS Version 3.6.0 (November 10, 2023)
---------------------------------------------------------

Changes to version 3.5.2:

  * Base libraries extended by including libraries for encapsulated search
    so that set functions can be used without installing packages.
    The new libraries are:
    - `Control.Search.SetFunctions`(implementing set functions)
    - `Control.Search.AllValues` (implementing a strong  encapsulation
      as I/O operations)
    - `Control.Search.Unsafe`
      (implementing strong encapsulation as non I/O operations, but this
      method has a non-declarative behavior)
    - `Control.Search.SearchTree` (implementing search trees which
      are mainly used in KiCS2 to implement  encapsulation).
  * Update CPM (modified options for command `upload`)


Release notes for PAKCS Version 3.5.2 (August 18, 2023)
-------------------------------------------------------

Changes to version 3.5.1:

  * Verbosity mode slightly changed: parser messages shown only for verbosity
    greater than 1.
  * Fix command `:add ...` by compiling new imports.
  * Add let bindings to the interactive REPL.
  * Update CPM (with automatic upload to Masala/CPM)


Release notes for PAKCS Version 3.5.1 (March 13, 2023)
------------------------------------------------------

Changes to version 3.5.0:

  * Update front-end where option `--extended` is the default.
    In order to switch off the extensions for functional patterns
    and anonymous free variables, one can use the options
    `NoFunctionalPatterns` and `NoAnonFreeVars`, respectively.
  * Update front-end to use GHC 9.2
  * Update CPM (with new resource URLs)


Release notes for PAKCS Version 3.5.0 (October 8, 2022)
-------------------------------------------------------

Changes to version 3.4.2:

  * New base modules `Data.Functor.Compose` and `Data.Functor.Const` added.
  * Small fix for debug mode when tracing is off.
  * Front end supports new language options
    `NoImplicitPrelude` and `NoDataDeriving`.
  * New property `tmpdir` added to RC file (`~/.pakcsrc`) in order to
    redefine the location of a temporary directory (e.g., if the
    default location `/tmp` is not writable for the PAKCS process).
  * Properties `verboserc` and `dynamicmessages` removed from RC file
    since they are no longer used.
  * Current RC properties are printed with command `:set` when verbosity level
    is greater than 1.


Release notes for PAKCS Version 3.4.2 (December 20, 2021)
---------------------------------------------------------

Changes to version 3.3.0:

  * Verbosity options refined.
  * Output of command `:type` for expressions with free variables corrected.
  * Top-level expressions with `Monad` context are specialized to `IO`.
  * Top-level expressions with `Data` context are specialized to `Bool`.
  * Top-level expressions with `Floating` context are specialized to `Float`.
  * Desugaring of `newtype` declarations integrated into the compiler.
    Thus, the FlatCurry files produced by the front end contain
    `newtype` declarations which are eliminated before the Prolog code
    is generated.
  * Since type information is not used in the run-time system,
    it is no longer generated in the Prolog files.


Release notes for PAKCS Version 3.3.0 (February 15, 2021)
-------------------------------------------------------

Changes to version 3.2.0:

  * New compilation scheme for type classes to avoid problems
    with 0-ary non-deterministic definitions in instance declarations.
    Dictionaries are now represented as functions in order to
    enforce the evaluation of all instance operations.
  * Prelude: operation `(/==)` added.
  * The prelude operations `(=:=)` and `(=:<=)` changed from external
    to defined operations that call the external operations
    `constrEq` and `nonstrictEq`, respectively. This is meaningful
    to keep the `Data` constraint for `(=:=)` and `(=:<=)` whereas
    external operations have no class contexts.
  * Installation slightly simplified: SICStus-Prolog or SWI-Prolog
    executables are searched in the load path so that the old
    installation cache file `pakcsinitrc` is no longer used.


Release notes for PAKCS Version 3.2.0 (November 24, 2020)
---------------------------------------------------------

Changes to version 2.3.0: bug fixes and

  * Type class `Data` with operations `(===)` (equality) and
    `aValue` (non-deterministic value generator) added to the prelude.
    For each `data` declaration, `Data` instances for the defined type
    are automatically derived as long as the defined type is first-order
    (i.e., does not contain functional types).
    Free variable have type class constraint `Data`.
    The motivation for this design and its advantages are described in a
    [DECLARE/WFLP'19 paper](https://doi.org/10.1007/978-3-030-46714-2_15).
  * The standard libraries has been changed in order to keep the names
    and structure more closely with Haskell. Specialized functionality
    is moved separate packages. There is a separate
    [migration guide](https://git.ps.informatik.uni-kiel.de/curry/curry-libs/-/blob/master/MigrationGuide.md)
    describing the changes.
  * Libraries `FilePath`, `Directory`, `Distribution`, `Time`,
    `IOExts`, `ReadShowTerm` removed
    (now available in packages `filepath`, `directory`, `distribution`,
    `time`, `io-extra` and `read-legacy`).
  * Library `System` split into `System.Process`, `System.CPUTime`,
    `System.Environment`. 
    `System.Process` is available in package `process`.
    The rest remains in the library.
  * Implemented the "MonadFail-Proposal" for Curry
    (see <https://wiki.haskell.org/MonadFail_Proposal>).
  * Intermediate files are written into versioned directories, e.g.,
    the FlatCurry representation of `lib/Prelude.curry` is written
    to `lib/.curry/pakcs-3.2.0/Prelude.fcy` (and similarly all
    other intermediate files). This avoids inconsistencies
    of intermediate files when different Curry systems are used.


Release notes for PAKCS Version 2.3.0 (October 12, 2020)
--------------------------------------------------------

Changes to version 2.2.0:

  * CPM updated (faster `update` command and support for caret
    and tilde comparison operators in dependency descriptions)
  * Interface for external functions simplified (as described
    in Appendix F of the manual).


Release notes for PAKCS Version 2.2.0 (October 30, 2019)
--------------------------------------------------------

Changes to version 2.1.2: bug fixes and

  * CPM updated (improved handling of temporary working directory)


Release notes for PAKCS Version 2.1.2 (September 23, 2019)
----------------------------------------------------------

Changes to version 2.1.0: bug fixes and

  * Compatibility to SWI-Prolog version 8.* improved
  * Prelude: `chr` defined as total operation
  * Pre-compilation of libraries improved
  * CPM updated (e.g., faster `update` operation)


Release notes for PAKCS Version 2.1.0 (January 30, 2019)
--------------------------------------------------------

Changes to version 2.0.2:

  * Libraries `AllSolutions`, `Findall`, `SearchTree`, `SearchTreeGenerators`,
    `SearchTreeTraversal`, `ValueSequence removed
    (available in package `searchtree` as `Control...`)
  * Library `AnsiCodes` removed (available in package `ansi-terminal`
    as `System.Console.ANSI.Codes`)
  * Library `Array` removed (available in package `array` as `Data.Array`)
  * Library `Combinatorial` removed (available in package `combinatorial`)
  * Library `CPNS` removed
    (available in package `cpns` as `Network.CPNS`)
  * Library `Dequeue` removed (available in package `queue` as `Data.Queue`)
  * Library `Distribtion`: `curryCompilerRevisionVersion` added,
    operations related to load paths removed (available in package
    `currypath` in library `System.CurryPath`),
    operations to call the front end removed (available in package
    `frontend-exec` in library `System.FrontendExec`)
  * Library `Format` removed (available in package `printf` as `Data.Format`)
  * Library `NamedSocket` removed
    (available in package `cpns` as `Network.NamedSocket`)
  * Library `Nat` removed (available in package `peano` as `Data.Nat`)
  * Library `Profile` removed
    (available in package `profiling` as `Debug.Profile`)
  * Library `PropertyFile` removed
    (available in package `propertyfile` as `Data.PropertyFile`)
  * Library `Random` removed (available in package `random` as `System.Random`)
  * Libraries `RedBlackTree`, `SetRBT`, and `TableRBT` removed
    (available in package `redblacktree` as `Data.RedBlackTree`,
    `Data.Set.RBTree`, and `Data.Table.RBTree` with slightly renamed API
    operations).
  * Library `SCC` removed (available in package `scc` as `Data.SCC`)
  * Library `Socket` removed
    (available in package `socket` as `Network.Socket`)
  * Library `SetFunctions` removed
    (available in package `setfunctions` as `Control.SetFunctions`)
  * Library `Traversal` removed
    (available in package `traversal` as `Data.Traversal`)
  * Library `Test.EasyCheck` removed (available in package `easycheck`).
    The import of this library should be replaced by `Test.Prop`.
  * Library `Test.Contract` removed
    (available in package `contracts`)
  * Front end updated in order to generate ASTs with source code
    position information (see package `curry-ast`).
  * Curry Port Name Server removed from `currytools`
    (available as `curry-cpnsd` in package `cpns`)
  * Registry for dynamic web pages removed from `currytools`
    (available as `curry-cgi` in package `html-cgi`)
  * Old partial evaluator (command "peval") removed from tools since
    it is available as package `peval-noshare`.


Release notes for PAKCS Version 2.0.2 (November 15, 2018)
----------------------------------------------------------

Changes to version 2.0.0:

  * Library `Findall`: `oneValue` added
  * Library `SetFunctions`: `minValueBy` and `maxValueBy` added, `minValue`
    and `maxValue` depend on `Ord` context.
  * Parameter `-cpmexec` added to script `pakcs-makecgi`.


Release notes for PAKCS Version 2.0.0 (January 12, 2018)
--------------------------------------------------------

This version has almost the same functionality as version 1.15.0
but adds type classes similar to Haskell 98.
In addition to version 1.15.0 and type classes,
this version contains the following changes:

  * `define` command removed (since it was based on a quite restricted parser)
  * Free variable mode (option `+free`) removed since it has many restrictions
    and is no longer compatible with type classes.
  * Base libraries are now versioned. The actual version of the base
    libraries can be queried by `pakcs --base-version` or inside
    Curry programs by the operation `Distribution.baseVersion`.
    The versioning of base libraries is intended to be used by CPM.
  * Sources of compiler are contained in directory `src` (instead of
    `curry2prolog` as in versions 1.x).
  * Specification files for external operations renamed from `xxx.prim_c2p`
    to `xxx.pakcs` (for conformity with other Curry compilers).
  * Library `SetFunctions`: `minValueBy` and `maxValueBy` added, `minValue`
    and `maxValue` depend on `Ord` context.
  * Some libraries removed since they are available as packages
    which can easily be installed via `cypm`:

      - `Assertion`
        (no longer used since `currytest` has been replaced by `currycheck`)
      - `CLP*` (now in package `clp-pakcs`)
      - `CSV` (now available as `Text.CSV` in package `csv`)
      - `GlobalVariable` (now available in package `global-variables`)
      - `Parser` (now available in package `fl-parser`)
      - `PlProfileData` (now available in package `profiling`)
      - `Ports` (now available in package `ports`)
      - `Pretty` (now available in package `wl-pprint` as `Text.Pretty`,
         where `Pretty.pretty` has been renamed to `Text.Pretty.showWidth`)
      - `RegExp` (now available in package `regexp`)
 

Release notes for PAKCS Version 1.15.0 (May 15, 2018)
-----------------------------------------------------

Changes to version 1.14.2:

  * Readline editing added to REPL in SWI-Prolog (thanks to Jan Wielemaker)
  * Curry Package Manager added as tool `cypm`.
  * Various tools (e.g., addtypes, currybrowser, currycheck, currydoc,
    currypp, erd2curry, runcurry, spicey, verify, xmldata)
    have been removed from the distribution
    since they are not necessary for the basic use of PAKCS and they can
    easily be installed (by a one-line command) locally via `cypm`.
    Instructions how to install these tools are included
    in the PAKCS manual.
  * Operation `RegExp.match`: order of arguments swapped
  * Curry preprocessor does not generate implicit `match` for regexps.
  * Some libraries removed since they are available as packages
    which can easily be installed via `cypm`:

      - `AbstractCurry.*` (now in package `abstract-curry`)
      - `Bootstrap3Style` (now in package `html`)
      - `CLPB` (now in package `clpbool`)
      - `CHR` (now in package `chr-curry`)
      - `CategorizedHtmlList` (now in package `html`)
      - `CurryStringClassifier` (now in package `addtypes`)
      - `Database.ERDGoodies` (now in package `ertools`)
      - `Database.ERD` and `Database.CDBI.*` (now in package `cdbi`)
      - `FlatCurry.*` (now in package `flatcurry`)
      - `FlatCurry.Annotated.*` (now in package `flatcurry-annotated`)
      - `GraphInductive` (now in package `graph-inductive`)
      - `GUI` (now in package `gui`)
      - `HTML` (now in package `html` as library `HTML.Base`)
      - `HtmlParser` (now in package `html`)
      - `KeyDatabaseSQLite` (now in package `keydb`)
      - `JavaScript` (now in package `javascript`)
      - `Mail` (now in package `mail`)
      - `Markdown` (now in package `markdown`)
      - `Prolog` (now in package `prolog`)
      - `Rewriting.*` (now in package `rewriting`)
      - `URL` (now in package `url`)
      - `WUI` and `WUIjs` (now in package `wui`)
      - `XML` (now in package `xml`)
      - `XmlConv` (now in package `xml`)

  * Libraries `tools/ui` removed since they are available as
    CPM package `ui`.
  * Command `:xml` removed (since the XML format is deprecated).
  * Compatibility with newer version of SWI-Prolog (7.x) improved.
  * Changing memory limits for compiled Curry programs with SWI-Prolog
    supported (see section about technical problems in the manual).


Release notes for PAKCS Version 1.14.2 (February 23, 2017)
----------------------------------------------------------

Changes to version 1.14.0:

  * Makefiles changed so that parallel build (make -j) is possible.
  * Library `Database.ERD...` added (formerly part of ER currytools).
  * Libraries `IOExts` and `Global` use system commands
    `lockfile-create` and `lockfile-remove` instead of `lockfile`
    for internal file synchronization in order to remove dependency
    on package `procmail`.
  * Library `Nat` for Peano numbers added.
  * Libraries `Rewriting.*` for term rewriting in Curry extended
    to deal with rewriting strategies, narrowing strategies,
    critical pairs, definitional trees.
  * Library `Sort`: ...Sort operations renamed to ...SortBy and
    ...Sort operations with standard ordering added.
  * Library `State` with an implementation of the state monad added.
  * Library `Test.EasyCheck` split into two modules to have less
    import dependencies when putting properties into a module.
  * Library `Test.Prop` added as a clone of `Test.EasyCheck` which defines
    the interface but no implementation so that it does not import
    any other library.
    import dependencies when putting properties into a module.
  * CurryDoc shows properties and contracts, if they are present
    in source files, in the HTML documentation.
  * The Curry Preprocessor supports a new option `contracts`
    to transform contracts (specifications, pre/postconditions)
    into run-time assertions.
  * New partial evaluator (command "peval") added to `currytools`.
  * New tool Curry2Verify (to translate Curry programs into Agda programs)
    added to `currytools`.
  * Obsolete script `parsecurry` removed.
  * Tools `cpns` (Curry Port Name Server) and `www` (web scripting)
    moved to `currytools`.
  * Names of tool executables changed to `pakcs toolname` or `curry toolname`.
  * Name of Curry parser `cymake` changed to `curry frontend`.
  * Tool `curry analysis` (CASS): option `--all` added.
  * Tool `curry analysis` (CASS): analysis `Functional` added and
    analysis `Deterministic` modified so that it considers encapsulated search.
  * Tool `curry analysis` (CASS): simple termination analysis `Terminating`
    added.
  * Tool `curry analysis` (CASS): analysis `TypesInValues` added.
  * Tool `curry check` supports also testing with float arguments.
  * Run-time parameters passed to PAKCS must be separated by `--`.
  * Installation made more flexible by supporting environment variable
    `PAKCSINSTALLDIR` to specify the location where PAKCS is moved
    after the build process.


Release notes for PAKCS Version 1.14.0 (April 19, 2016)
------------------------------------------------------

Changes to version 1.13.0:

  * Type `Success` is now a type synonym for `Bool` and
    `success` is defined as `True` in the prelude.
  * Library Constraints removed since it is no longer necessary
    due to the equivalence of `Success` and `Bool`.
  * Operation `Prelude.compare` and related comparison operations
    are flexible (instead of rigid as before) on user-defined datatypes.
    It still suspends when comparing two variables or
    one variable with a number or character.
  * Prelude: operations `(===)` and `(&&>)` removed
  * Libraries CLPFD and CLP.FD: labeling options  `RandomVariable` and
    `RandomValue` added
    (currently only supported by SWI-Prolog)
  * Library Distribution: some load path handling operations
    (`findFileInLoadPath`, `lookupFileInLoadPath`, `readFirstFileInLoadPath`,
    `getLoadPath`, `getLoadPathForFile`) removed since they are deprecated
    (use operations like `getLoadPathForModule` and
    `lookupModuleSourceInLoadPath` instead of the deprecated operations
    since they handle hierarchical module names better)
  * Libraries `List`: `diagonal` added
  * Libraries `meta/*` removed (since they have been replaced
    by libraries with hierachical names, see below)
  * Hierarchical libraries for FlatCurry added and extended:
    in order to to compatible with future versions, the following
    imports should be adapted in programs working with AbstractCurry:

    - replace `import FlatCurry` by

          import FlatCurry.Types
          import FlatCurry.Files

    - replace `import FlatCurryGoodies` by

          import FlatCurry.Goodies

    - replace `import FlatCurryPretty` by

          import FlatCurry.Pretty

    - replace `import FlatCurryRead` by

          import FlatCurry.Read

    - replace `import FlatCurryShow` by

          import FlatCurry.Show

    - replace `import FlatCurryXML` by

          import FlatCurry.XML

    - replace `import CompactFlatCurry` by

          import FlatCurry.Compact

    - replace `import AnnotatedFlatCurry` by

          import FlatCurry.Annotated.Types

    - replace `import AnnotatedFlatGoodies` by

          import FlatCurry.Annotated.Goodies

    - replace `import AnnotatedFlatCurryPretty` by

          import FlatCurry.Annotated.Pretty

    - replace `import FlexRigid` by

          import FlatCurry.FlexRigid

  * New libraries added: Bootstrap3Style, ErrorState, SCC, Rewriting.*,
    FlatCurry.Annotated.TypeInference
  * currytools: typeinference removed since it is now contained
    in the standard system libraries
  * currytools: new tool `currycheck` for automated test execution added
  * PAKCS option "-c" removed and replaced by REPL command ":compile"
    (the old option was only internally used for building the libraries)


Release notes for PAKCS Version 1.13.1 (October 2, 2015)
--------------------------------------------------------

Changes to version 1.13.0:

  * Hierarchical libraries for AbstractCurry added and extended:
    in order to to compatible with future versions, the following
    imports should be adapted in programs working with AbstractCurry:

    - replace `import AbstractCurry` by

          import AbstractCurry.Types
          import AbstractCurry.Files

    - replace `import AbstractCurryGoodies` by

          import AbstractCurry.Select
          import AbstractCurry.Build

    - replace `import PrettyAbstract` by

          import AbstractCurry.Pretty

  * Library `CLP.FD` added: its functionality is similar to the old
    library `CLPFD`, but the interface is different so that other
    FD constraint solvers are easier to connect.


Release notes for PAKCS Version 1.13.0 (August 24, 2015)
--------------------------------------------------------

Changes to version 1.12.0:
  * Operation `Prelude.==` is flexible (instead of rigid as before).
    It still suspends when comparing two variables or
    one variable with a number or character.
    However, if `Prelude.==` is (positively) used in conditions of rules,
    it is automatically transformed into a unification constraint
    so that it does not suspend.
  * Library `Pretty`: code and interface updated, e.g., some combinators
    renamed and new combinators added (e.g., to support ANSI formatting
    and colorisation of documents)


Release notes for PAKCS Version 1.12.0 (July 15, 2015)
------------------------------------------------------

Changes to version 1.11.5:
  * The extension for records (with special Curry syntax) was removed.
    Instead, PAKCS now supports Haskell's record syntax.
    See the manual for a detailed description of the new record syntax.
  * The Curry syntax is extended to also support binary integer literals.
    For instance, `0b101010` or `0B101010` can now be lexed
    and are converted to the integer value `42`.
  * The definition of AbstractCurry has been changed to sync with the
    changes of Curry done during the last years:
    - AbstractCurry files now contain version information
    - support for new record syntax
    - support for newtype declarations
    - evaluation annotations removed
    - arity of constructor declarations removed
    - simplified representation of function rules
    - String literals added
  * Library `FunctionInversion` added
  * Library `AnnotatedFlatCurryPretty` added
  * Library `Either` extended with functions `fromLeft` and `fromRight`
  * Library `ShowS` added
  * Library `AnsiCodes` added
  * Prelude operation `===` added


Release notes for PAKCS Version 1.11.5 (February 28, 2015)
----------------------------------------------------------

Changes to version 1.11.4:
  * Support for hierarchical module names added,
    libraries `Distribution` and `FlatCurry` adapted for
    this purpose.
  * Library `AbstractCurryGoodies` added.
  * Libraries `CHR` and `CHRcompiled` to implement Constraint Handling Rules
    in Curry added together with various examples in `examples/CHR`.


Release notes for PAKCS Version 1.11.4 (October 16, 2014)
---------------------------------------------------------

Changes to version 1.11.3:
  * The interactive top-level loop uses the standard front end
    to parse expressions if the option `+free` is not set.
  * Handling of path names in PAKCS commands improved, e.g.,
    the command `:load ~/rev` loads module `rev` from the home directory.
  * PAKCS command `edit` accepts module names (instead of arbitrary file names).
  * PAKCS can also be invoked via the command `curry` in the bin directory.
  * PAKCS options have the form of commands (e.g., `pakcs :set v2 :load rev`).
    Old options (except setting of `.pakcsrc` properties) are no longer
    necessary and, thus, not supported.
  * PAKCS command `:save`: if no argument is given, the executable
    is saved with the initial expression `main`.
  * PAKCS command `:source` added: allows separate viewing of visible or
    qualified function definitions.
  * PAKCS command `let var=exp` has now command syntax: `:define var=exp`
  * PAKCS option `args` for setting run-time arguments added.
  * Library `Prelude`: `solve` and `&&>` added.
  * Library `Findall`: `rewriteAll` and `rewriteSome` added (experimental!).
  * Library `Prolog` added.
  * Library `Unsafe`: `compareAnyTerm` and `isGround` added.
  * Tool `CASS`: new analysis `RequiredValues` added.
  * Optimization tool `bindingopt` for transforming Boolean equalities
    into constraint equalities added.
  * Tool `data2xml` for data conversion to XML and back added.


Release notes for PAKCS Version 1.11.3 (July 21, 2014)
------------------------------------------------------

Changes to version 1.11.2:
  * PAKCS parameter `--safe` added to support safe execution without
    I/O actions.
  * Option ":set +/-interactive" added for setting interactive mode.
    Initially, the interactive mode is TURNED OFF (new in this
    version!). The default value can be set in the `.pakcsrc` file.
  * Option ":set parser" added for setting options passed to cymake.
  * Bug fix to ensure compatibility with SWI-Prolog Vers. 6.6
  * Old deprecated libraries (`AbsCurry`, `AbsCurryIO`, `DaVinci`,
    `Flat`, `FlatCurryTools`, `FlatTools`, `FlatXML`, `Flat2FCY`, `Tk`)
    moved to `tools/lib_reprecated`.
  * Prelude: definition of `div` changed (to be compatible with Haskell)
    and `quot`, `rem`, `divMod`, `quotRem` added.
  * Prelude: IOError type extended with constructors for
    user/fail/nondet errors and implementation of `catch` adapted.
  * Prelude: `catchFail` omitted (use `catch` instead).
  * Prelude: encapsulated search operations (findall and more) moved
    into new library `Findall`.
  * Library `Distribution`: front-end parameters extended by supporting
    "special" (i.e., aribrary) arguments.
  * Library `Distribution`: front-end paramter `outfile` removed and
    front-end parameter `htmldir` added.
  * Library `Distribution` and `FlatCurry`: after calling the front end,
    an exception is raised if the front end returns with an error
    (due to an illegal source program).
  * Libraries `Float` and `Integer`: Power operators added.
  * Library `Float`: hyperbolic/ arc sine/cosine/tangent operators added.
  * Library `HTML`: `formMetaInfo` added, HTML header changed to HTML5.
  * Library `SetFunctions`: `notEmpty` added.
  * Libraries `Format` and `RegExp` added.
  * Tool `erd2curry` updated to new version with support for SQLite database.
    Bug fix in code generation w.r.t. checking of cardinality constraints.
  * Tool `spicey` (web framework) added and updated with a RESTful interface
    for entities.


Release notes for PAKCS Version 1.11.2 (September 6, 2013)
----------------------------------------------------------

Changes to version 1.11.1:
* Front-end updated (it writes and reads new kinds of interface files
  with the suffix `.icurry`).
* Saved states, i.e., executables for a main program `prog`
  are now stored in the file `prog` (instead of `prog.state`
  as in previous releases).
* CASS tool (Curry Analysis Server System) added and integrated
  into currydoc.
* Interactive command ":analyze" removed and ":usedimports" added.
  The removed features to analyze a program are available via the
  Curry Browser.
* Library `Function` added.
* Library `HTML`: default (white) background for body of generated
  web pages removed (since this is usually defined in css files).
* Library `IO`: Fixed a bug in `hGetLine` which caused an end-of-file error
  when reading a non-empty line without a newline termination.
* Type inference tool (see `currytools/typeinference`) added.
  This tool can be used to annotate expressions in FlatCurry programs
  with their type.


Release notes for PAKCS Version 1.11.1 (February 13, 2013)
----------------------------------------------------------

Changes to version 1.11.0:
* The front end accepts typed expressions of the form "Expr :: TypeExpr".
  However, such type annotations are currently ignored by the compiler
  since the target language Prolog is untyped.
* Parser strategy for combinations of functional and non-linear patterns
  improved. Parser also accepts as-patterns inside functional patterns.
* Library `Directory`: Operation `copyFile` added.
* Library `FlatCurry` extended to represent typed expressions
  by a new constructor `Typed`.
* Library `SetFunctions`: Operations `choose` and `select` added.
* Bug fix in script `parsecurry`.


Release notes for PAKCS Version 1.11.0 (December 18, 2012)
---------------------------------------------------------

Changes to version 1.10.1:
* New front end version included that implements new features of Curry,
  e.g., `fcase` expressions and non-linear patterns in left-hand sides
  of program rules.
* Syntax of records slightly changed to avoid problems
  with guarded case branches (their syntax conflicted with old syntax
  for selection of record fields):
  - Field selection operator is "`:>`" (instead of "`->`" in old syntax).
  - Record construction uses the operator "`:=`" for setting field values
    similarly to record updates (instead of "`=`" in the old syntax).
* Showing characters: `print '\''` fixed
* Library `GUI`: types of various main operations (like `runInitGUI`,
  `runInitGUIwithParams`) slightly changed (for compatibility with
  KiCS2 version of the library). The old operations are still
  available as `runInitGUI'` etc.
* Library `Markdown`: title argument added to `markdownText2CompleteHTML`
* Custimzation variables in `.pakcsrc` file extended.

-------------------------------------------------------------------------

[Release notes for older versions](http://www.informatik.uni-kiel.de/~pakcs/download/OLDRELNOTES.html)
