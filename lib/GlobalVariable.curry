------------------------------------------------------------------------------
--- Library for handling global variables.
--- A global variable has a name declared in the program.
--- Its value (a data term possibly containing free variables)
--- can be accessed and modified by IO actions.
---
--- In contast to global entities (as defined in the library Global),
--- global variables can contain logic variables shared with
--- computations running in the same computation space.
--- As a consequence, global variables cannot be persistent,
--- their values are not kept across different program executions.
---
--- Currently, it is still experimental so that its interface might
--- be slightly changed in the future.
---
--- A global variable <code>g</code> with an initial value <code>v</code>
--- of type <code>t</code> must be declared by:
--- 
--- <code>g :: GVar t</code><br/>
--- <code>g = gvar v</code>
---
--- Here, the type <code>t</code> must not contain type variables.
--- <code>v</code> is the initial value for every program run.
---
--- Note: the implementation in PAKCS is based on threading a state
--- through the execution. Thus, it might be the case that
--- some updates of global variables are lost if fancy features
--- like unsafe operations or debugging support are used.
---
--- @author Michael Hanus
--- @version March 2010
------------------------------------------------------------------------------

module GlobalVariable(GVar,gvar,writeGVar,readGVar) where

--- The general type of global variables.
data GVar a = GVarDef a | GVarValue () a

--- <code>gvar</code> is only used for the declaration of a global variable
--- and should not be used elsewhere. In the future, it might become a keyword.
gvar :: a -> GVar a
gvar v = GVarDef v

----------------------------------------------------------------------

--- Reads the current value of a global variable.
readGVar :: GVar a -> IO a
readGVar gv = prim_readGVar $# gv

prim_readGVar :: GVar a -> IO a
prim_readGVar external

--- Updates the value of a global variable.
--- The associated term is evaluated to a data term and might
--- contain free variables.
writeGVar :: GVar a -> a -> IO ()
writeGVar gv val = (prim_writeGVar $# gv) $!! val

prim_writeGVar :: GVar a -> a -> IO ()
prim_writeGVar external


------------------------------------------------------------------------
