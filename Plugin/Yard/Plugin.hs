module Plugin.Yard.Plugin (plugin) where

import GhcPlugins

import Plugin.Yard.Annotations
import Plugin.Yard.Pass


plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
	reinitializeGlobals
	-- Before our plugin runs we want a simplifierpass which inlines, executes rules and eta-expands. The numbers 100 and 1000
	-- have been empirically tested to work.
	-- en de Phase 1000, hoe hoger de phase, hoe eerder oid. http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/pragmas.html#phase-control
	let simpl = CoreDoSimplify 100 $ SimplMode {sm_names = ["pre-inline"], sm_phase = Phase 1000, sm_rules = True, sm_inline = True, sm_case_case = False, sm_eta_expand = True}
	return $ simpl : CoreDoPluginPass "Yard optimisation" (runPass opts) : todo

runPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
runPass _ guts = shouldOptimize guts >>= \optimize -> if optimize then pass guts else return guts

shouldOptimize :: ModGuts -> CoreM Bool
shouldOptimize guts = do
		anns <- getAnnotations deserializeWithData guts :: CoreM (UniqFM [Optimize])
		let modAnns = lookupWithDefaultUFM anns [] $ mg_module guts :: [Optimize]
		return $ null modAnns