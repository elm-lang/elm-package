module Install.Plan where

import qualified Data.Map as Map

import qualified Elm.Package.Dependencies as Dep
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V


data Plan = Plan
    { installs :: Map.Map N.Name V.Version
    , upgrades :: Map.Map N.Name (V.Version, V.Version)
    , removals :: Map.Map N.Name V.Version
	}


upgradePlan :: Dep.Solution -> Dep.Solution -> Plan
upgradePlan old new =
	Plan
	{ installs = Map.difference new old
  	, upgrades = discardNoOps (Map.intersectionWith Upgrade old new)
  	, removals = Map.difference old new
	}
  where
  	discardNoOps updates =
  		Map.mapMaybe isChanged updates

  	isChanged upgrade@(oldVersion,newVersion) =
  		if oldVersion == newVersion
  			then Nothing
  			else Just upgrade


-- DISPLAY  	

display :: Plan -> String
display (Plan installs upgrades removals) =
	displayCategory "Install" displayInstall installs
	++ displayCategory "Upgrade" displayUpgrade upgrades
	++ displayCategory "Remove" displayRemove removals
  where
  	displayCategory name render category =
  		if Map.null category then "" else
	  		"  " ++ name ++ ":"
	  		++ concatMap (\entry -> "\n    " ++ render entry) (Map.toList category)
	  		++ "\n"

	displayInstall (name, version) =
  		N.toString name ++ " " ++ V.toString version

  	displayUpgrade (name, (old, new)) =
		N.toString name ++ " (" ++ V.toString old ++ " -> " ++ V.toString new ++ ")"

	displayRemove (name, _version) =
		N.toString name
