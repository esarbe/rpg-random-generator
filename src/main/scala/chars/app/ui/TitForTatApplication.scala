package chars.app.ui

import chars.titfortat.TitForTat
import com.monovore.decline.CommandApp


//mvp:
// - (/) run IteratedPrisonersDilemma
// - (/) run IteratedPrisonersDilemma with participants type distribution taken from cmd line
// - (/) run IteratedPrisonersDilemma interactively from cmd line
// - run IteratedPrisonersDilemma interactively from web gui
// - run IteratedPrisonersDilemma interactively concurrently
// - accounts
// - scoreboard
// - UX
// - context features
// - player traits

object TitForTatApplication
  extends CommandApp(
    name = "TitForTat",
    header = "Iterated Prisoner's Dilemma with Extra Sauce",
    main = TitForTat.runGame,
    helpFlag = true,
    version = "0.0.1-SNAPSHOT")