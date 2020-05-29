import { Elm } from './elm/Main.elm'
import qsort from './data/qsort'

const app = Elm.Main.init({
  flags: { qsort }
})

