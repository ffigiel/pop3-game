import { Elm } from './src/Main.elm'

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    highScore: window.localStorage.getItem("highScore"),
  },
})
app.ports.saveHighScore.subscribe(hs => {
  window.localStorage.setItem("highScore", hs)
})
