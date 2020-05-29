import { Elm } from './elm/Main.elm'
import qsort from './data/qsort'


const app = Elm.Main.init({
  flags: { qsort }
})

const host = 'ws://qmul-qsort.herokuapp.com/'
const socket = new WebSocket(host)

socket.onmessage = event => {
  app.ports.fromWebSocket.send(
    JSON.parse(event.data)
  )
}

app.ports.toWebSocket.subscribe(data => {
  socket.send(
    JSON.stringify(data)
  )
})

