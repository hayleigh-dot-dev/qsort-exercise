const Wxpress = require('express')
const WebSocket = require('ws')

const Port = process.env.PORT || 3000

const server = Express().listen(Port)
const socket = new WebSocket.Server({ server })

socket.on('connection', ws => {
  
})
