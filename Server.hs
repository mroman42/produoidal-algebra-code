module Server where

import Distribution
import ArrowContext
import Control.Arrow


server :: Split (Kleisli Distribution) Server Server 
  ()         -- send    ==>
  (Syn, Ack) -- receive <== 
  (Syn, Ack) -- send    ==>
  (Syn, Ack) -- receive <==
server = Split

    -- Part 1: Open protocol.
    { part1 = Kleisli $ \server -> do
        return (server, ())

    -- Part 2: Receive SYN and send ACK.
    , part2 = Kleisli $ \(server, (syn, ack)) -> do
        server <- pure 20
        return (if syn == 0 then (0,(0,0)) else (server, (syn+1, server)))

    -- Part 3: Receive ACK.
    , part3 = Kleisli $ \(server, (syn, ack)) -> do
        return (if server+1 /= ack then 0 else server)
    }
