module Client where

import Distribution
import ArrowContext
import Control.Arrow


client :: Split (Kleisli Distribution) Client Client 
  (Syn, Ack) -- !
  (Syn, Ack) -- ? 
  (Syn, Ack) -- !
  ()         -- ? 
client = Split 
     
    -- Part 1: Send a SYN message.
    { part1 = Kleisli $ \client -> do
        client <- pure 10
        return (client, (client, 0))
    
    -- Part 2: Receive ACK, send ACK.
    , part2 = Kleisli $ \(client, (syn, ack)) -> do
        return (if client+1 /= syn then (0,(0,0)) else (client, (client+1, ack+1)))

    -- Part 3: Close protocol.
    , part3 = Kleisli $ \(client, ()) -> do
        return client
    }   