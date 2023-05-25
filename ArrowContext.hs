module ArrowContext where

{-# LANGUAGE DatatypeContexts #-}

import Distribution
import Control.Arrow

data Split c a b x y s t where
    Split :: { part1 :: c a       (m , x)
             , part2 :: c (m , y) (n , s)
             , part3 :: c (n , t) b 
             } -> Split c a b x y s t

data Atom c a b where
    Atom :: { atom :: c a b } -> Atom c a b

data Context c a b x y where
    Context :: { partA :: c a (m , x) 
               , partB :: c (m , y) (m , b) 
               } -> Context c a b x y


protocol :: 
  Split (Kleisli Distribution) Client Client 
    (Syn, Ack) -- ! 
    (Syn, Ack) -- ?
    (Syn, Ack) -- !
    ()         -- ? 
  -> Split (Kleisli Distribution) Server Server 
    ()         -- ! 
    (Syn, Ack) -- ?
    (Syn, Ack) -- !
    (Syn, Ack) -- ?
  -> (Client, Server) ~> (Client, Server)
protocol 
  (Split (Kleisli client1) (Kleisli client2) (Kleisli client3)) 
  (Split (Kleisli server1) (Kleisli server2) (Kleisli server3)) 
  (client , server) = do
    (server, ())    <- server1(server)
    (client, (s,a)) <- client1(client)
    (s, a) <- noise 0.1 (s,a)
    (server, (s,a)) <- server2(server, (s,a))
    (s, a) <- noise 0.1 (s,a)
    (client, (s,a)) <- client2(client, (s,a))
    (s, a) <- noise 0.1 (s,a)
    (server)        <- server3(server, (s,a))
    (client)        <- client3(client, ())
    return (client, server)

type Client = Int
type Server = Int
type Syn = Int
type Ack = Int
type Noise = Rational

random :: Distribution Int
random = Distribution [ ( 10 , 1 / 2 ) , ( 20 , 1 / 2 ) ]

type (a ~> b) = (a -> Distribution b) 


-- The following is the separate code for each part of the client 
-- and server.
syn :: Client ~> (Client, Syn, Ack)
syn(client) = do
  client <- random
  return (client, client, 0)

synack :: (Syn, Ack, Server) ~> (Syn, Ack, Server)
synack(syn, ack, server) = do
  server <- random
  return (if syn == 0 then (0,0,0) else (server, ack+1, server))

noise :: Noise -> (Syn, Ack) ~> (Syn, Ack)
noise k (syn,ack) = do
  noise <- binomial k
  return (if noise then (0,0) else (syn,ack))

ack :: (Client, Syn, Ack) ~> (Client, Syn, Ack)
ack(client, syn, ack) = do
  return (if client+1 /= ack then (0,0,0) else (client+1, syn+1, client))

receive :: (Syn, Ack, Server) ~> Server
receive(syn, ack, server) = do
  return (if server+1 /= ack then 0 else server)