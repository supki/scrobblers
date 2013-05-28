# scrobblers

[![Build Status](https://travis-ci.org/supki/scrobblers.png)](https://travis-ci.org/supki/scrobblers)

Lastfm scrobblers making library

# What's scrobbling?

> Scrobbling is a way to send information about the music a user is listening to. A client is anything that plays music, such as desktop music players, mobile apps, websites, etc.

From [scrobbling guide][0]

# How does simplest possible scrobbler look like?

We do not provide `magic :: IO ()` to solve any scrobbling problem
you may encounter, but you can make working scrobbler simply by composing relevant parts:

```haskell
import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD

main :: IO ()
main = scrobbler $
  scrobble credentials . contest . updateNowPlaying credentials . candidate
```

All these parts are actually quite simple, but to explain them, we need to "explain" `Wire`

## Wait, what's `Wire`?

I'll just quote the relevant part of definition:

```haskell
data Wire e m a b
    = WGen (Time -> a -> m (Either e b, Wire e m a b))
  ...
```
I think definition explains `Wire` fairly well, but if not, you may think of it
being function `Monad m => a -> m b` that may "error" and also has some "internal state". So,

## What are parts our scrobbler is composed of?

 First, some helper types definitions (they are all just for descriptive names):

```haskell
data PlayerStateChange a = Started a | Stopped -- isomorphic to Maybe
newtype Scrobble a       = Scrobble a          -- isomorphic to Identity
newtype Successes a      = Successes [a]       -- isomorphic to []
```

Next, meat of scrobbling, track information:

```haskell
data Track = Track
  { start  :: Int64 -- track start time
  , title  :: Text  -- title
  , artist :: Text  -- artist
  , album  :: Text  -- album title
  , length :: Int64 -- length
  , local  :: Int64 -- local time
  }
```

Okay, we are all set, let's describe `Wire`s:

### Getting scrobblers candidates
```haskell
candidate :: Wire Error IO Time (PlayerStateChange Track)
```
That wire repeatedly asks player (in our case, that player would be MPD) if its state was changed.

Player may respond with 3 different answers:

  1. No, my state wasn't changed
  2. Yes, I started playing some track
  3. Yes, I stopped playing

We are not interested in answer 1 at all, but we want to know if 2 or 3 happened. That's what `PlayerStateChange` describes

### Update [last.fm][1] profile status
```haskell
updateNowPlaying :: Credentials -> Wire Error IO (PlayerStateChange Track) (PlayerStateChange Track)`
```
That wire notifies [last.fm][1] about changes in player state (only if it worth it:
surely nobody wants to know you stopped playing music) and passes its argument further

### Test if track is worth scrobbling
```haskell
contest :: Wire Error IO (PlayerStateChange Track) (Scrobble Track)
```
That wire has internal state: previously played track. If player started to playing
the new one or stopped to play anything, we want to know if that previous track
is worth scrobbling. There are 2 choices there:

  1. Yes. So we return previous track
  2. No. So nothing happens

### Finally, scrobble track
```haskell
scrobble :: Credentials -> Wire Error IO (Scrobble Track) (Successes Track)
```
That wire tries to scrobble incoming `Track` and also all other failed to scrobble
before tracks (if they exist) and returns the list of successes

## Recap

Now meaning of example scrobbler should be clear: (`scrobbler` is just a main loop)

```haskell
main :: IO ()
main = scrobbler $
  scrobble credentials . contest . updateNowPlaying credentials . candidate
```
[Casual][2] example is basically this but also with some debug information included.

## Advanced stuff

### Networking

You can split your scrobbler in parts, if you don't like having your lastfm information
on machine where player is started. Or, for example, you want to have one scrobbler for
all players. Or something like that

  * Serialization

	```haskell
	deserialize :: (Serialize b, Monad m) => Wire Error m ByteString b
	serialize :: (Serialize a, Monad m) => Wire e m a ByteString
	```

	`serialize` and `deserialize` wires help with serialization of any stuff you can get
	in the process of scrobbling

  * Encryption

    ```haskell
	encrypt :: (Serialize b, Monad m) => AESKey -> IV AESKey -> Wire e m b ByteString
	decrypt :: (Serialize b, Monad m) => AESKey -> IV AESKey -> Wire Error m ByteString b
	```

	`encrypt` and `decrypt` wires help with encryption of the same stuff. They do
	serialization too, use `encrypt'` and `decrypt'` variants if for some reason
	you don't want them to

  * Communication

    Finally:

	```haskell
	send :: HostName -> PortID -> Wire Error IO ByteString ()
	receive :: PortID -> Wire Error IO () ByteString
	```

	`send` and `receive` will transmit bytestrings back and forth. `send` also maintains the queue of
	faliures and tries to resubmit them every time it gets a chance.

[Hardcore][3] example demonstrates networking


 [0]: http://www.last.fm/api/scrobbling
 [1]: http://www.last.fm/
 [2]: https://github.com/supki/scrobblers/blob/master/examples/Casual.hs
 [3]: https://github.com/supki/scrobblers/blob/master/examples/Hardcore.hs
