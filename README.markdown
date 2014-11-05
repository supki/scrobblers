# scrobblers

[![Build Status](https://travis-ci.org/supki/scrobblers.png)](https://travis-ci.org/supki/scrobblers)

Lastfm scrobblers making library

# What's scrobbling?

> Scrobbling is a way to send information about the music a user is listening to. A client is anything that plays music, such as desktop music players, mobile apps, websites, etc.

From [scrobbling guide][0]

# What the simplest possible scrobbler looks like?

We do not provide `magic ∷ IO ()` to solve any scrobbling problem
you may encounter, but you can make working scrobbler simply by composing the relevant parts:

```haskell
import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD

main ∷ IO ()
main = scrobbler $
    scrobble credentials
  . contest
  . updateNowPlaying credentials
  . candidate
```

Each part does roughly what its name says. But to actually explain this, we need to explain `Scrobbler` first

## Wait, what's `Scrobbler`?

I assume you are familiar with [netwire](https://hackage.haskell.org/package/netwire), but if not, there is excerpt from it:

```haskell
data Wire e m a b
    = WGen (Time → a → m (Either e b, Wire e m a b))
  ...

type Scrobbler a b = Wire ScrobblerError IO a b
```

So, you can think of `Scrobbler` as of glorified function of type `a → IO b`
that may "error" and also has some "internal state". So,

## What are parts our scrobbler is composed of?

 First, some helper types definitions (they are all just for descriptive names):

```haskell
data PlayerStateChange a = Started a | Stopped -- isomorphic to Maybe
newtype Scrobble a       = Scrobble a          -- isomorphic to Identity
newtype Successes a      = Successes [a]       -- isomorphic to []
```

Next, the meat of scrobbling, - track information:

```haskell
data Track = Track
  { start  ∷ Int64 -- track start time
  , title  ∷ Text  -- title
  , artist ∷ Text  -- artist
  , album  ∷ Text  -- album title
  , length ∷ Int64 -- length
  , local  ∷ Int64 -- local time
  }
```

Okay, we are all set, let's describe provided `Scrobbler`s:

### Getting scrobblers candidates

```haskell
candidate ∷ Scrobbler Time (PlayerStateChange Track)
```

That wire repeatedly asks player (in our example above, that player would be MPD) if its state was changed.

Player may respond with 3 different answers:

  1. No, my state wasn't changed
  2. Yes, I started playing some track
  3. Yes, I stopped playing

### Update [last.fm][1] profile status

```haskell
updateNowPlaying ∷ Credentials → Scrobbler (PlayerStateChange Track) (PlayerStateChange Track)`
```

That wire notifies [last.fm][1] about changes in player state (only if it worths it:
surely nobody wants to know you stopped playing music) and passes its argument further

### Test if track is worth scrobbling

```haskell
contest ∷ Scrobbler (PlayerStateChange Track) (Scrobble Track)
```

That wire has internal state: previously played track. If player started
the new one or stopped, we want to know if that previously played track
is worth scrobbling. There are 2 choices there:

  1. Yes. So we return the previous track
  2. No. So nothing happens

### Finally, scrobble track

```haskell
scrobble ∷ Credentials → Scrobbler (Scrobble Track) (Successes Track)
```

That wire tries to scrobble incoming `Track` and also all previous failures (if any)
Yes, scrobbling may fail for variety of reasons, so we need to store failures for some time.

## Recap

Now example scrobbler should be clear: (`scrobbler` is just a kind of main loop)

```haskell
main ∷ IO ()
main = scrobbler $
    scrobble credentials
  . contest
  . updateNowPlaying credentials
  . candidate
```

[Casual][2] example is basically this but also with some debug information included.

## Advanced stuff

### Networking

You can split your scrobbler in parts, if you don't like having your lastfm information
on machine where player is started. Or, for example, you want to have one scrobbler for
all players. Or something like that

  * Serialization

    ```haskell
    deserialize ∷ Serialize b ⇒ Scrobbler ByteString b
    serialize   ∷ Serialize a ⇒ Scrobbler a ByteString
    ```

    `serialize` and `deserialize` wires help with serialization of any stuff you can get
    in the process of scrobbling

  * Communication

    Finally:

    ```haskell
    data Failures = Drop | Preserve

    data NetworkSettings = NetworkSettings
      { host     ∷ HostName
      , port     ∷ PortID
      , failures ∷ Failures
      }

    send    ∷ NetworkSettings → Scrobbler ByteString ()
    receive ∷ NetworkSettings → Scrobbler () ByteString
    ```

    `send` sends data to specified `host` and `port`. Also it takes into account failure treating policy:
    if it's to `Drop` failures, then every `send` is successful, otherwise it's `Preserve` and every failed
    sending accumulates data that it tries to resubmit on next activation

    `receive` only cares about `port` part of settings


 [0]: http://www.last.fm/api/scrobbling
 [1]: http://www.last.fm/
 [2]: https://github.com/supki/scrobblers/blob/master/examples/Casual.hs
