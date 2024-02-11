# Infinite-Jest: Multiplayer Infinite Craft

This adds a multiplayer lockout mod to [infinite craft](https://neal.fun/infinite-craft).

## How to play

I host a server at the link hardcoded into the game code. So you only need to run the script to start the game.

- Open https://neal.fun/infinite-craft
- Open the Dev Console with F12 or however else you tend to open it
- Copy the contents of `infinite-jest.js`
- Paste the contents of `infinite-jest.js` into the Dev Console
- You should see some text appear in the top-left of the window, and then it's working! 

## How to change the mod

The client code is written in TypeScript, so edit the TS file and then compile it to get an updated version of the JS code. You'll want to compile it with this command

```
tsc infinite-jest.ts --lib es2017,dom
```

Or you can directly edit the JS if you like, but such changes will not be upstreamed.

To run your own server, install Haskell via `ghcup` and use `cabal run` in the `server` folder. No special configuration is needed.

Happy crafting!
