# hide-my-js

A brutally simple Javascript obfuscator written in Rust, leveraging the [oxc](https://github.com/oxc-project/oxc/tree/main) toolchain.

It is capable of a handful of potent obfuscation passes ranging from renaming variables and functions to control flow flattening.

## How can I use this?

You can simply pass a path to a Javascript file to the CLI and provide the passes you want to be performed as arguments.

```
  hide-my-js file.js -renaming -numeric -string -control_flow_flattening -dead_code
```
