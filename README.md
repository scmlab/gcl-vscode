# Guacamole on VS Code

## Installation

[![MarketPlace Tag](https://vsmarketplacebadge.apphb.com/version/scmlab.guacamole.svg)](https://marketplace.visualstudio.com/items?itemName=scmlab.guacamole)

Search for "Guacamole" and install it from the Visual Studio Marketplace

## Development

After cloning this repository, download dependencies and build files with:

```bash
npm install 
npm run build
```

Fire up this command to enter the "watch mode" so that you don't have to rebuild stuff manually:

```bash 
npm run dev
```

Press <kbd>F5</kbd> in VS Code and you should have this extension up and running.

### Deployment

Say, we have a new version `v0.0.18` and we want to publish it to the Marketplace:

```
git tag v0.0.18
git push --tags
```

This should trigger [the deployment process](https://github.com/scmlab/gcl-vscode/runs/2552584344?check_suite_focus=true) on GitHub Actions.

Before publishing, please make sure that there's [a corresponding release](https://github.com/scmlab/gcl/releases/tag/v0.0.18) of `gcl` with the exact version and tag name (`v0.0.18` in this case). So that the extension could fetch the correct prebuilt binary from GitHub.

