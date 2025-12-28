Bitcoin Lightning Wallet
------------------------

## Project state

After some years without updates the wallet stopped working, but I've fixed
several bugs and now it's usable again.

Use it at you own risk.

## Runtime Dependencies

To fetch fiat and mempool rates and also calculate lightning routes, you must
connect to a server, [Golympus](http://github.com/mrbitcoiner/golympus) can do
that for you.

## Building

To build, a x86-64 machine with podman is needed.

1. build the image with `./control.sh build`

2. start the container with the gradle daemon with `./control.sh up`

3. rebuild the apk at `./app/build/outputs/apk/app-debug.apk` with the command: 
`./control.sh build-apk`

## Contributing

Feel free to open a pull request, but this is not my full time job, be 
respectful.

## Acknowledgments

A huge thanks for the project creator, Anton Kumaigorodski.

This wallet uses modified code from [Eclair](https://github.com/ACINQ/eclair)
project.

