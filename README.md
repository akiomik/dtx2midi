# dtx2midi

[![Haskell CI](https://github.com/akiomik/dtx2midi/workflows/Haskell%20CI/badge.svg)](https://github.com/akiomik/dtx2midi/actions?query=workflow%3A%22Haskell+CI%22)

.dtx -> .midi converter

## Usage

```
dtx2midi 0.2.1.1 - .dtx -> .midi converter

Usage: dtx2midi (-i|--input DTXFILE) (-o|--output MIDIFILE)
  Convert .dtx file into .midi file

Available options:
  -i,--input DTXFILE       An input .dtx file
  -o,--output MIDIFILE     An output .midi file
  -h,--help                Show this help text
```

## Download

The binaries for linux and macOS can be downloaded from the [release page](https://github.com/akiomik/dtx2midi/releases/latest).

## Build from source

[stack](https://github.com/commercialhaskell/stack) is required.

```bash
git clone https://github.com/akiomik/dtx2midi.git
cd dtx2midi
stack build
```

## License

The BSD 3-Clause License
