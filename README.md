# dtx2midi

[![Haskell CI](https://github.com/akiomik/dtx2midi/workflows/Haskell%20CI/badge.svg)](https://github.com/akiomik/dtx2midi/actions?query=workflow%3A%22Haskell+CI%22)

.dtx -> .midi converter

## Requirements

* [stack](https://github.com/commercialhaskell/stack)

## Installation

```bash
git clone https://github.com/akiomik/dtx2midi.git
cd dtx2midi
stack build
```

## Usage

```
dtx2midi 0.2.1.0 - .dtx -> .midi converter

Usage: dtx2midi (-i|--input DTXFILE) (-o|--output MIDIFILE)
  Convert .dtx file into .midi file

Available options:
  -i,--input DTXFILE       An input .dtx file
  -o,--output MIDIFILE     An output .midi file
  -h,--help                Show this help text
```

## License

The BSD 3-Clause License
