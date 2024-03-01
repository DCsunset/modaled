# Changelog

All notable changes to this project will be documented in this file. See [commit-and-tag-version](https://github.com/absolute-version/commit-and-tag-version) for commit guidelines.

## [0.8.2](https://github.com/DCsunset/modaled/compare/v0.8.1...v0.8.2) (2024-03-01)


### Features

* add main state and its helper functions ([9ff4b59](https://github.com/DCsunset/modaled/commit/9ff4b59b60bcbad79d67acf15b9871b6b971df0b))


### Bug Fixes

* fix bugs in modaled-get-main-state ([19dc77a](https://github.com/DCsunset/modaled/commit/19dc77a95ff934813cead3c7379ffde82524caaa))


### Misc

* add detailed instruction for Nix flake installation ([7eb128b](https://github.com/DCsunset/modaled/commit/7eb128b44da59a5a303a00b360f66f7f8c40c3ce))
* fix language for code block ([fdc8506](https://github.com/DCsunset/modaled/commit/fdc850692e475a0424f6678d2691f4df8fd709b9))

## [0.8.1](https://github.com/DCsunset/modaled/compare/v0.8.0...v0.8.1) (2023-10-28)


### Features

* allow enabling substate on state change ([244b3ef](https://github.com/DCsunset/modaled/commit/244b3ef980740f8da18f8c5f2394c8ffb67a056e))


### Misc

* add deprecation warning and fix a typo ([33d23db](https://github.com/DCsunset/modaled/commit/33d23db62fd835969edfaafe0aa94e4f38f99ee6))
* add example for modaled-enable-substate-on-state-change ([c3244a9](https://github.com/DCsunset/modaled/commit/c3244a927bb1ed864814318dfb45ad5c016738fc))
* fix an example of key definition ([bb0a63b](https://github.com/DCsunset/modaled/commit/bb0a63b04be499610680b50ea3acec5f3b47dbd1))

## [0.8.0](https://github.com/DCsunset/modaled/compare/v0.7.0...v0.8.0) (2023-10-26)


### Features

* add modaled-define-keys for more generic key binding ([269e285](https://github.com/DCsunset/modaled/commit/269e28526937c8abf113047891233c6ff0da9dee))


### Misc

* fix doc for defining keys ([8579390](https://github.com/DCsunset/modaled/commit/85793900936864598481f3cab9356f9165683c3d))

## [0.7.0](https://github.com/DCsunset/modaled/compare/v0.6.0...v0.7.0) (2023-10-14)


### ⚠ BREAKING CHANGES

* support different default states for different major modes

### Features

* support different default states for different major modes ([20436da](https://github.com/DCsunset/modaled/commit/20436da84f15c0159341b52b53ae833c08c45b56))


### Misc

* add more type output in changelog ([cc1212a](https://github.com/DCsunset/modaled/commit/cc1212a893e6bc5e668ad24589cfb175a8069304))
* update docs for default states ([f4aae9d](https://github.com/DCsunset/modaled/commit/f4aae9d0c433056fc536136959949035b9088a23))

## [0.6.0](https://github.com/DCsunset/modaled/compare/v0.5.0...v0.6.0) (2023-08-25)


### ⚠ BREAKING CHANGES

* suppress keymap by default and add no-suppress option

### Features

* suppress keymap by default and add no-suppress option ([6cece47](https://github.com/DCsunset/modaled/commit/6cece47abd2a24b0f3238ae1789c05cc8b35ccb6))


### Bug Fixes

* update docs for insert state ([76905b0](https://github.com/DCsunset/modaled/commit/76905b05a75b6ef92594383121d9d84ec3bf4988))

## [0.5.0](https://github.com/DCsunset/modaled/compare/v0.4.0...v0.5.0) (2023-08-20)


### ⚠ BREAKING CHANGES

* add keymap to emulation-mode-map-alists by default

### Features

* add keymap to emulation-mode-map-alists by default ([afae3cb](https://github.com/DCsunset/modaled/commit/afae3cb778fb3f2f65eb6f4f06e4cfcefb427a0c))
* make internal functions regarding the mode symbols public ([9426c28](https://github.com/DCsunset/modaled/commit/9426c286bef2a9e0c43146af4b8fde3f6a3bc7a6))
* support adding state and its keymap to emulation-mode-map-alist ([60997f6](https://github.com/DCsunset/modaled/commit/60997f6d562008303e3768465a9116169306d00e))
* support binding multiple keys to the same command ([e47bd42](https://github.com/DCsunset/modaled/commit/e47bd427a2161193b85dc6121c975833f867f47e))


### Bug Fixes

* fix docs of functions and macros ([58a21b2](https://github.com/DCsunset/modaled/commit/58a21b213d63ec938f4bffe1b42738d0efc2d42c))

## [0.4.0](https://github.com/DCsunset/modaled/compare/v0.3.3...v0.4.0) (2023-06-25)


### ⚠ BREAKING CHANGES

* add substate and rename state keymap and mode

### Features

* add substate and rename state keymap and mode ([3999b7f](https://github.com/DCsunset/modaled/commit/3999b7ff94d01311871f29263eb03f3d4cdf8219))

## [0.3.3](https://github.com/DCsunset/modaled/compare/v0.3.2...v0.3.3) (2023-06-17)


### Bug Fixes

* prevent re-enabling states and skipping active state as mode may be disabled in other ways ([70c6c60](https://github.com/DCsunset/modaled/commit/70c6c6029c430647fd510b44f9a8caa74b562d07))

## [0.3.2](https://github.com/DCsunset/modaled/compare/v0.3.1...v0.3.2) (2023-06-16)


### Bug Fixes

* do nothing if changing to the active state ([0ea6447](https://github.com/DCsunset/modaled/commit/0ea6447c006299ed0520c80cf55789877a020f6f))
* improve indentation for macros ([3a39f38](https://github.com/DCsunset/modaled/commit/3a39f38d3a87565d640330a87e42feb456ea5785))

## [0.3.1](https://github.com/DCsunset/modaled/compare/v0.3.0...v0.3.1) (2023-06-12)


### Bug Fixes

* make modaled-state buffer local ([3b91bc0](https://github.com/DCsunset/modaled/commit/3b91bc0658e058f5c353a7af90e3f0cef55e6207))
* remove unnecessary eval function ([2ebe985](https://github.com/DCsunset/modaled/commit/2ebe985f8b170b101f4f8de5f833ba5718761638))

## [0.3.0](https://github.com/DCsunset/modaled/compare/v0.2.0...v0.3.0) (2023-05-31)


### ⚠ BREAKING CHANGES

* remove kbd function for defining keys to provide better flexibility

### Features

* support :predicate arg in define-default-state ([8681e23](https://github.com/DCsunset/modaled/commit/8681e23216084ee2af1d2cd826115d0135543ebc))


### Bug Fixes

* remove kbd function for defining keys to provide better flexibility ([3324c7e](https://github.com/DCsunset/modaled/commit/3324c7e79dbfab7e0a57390b2879e7edabc55b20))
* update description and use spaces for indentation ([ff0eee4](https://github.com/DCsunset/modaled/commit/ff0eee4f8830ed022c8772e5910743e9c8b46768))

## [0.2.0](https://github.com/DCsunset/modaled/compare/v0.1.0...v0.2.0) (2023-05-26)


### Features

* support defining keys for multiple states ([45bb7ca](https://github.com/DCsunset/modaled/commit/45bb7cac0ffa06844ef9af5b6086d73ddd59b6b1))


### Bug Fixes

* add require for global minor mode ([d4ca18c](https://github.com/DCsunset/modaled/commit/d4ca18c4c0e5b01e35548667e88cfb5e4b93e3b1))
* fix lint warning ([1100e2d](https://github.com/DCsunset/modaled/commit/1100e2d2d9ca1c9e4a8f9b4e90d9213b8cd9d9dd))
* remove unnecessary logging ([9c5ebc3](https://github.com/DCsunset/modaled/commit/9c5ebc3944b749889897182e740ef7af1f4e5301))

## 0.1.0 (2023-05-17)


### ⚠ BREAKING CHANGES

* remove mode-alist and add define-state-keys

### Features

* add customization group definition ([e3b60ee](https://github.com/DCsunset/modaled/commit/e3b60eea631f6ca9c6dcc24db002b3e9a329ec08))
* add set-default-state command ([c1afb2f](https://github.com/DCsunset/modaled/commit/c1afb2f9d5395297a816da62b342235260a735ff))
* remove mode-alist and add define-state-keys ([60c06fa](https://github.com/DCsunset/modaled/commit/60c06fae8ceee576bd7ca6af9dc581e6bdd49d15))
* support customizing cursor-type and lighter ([77df848](https://github.com/DCsunset/modaled/commit/77df84804e5c9aa8fe925abf80cf6d14e0d82852))
* support sparse keymap and suppressing keymap ([2a47220](https://github.com/DCsunset/modaled/commit/2a47220f18d5b7de031e92abe7f78bfec828304e))


### Bug Fixes

* fix lint error ([5f57682](https://github.com/DCsunset/modaled/commit/5f57682d109bd2781d947b264760b4f45112f9be))
* fix lint warnings ([62f697b](https://github.com/DCsunset/modaled/commit/62f697bb9dbcdc0cc9c3173702c818c2327e7cd2))
* rename variable and add autoload comments ([6e59ac3](https://github.com/DCsunset/modaled/commit/6e59ac3c0bfe01ed2f61efd4d00875510dd49e19))
