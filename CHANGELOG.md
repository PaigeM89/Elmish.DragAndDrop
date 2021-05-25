# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Can now find item list index & index by Id.
- Added APIs For `Draggables`, separating the logic between what is grabbable and what actually drags.

## [0.3.0-alpha01] - 2021-05-23

### Added
- New `DragDropContext` to capture events in a wider area than a `droparea`. This lets the user drag things around in an inuitive way without changing where things can be dropped.

### Changed
- Redid namespace organization to improve usability

## [0.2.0] - 2021-05-23

### Added
- Allow the user to specify the HTML tag to use to render a draggable and a drop area. This allows Tables to be used.
- Added helper functions for managing the item collection by the consumer.

### Removed
- Took out the item location dictionary, as it was not needed by the library.
- The update function no longer returns a command as part of a tuple. It was always returning `Cmd.none`, and no commands are needed.

### Changed
- Complete ground up rewrite
- Significanty better performance
- Significantly easier to implement API

### Deprecated
- The entire existing API is gone, replaced by a new one

## [0.2.0-rc3] - 2021-05-23

### Added
- Allow the user to specify the HTML tag to use to render a draggable and a drop area. This allows Tables to be used.
- Added helper functions for managing the item collection by the consumer.

### Removed
- Took out the item location dictionary, as it was not needed by the library.
- The update function no longer returns a command as part of a tuple. It was always returning `Cmd.none`, and no commands are needed.

### Changed
- Complete ground up rewrite
- Significanty better performance
- Significantly easier to implement API

### Deprecated
- The entire existing API is gone, replaced by a new one

## [0.2.0-rc2] - 2021-05-22

### Added
- Added helper functions for managing the item collection by the consumer.

### Removed
- Took out the item location dictionary, as it was not needed by the library.
- The update function no longer returns a command as part of a tuple. It was always returning `Cmd.none`, and no commands are needed.

### Changed
- Complete ground up rewrite
- Significanty better performance
- Significantly easier to implement API

### Deprecated
- The entire existing API is gone, replaced by a new one

## [0.2.0-rc1] - 2021-05-22

### Changed
- Complete ground up rewrite
- Significanty better performance
- Significantly easier to implement API

### Deprecated
- The entire existing API is gone, replaced by a new one

## [0.1.0] - 2021-02-28

### Added
- Drag and Drop with Rotate.
- Horizontal, Vertical, and Free ghost item movement.

[Unreleased]: https://github.com/PaigeM89/Elmish.DragAndDrop/compare/v0.3.0-alpha01...HEAD
[0.3.0-alpha01]: https://github.com/PaigeM89/Elmish.DragAndDrop/compare/v0.2.0...v0.3.0-alpha01
[0.2.0]: https://github.com/PaigeM89/Elmish.DragAndDrop/compare/v0.1.0...v0.2.0
[0.2.0-rc3]: https://github.com/PaigeM89/Elmish.DragAndDrop/compare/v0.1.0...v0.2.0-rc3
[0.2.0-rc2]: https://github.com/PaigeM89/Elmish.DragAndDrop/compare/v0.1.0...v0.2.0-rc2
[0.2.0-rc1]: https://github.com/PaigeM89/Elmish.DragAndDrop/compare/v0.1.0...v0.2.0-rc1
[0.1.0]: https://github.com/PaigeM89/Elmish.DragAndDrop/releases/tag/v0.1.0
