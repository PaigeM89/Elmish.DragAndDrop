# Elmish.DragAndDrop

## How To Use

1. At the scope you'll define the `DragDropContext` (see next step), add a `DragAndDropModel` to your model, and a `DragAndDropMsg` to your messages.

```fsharp
    open Elmish.DragAndDrop

    type Model = {
      DragAndDrop : DragAndDropModel
      ContentMap : Map<ContentKey, ContentValue>
    }

    type Msg =
    | DndMsg of DragAndDropMsg
    | InputChange of elementId : string * newValue : string
```

You will need a way to determine which content to render based on which element Id is given. It is recommended to use a map for this, storing your content in an easily accessible (and easily modifiable) way.

2. Create a `DragDropContext` at the scope you want the user to be able to drag things. This does not have to be the scope they can drop things; for usability, consider putting this context towards the root of your application.

```fsharp
    let mappedMsg msg = DndMsg msg

    DragDropContext.context
      model.DragAndDrop
      (mappedMsg >> dispatch) 
      div [] [ dropAreaContent ]
```

3. Create your `Draggables`. These consist of 2 parts: (1) the `Draggable` itself, the element that will move around the screen and be moved to different locations in your collection(s); (2) the `DragHandle`, the part of the element your user will click on to do these actions. In many cases, the entire `Draggable` will be a drag handle, but if your elements contain things the user may want to interact with, such as an input box or a checkbox, then you'll want to use define a specific `DragHandle`.

The `Draggable` will need the `DragHandle` to be part of the elements given to it, or it can be built as a `DragHandle` (making the whole element a drag handle). If no drag handle is provided, the element will not be draggable, though it will still be movable based on other items moving it out of the way to accomodate it dropping. A `DragHandle` must have a reference to the `id` of the `Draggable`. 

Additionally, you'll want to configure your `DragAndDropConfig` at the scope of any specific drag & drop, to define additional styles or properties to put on your various drag & drop elements.

```fsharp
      let dragAndDropConfig = {
          DragAndDropConfig.Empty() with
              DraggedElementStyles = Some [
                  MarginLeft -130.
                  MarginTop -50.
                  Opacity 0.8
                  Position PositionOptions.Fixed
                  Cursor "grabbing"
                  Background "#00ffff"
              ]
              HoverPreviewElementStyles = Some [
                  Opacity 0.2
              ]
        }

    let createDragHandle dndModel draggableId dispatch handleContent : ReactElement =
        let handleId = draggableId + "-handle"
        ElementGenerator.Create handleId [] [] handleContent
        |> DragHandle.dragHandle dndModel draggableId (mappedMsg >> dispatch)

    let createDraggable dndModel draggableId dispatch handleContent content =
        let handle = createDragHandle dndModel draggableId dispatch handleContent
        ElementGenerator.Create draggableId [] [] [ handle; content ]
        |> Draggable.draggable dndModel dragAndDropConfig (mappedMsg >> dispatch)
```

4. Define one or more `DropArea` elements for your `Draggables` to live. 

```fsharp
    let draggables 
      let handleContent = p [] [ "Click here to drag!" ]
      model.DragAndDrop.ElementIds()
      |> List.concat // collect into a single list; this is just for example, you may need to keep distinct lists
      |> List.map (fun rootElementId ->
        // find the content based on the Id given by drag and drop. In this example, this returns pure content
        // In most scenarios, you'll store some object, and map that to content at this step.
        let content = model.ContentMap |> Map.find rootElementId
        createDraggable dndModel rootelementId dispatch handleContent content
      )
    let dropArea = DropArea.fromDraggables div [] draggables
```

With all that, you're good to go! There's a fair bit of setup, which arises from the amount of configurability this library aims to achieve.

See the Examples folder for complete, working examples of different types of setups.


### Element Generators

This is a building block used to specify all the ingredients of a `ReactElement`, so that additional styles or properties can be added as needed before it is turned into an element.

## Contributions & Todos

Heavily inspired by / shamelessly copied off of [dnd-list for Elm](https://annaghi.github.io/dnd-list/introduction/basic), this implements drag-and-drop sorting functionality for the Elmish architecture in Fable.

Features in progress/TODO:
* Sliding animations
* Full documentation
* More examples
  * Horizontal demo
  * Grid demo
  * Sliding demo
* Allow CSS classes to be added to generated content
* Allow a callback to be used to filter eligible drop areas or drop locations
* Fix a bug with multi-list inserting where inserting to the bottom of a list requires inserting into the middle first. This will probably require a ghost last element to hover over, which should disappear. If this approach is used, that element will need to dynamically take up the rest of the space in that list.
* Fix bug with flickering on large items as they slide back & forth from a spot
* Use location finding & offset calculations to place a dragged item under the cursor at the spot it was clicked; right now, all dragged items appear in the same spot under the cursor, regardless of where the user clicked on that item.
* Fix bug where "grabbing" cursor never appears, though "grab" does.

**Contributions Welcome**

I don't currently have the time to fully implement all the features that may be needed.

---

## Builds

GitHub Actions |
:---: |
[![GitHub Actions](https://github.com/PaigeM89/Elmish.DragAndDrop/workflows/Build%20master/badge.svg)](https://github.com/PaigeM89/Elmish.DragAndDrop/actions?query=branch%3Amaster) |
[![Build History](https://buildstats.info/github/chart/PaigeM89/Elmish.DragAndDrop)](https://github.com/PaigeM89/Elmish.DragAndDrop/actions?query=branch%3Amaster) |

## NuGet 

Package | Stable | Prerelease
--- | --- | ---
Elmish.DragAndDrop | [![NuGet Badge](https://buildstats.info/nuget/Elmish.DragAndDrop)](https://www.nuget.org/packages/Elmish.DragAndDrop/) | [![NuGet Badge](https://buildstats.info/nuget/Elmish.DragAndDrop?includePreReleases=true)](https://www.nuget.org/packages/Elmish.DragAndDrop/)

---

### Developing

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

or

- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)


---

### Environment Variables

- `CONFIGURATION` will set the [configuration](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options) of the dotnet commands.  If not set, it will default to Release.
  - `CONFIGURATION=Debug ./build.sh` will result in `-c` additions to commands such as in `dotnet build -c Debug`
- `GITHUB_TOKEN` will be used to upload release notes and Nuget packages to GitHub.
  - Be sure to set this before releasing
- `DISABLE_COVERAGE` Will disable running code coverage metrics.  AltCover can have [severe performance degradation](https://github.com/SteveGilham/altcover/issues/57) so it's worth disabling when looking to do a quicker feedback loop.
  - `DISABLE_COVERAGE=1 ./build.sh`


---

### Building


```sh
> build.cmd <optional buildtarget> // on windows
$ ./build.sh  <optional buildtarget>// on unix
```

The bin of your library should look similar to:

```
$ tree src/MyCoolNewLib/bin/
src/MyCoolNewLib/bin/
└── Debug
    └── net50
        ├── MyCoolNewLib.deps.json
        ├── MyCoolNewLib.dll
        ├── MyCoolNewLib.pdb
        └── MyCoolNewLib.xml

```

---

### Build Targets

- `Clean` - Cleans artifact and temp directories.
- `DotnetRestore` - Runs [dotnet restore](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-restore?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- [`DotnetBuild`](#Building) - Runs [dotnet build](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `DotnetTest` - Runs [dotnet test](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-test?tabs=netcore21) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `GenerateCoverageReport` - Code coverage is run during `DotnetTest` and this generates a report via [ReportGenerator](https://github.com/danielpalme/ReportGenerator).
- `WatchTests` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) with the test projects. Useful for rapid feedback loops.
- `GenerateAssemblyInfo` - Generates [AssemblyInfo](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.applicationservices.assemblyinfo?view=netframework-4.8) for libraries.
- `DotnetPack` - Runs [dotnet pack](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-pack). This includes running [Source Link](https://github.com/dotnet/sourcelink).
- `SourceLinkTest` - Runs a Source Link test tool to verify Source Links were properly generated.
- `PublishToNuGet` - Publishes the NuGet packages generated in `DotnetPack` to NuGet via [paket push](https://fsprojects.github.io/Paket/paket-push.html).
- `GitRelease` - Creates a commit message with the [Release Notes](https://fake.build/apidocs/v5/fake-core-releasenotes.html) and a git tag via the version in the `Release Notes`.
- `GitHubRelease` - Publishes a [GitHub Release](https://help.github.com/en/articles/creating-releases) with the Release Notes and any NuGet packages.
- `FormatCode` - Runs [Fantomas](https://github.com/fsprojects/fantomas) on the solution file.
- `BuildDocs` - Generates Documentation from `docsSrc` and the [XML Documentation Comments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/xmldoc/) from your libraries in `src`.
- `WatchDocs` - Generates documentation and starts a webserver locally.  It will rebuild and hot reload if it detects any changes made to `docsSrc` files, libraries in `src`, or the `docsTool` itself.
- `ReleaseDocs` - Will stage, commit, and push docs generated in the `BuildDocs` target.
- [`Release`](#Releasing) - Task that runs all release type tasks such as `PublishToNuGet`, `GitRelease`, `ReleaseDocs`, and `GitHubRelease`. Make sure to read [Releasing](#Releasing) to setup your environment correctly for releases.
---


### Releasing

- [Start a git repo with a remote](https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/)

```sh
git add .
git commit -m "Scaffold"
git remote add origin https://github.com/user/MyCoolNewLib.git
git push -u origin master
```

- [Create your NuGeT API key](https://docs.microsoft.com/en-us/nuget/nuget-org/publish-a-package#create-api-keys)
    - [Add your NuGet API key to paket](https://fsprojects.github.io/Paket/paket-config.html#Adding-a-NuGet-API-key)

    ```sh
    paket config add-token "https://www.nuget.org" 4003d786-cc37-4004-bfdf-c4f3e8ef9b3a
    ```

    - or set the environment variable `NUGET_TOKEN` to your key


- [Create a GitHub OAuth Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
  - You can then set the environment variable `GITHUB_TOKEN` to upload release notes and artifacts to github
  - Otherwise it will fallback to username/password

- Then update the `CHANGELOG.md` with an "Unreleased" section containing release notes for this version, in [KeepAChangelog](https://keepachangelog.com/en/1.1.0/) format.

NOTE: Its highly recommend to add a link to the Pull Request next to the release note that it affects. The reason for this is when the `RELEASE` target is run, it will add these new notes into the body of git commit. GitHub will notice the links and will update the Pull Request with what commit referenced it saying ["added a commit that referenced this pull request"](https://github.com/TheAngryByrd/MiniScaffold/pull/179#ref-commit-837ad59). Since the build script automates the commit message, it will say "Bump Version to x.y.z". The benefit of this is when users goto a Pull Request, it will be clear when and which version those code changes released. Also when reading the `CHANGELOG`, if someone is curious about how or why those changes were made, they can easily discover the work and discussions.

Here's an example of adding an "Unreleased" section to a `CHANGELOG.md` with a `0.1.0` section already released.

```markdown
## [Unreleased]

### Added
- Does cool stuff!

### Fixed
- Fixes that silly oversight

## [0.1.0] - 2017-03-17
First release

### Added
- This release already has lots of features

[Unreleased]: https://github.com/user/MyCoolNewLib.git/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/user/MyCoolNewLib.git/releases/tag/v0.1.0
```

- You can then use the `Release` target, specifying the version number either in the `RELEASE_VERSION` environment
  variable, or else as a parameter after the target name.  This will:
  - update `CHANGELOG.md`, moving changes from the `Unreleased` section into a new `0.2.0` section
    - if there were any prerelease versions of 0.2.0 in the changelog, it will also collect their changes into the final 0.2.0 entry
  - make a commit bumping the version:  `Bump version to 0.2.0` and adds the new changelog section to the commit's body
  - publish the package to NuGet
  - push a git tag
  - create a GitHub release for that git tag

macOS/Linux Parameter:

```sh
./build.sh Release 0.2.0
```

macOS/Linux Environment Variable:

```sh
RELEASE_VERSION=0.2.0 ./build.sh Release
```


