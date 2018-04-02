# Change Log
All notable changes to the "vscode-java-debugger" extension will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 0.7.0 - 2018-3-15
### Added
- Support [conditional breakpoints](https://github.com/Microsoft/vscode-java-debug/issues/118). See [PR#153](https://github.com/Microsoft/java-debug/pull/153), [PR#154](https://github.com/Microsoft/java-debug/pull/154), [PR#156](https://github.com/Microsoft/java-debug/pull/156)
- Support prompting user for program arguments. See [PR#245](https://github.com/Microsoft/vscode-java-debug/pull/245)

### Changed
- Fix the unsupported breakpoint at method entry/exit issue. See [PR#129](https://github.com/Microsoft/java-debug/pull/129)
- Fix the issue when the projectName is not specified, the expression evaluation doesn't work. See [PR#156](https://github.com/Microsoft/java-debug/pull/156)
- Fix VMDisconnectionException in HCR. See [PR#150](https://github.com/Microsoft/java-debug/pull/150)


## 0.6.0 - 2018-2-1
### Added
- Support hot code replace. See [PR#225](https://github.com/Microsoft/vscode-java-debug/pull/225)

## 0.5.0 - 2017-12-20
### Added
- Support step filters when stepping. See [PR#155](https://github.com/Microsoft/vscode-java-debug/pull/155)
- Support expression evaluation. See [PR#126](https://github.com/Microsoft/vscode-java-debug/pull/126), [PR#131](https://github.com/Microsoft/java-debug/pull/131)
- Publish the binaries to the maven central repository. See [PR#132](https://github.com/Microsoft/java-debug/pull/132)

### Changed
- Adopt new VSCode 1.19.0 debug activation events. See [PR#196](https://github.com/Microsoft/vscode-java-debug/pull/196)
- Looking up the stack frame's associated source file from source containers to improve searching perf. See [PR#127](https://github.com/Microsoft/java-debug/pull/127)

## 0.4.0 - 2017-11-30
### Added
- Add `stopOnEntry` and `console` options for launch.json. See [PR#177](https://github.com/Microsoft/vscode-java-debug/pull/177)
- Support console input by launching the program in the integrated/external terminal. See [PR#122](https://github.com/Microsoft/java-debug/pull/122)
- Add debugging settings: `java.debug.settings.showHex`, `java.debug.settings.showStaticVariables`, `java.debug.settings.showQualifiedNames`, `java.debug.settings.maxStringLength`. See [README](https://github.com/Microsoft/vscode-java-debug/README.md) for details
- Support project scope when resolving multiple-root project. See [PR#174](https://github.com/Microsoft/vscode-java-debug/pull/174)

### Fixed
- Fix single file build issue. See [Issue#167](https://github.com/Microsoft/vscode-java-debug/issues/167)
- Fix perf issue when debugging with "stopOnEntry". See [PR#115](https://github.com/Microsoft/java-debug/pull/115)

## 0.3.1 - 2017-11-17
### Fixed
- Fix the unable to start debugging issue[Issue#146](https://github.com/Microsoft/vscode-java-debug/issues/146)

## 0.3.0 - 2017-11-10
### Added
- Support debugging java 9 project. See [Issue#47](https://github.com/Microsoft/vscode-java-debug/issues/47)
- Support debugging standalone java file. See [Issue#94](https://github.com/Microsoft/vscode-java-debug/issues/94)
- Support "cwd" and "env" in launch.json. See [Issue#12](https://github.com/Microsoft/vscode-java-debug/issues/12), [Issue#75](https://github.com/Microsoft/vscode-java-debug/issues/75)

### Changed
- Build workspace before starting debugger. See [Issue#32](https://github.com/Microsoft/vscode-java-debug/issues/32)
- Show progress when initializing the launch.json. See [PR#106](https://github.com/Microsoft/vscode-java-debug/pull/106)
- Get debug settings from VSCode user preferences. See [PR#135](https://github.com/Microsoft/vscode-java-debug/pull/135),[PR#94](https://github.com/Microsoft/java-debug/pull/94)

### Fixed
- Fix perf issue on getting locations of breakpoint. See [Issue#49](https://github.com/Microsoft/java-debug/issues/49)
- Show warning message when the debugger and the debuggee run in the different versions of JVMs. See [Issue#30](https://github.com/Microsoft/vscode-java-debug/issues/30)

## 0.2.0 - 2017-10-20
### Added
- Automatically resolve the main class during launching. See [Issue#9](https://github.com/Microsoft/vscode-java-debug/issues/9)
- Fully support external source files together with the changes from VSCode. See [PR#58](https://github.com/Microsoft/java-debug/pull/58)

### Changed
- Adopt the new DebugConfigurationProvider protocol of VS Code. See [PR#87](https://github.com/Microsoft/vscode-java-debug/pull/87)
- Display the function names in the format of ClassName.MethodName(Parameter List...).. See [PR#73](https://github.com/Microsoft/java-debug/pull/73)
- Improve the call stack display info for the files without sources. See [PR#72](https://github.com/Microsoft/java-debug/pull/72)

### Fixed
- Fix the inconsistent URI issue when set breakpoint request. See [PR#84](https://github.com/Microsoft/java-debug/pull/84)
- Avoid two stopped events for step and breakpoint. See [Issue#14](https://github.com/Microsoft/vscode-java-debug/issues/14)
- Fix the issue that JDT search might return multiple results from the same project. See [Issue#21](https://github.com/Microsoft/java-debug/issues/21)
- Avoid send error messages after debugger adapter stopped. See [PR#75](https://github.com/Microsoft/java-debug/pull/75)
- Fix several exception cases. See [PR#64](https://github.com/Microsoft/java-debug/pull/62), [PR#67](https://github.com/Microsoft/java-debug/pull/67), [PR#74](https://github.com/Microsoft/java-debug/pull/74)

## 0.1.0 - 2017-09-27
### Added

- Launch/Attach
- Breakpoints
- Exceptions
- Pause & Continue
- Step In/Out/Over
- Variables
- Callstacks
- Threads
- Debug console
