# Debugger for Java

[![Gitter](https://badges.gitter.im/Microsoft/vscode-java-debug.svg)](https://gitter.im/Microsoft/vscode-java-debug)
[![Travis CI](https://travis-ci.org/Microsoft/vscode-java-debug.svg?branch=master)](https://travis-ci.org/Microsoft/vscode-java-debug)

## Overview
A lightweight Java Debugger based on [Java Debug Server](https://github.com/Microsoft/java-debug) which extends the [Language Support for Java by Red Hat](https://marketplace.visualstudio.com/items?itemName=redhat.java). It allows users debugging Java code using Visual Studio Code (VS Code). Here's a list of features:

- Launch/Attach
- Breakpoints
- Exceptions
- Pause & Continue
- Step In/Out/Over
- Variables
- Callstacks
- Threads
- Debug console

## Requirements
- JDK (version 1.8.0 or later)
- VS Code (version 1.19.0 or later)
- [Language Support for Java by Red Hat](https://marketplace.visualstudio.com/items?itemName=redhat.java) (version 0.14.0 or later)

## Install

Open VS Code and press `F1` or `Ctrl + Shift + P` to open command palette, select **Install Extension** and type `vscode-java-debug`.

Or launch VS Code Quick Open (`Ctrl + P`), paste the following command, and press enter.
```bash
ext install vscode-java-debug
```

## Use

- Launch VS Code
- Open a Java project (Maven/Gradle/Eclipse)
- Open a Java file to activate the extensions
- Add debug configurations and edit launch.json
    - To launch: specify `mainClass`
    - To attach: specify `hostName` and `port`
- Press F5

Please also check the documentation of [Language Support for Java by Red Hat](https://marketplace.visualstudio.com/items?itemName=redhat.java) if you have trouble setting up your project.

## Options

### Launch

- `mainClass` (required) - The main class of the program (fully qualified name, e.g. [mymodule/]com.xyz.MainClass).
- `args` - The command line arguments passed to the program. Use `"${command:SpecifyProgramArgs}"` to prompt for program arguments.
- `sourcePaths` - The extra source directories of the program. The debugger looks for source code from project settings by default. This option allows the debugger to look for source code in extra directories.
- `modulePaths` - The modulepaths for launching the JVM. If not specified, the debugger will automatically resolve from current project.
- `classPaths` - The classpaths for launching the JVM. If not specified, the debugger will automatically resolve from current project.
- `encoding` - The `file.encoding` setting for the JVM. If not specified, 'UTF-8' will be used. Possible values can be found in http://docs.oracle.com/javase/8/docs/technotes/guides/intl/encoding.doc.html.
- `vmArgs` - The extra options and system properties for the JVM (e.g. -Xms\<size\> -Xmx\<size\> -D\<name\>=\<value\>).
- `projectName` - The preferred project in which the debugger searches for classes. There could be duplicated class names in different projects. This setting also works when the debugger looks for the specified main class when launching a program. It is required when the workspace has multiple java projects, otherwise the expression evaluation and conditional breakpoint may not work.
- `cwd` - The working directory of the program.
- `env` - The extra environment variables for the program.
- `stopOnEntry` - Automatically pause the program after launching.
- `console` - The specified console to launch the program. Defaults to `internalConsole`.
  - `internalConsole` - VS Code debug console (input stream not supported).
  - `integratedTerminal` - VS Code integrated terminal.
  - `externalTerminal` - External terminal that can be configured in user settings.
- `stepFilters` - Skip specified classes or methods when stepping.
  - `classNameFilters` - Skip the specified classes when stepping. Class names should be fully qualified. Wildcard is supported.
  - `skipSynthetics` - Skip synthetic methods when stepping.
  - `skipStaticInitializers` - Skip static initializer methods when stepping.
  - `skipConstructors` - Skip constructor methods when stepping.

### Attach

- `hostName` (required) - The host name or IP address of remote debuggee.
- `port` (required) - The debug port of remote debuggee.
- `timeout` - Timeout value before reconnecting, in milliseconds (default to 30000ms).
- `sourcePaths` - The extra source directories of the program. The debugger looks for source code from project settings by default. This option allows the debugger to look for source code in extra directories.
- `projectName` - The preferred project in which the debugger searches for classes. There could be duplicated class names in different projects. It is required when the workspace has multiple java projects, otherwise the expression evaluation and conditional breakpoint may not work.
- `stepFilters` - Skip specified classes or methods when stepping.
  - `classNameFilters` - Skip the specified classes when stepping. Class names should be fully qualified. Wildcard is supported.
  - `skipSynthetics` - Skip synthetic methods when stepping.
  - `skipStaticInitializers` - Skip static initializer methods when stepping.
  - `skipConstructors` - Skip constructor methods when stepping.

### User Settings

- `java.debug.logLevel`: minimum level of debugger logs that are sent to VS Code, defaults to `warn`.
- `java.debug.settings.showHex`: show numbers in hex format in "Variables" viewlet, defaults to `false`.
- `java.debug.settings.showStaticVariables`: show static variables in "Variables" viewlet, defaults to `true`.
- `java.debug.settings.showQualifiedNames`: show fully qualified class names in "Variables" viewlet, defaults to `false`.
- `java.debug.settings.maxStringLength`: the maximum length of string displayed in "Variables" or "Debug Console" viewlet, the string longer than this length will be trimmed, defaults to `0` which means no trim is performed.
- `java.debug.settings.enableHotCodeReplace`: enable hot code replace for Java code. Make sure the auto build is not disabled for [VSCode Java](https://github.com/redhat-developer/vscode-java). See the [wiki page](https://github.com/Microsoft/vscode-java-debug/wiki/Hot-Code-Replace) for more information about usages and limitations.

## Feedback and Questions
You can find the full list of issues at [Issue Tracker](https://github.com/Microsoft/vscode-java-debug/issues). You can submit a [bug or feature suggestion](https://github.com/Microsoft/vscode-java-debug/issues/new), and participate community driven [![Gitter](https://badges.gitter.im/Microsoft/vscode-java-debug.svg)](https://gitter.im/Microsoft/vscode-java-debug)

## License
This extension is licensed under [MIT License](https://github.com/Microsoft/vscode-arduino/blob/master/LICENSE.txt).

## Data/Telemetry
VS Code collects usage data and sends it to Microsoft to help improve our products and services. Read our [privacy statement](https://go.microsoft.com/fwlink/?LinkID=528096&clcid=0x409) to learn more. If you don't wish to send usage data to Microsoft, you can set the `telemetry.enableTelemetry` setting to `false`. Learn more in our [FAQ](https://code.visualstudio.com/docs/supporting/faq#_how-to-disable-telemetry-reporting).
