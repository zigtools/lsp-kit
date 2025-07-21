[![CI](https://github.com/zigtools/lsp-kit/actions/workflows/main.yml/badge.svg)](https://github.com/zigtools/lsp-kit/actions)
[![codecov](https://codecov.io/gh/zigtools/lsp-kit/graph/badge.svg?token=C3HCN59E4C)](https://codecov.io/gh/zigtools/lsp-kit)
[![Documentation](https://badgen.net/badge/icon/Docs?icon=wiki&label)](https://zigtools.github.io/lsp-kit)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# Zig LSP Kit

Provides the necessary building blocks to develop Language Server Protocol implementations in Zig.

# Installation

> [!NOTE]
> The default branch requires Zig `0.15.0-dev.1160+e43617e68` or later. Checkout the `0.14.x` branch when using Zig 0.14

```bash
# Initialize a `zig build` project if you haven't already
zig init
# Add the `lsp_kit` package to your `build.zig.zon`
zig fetch --save git+https://github.com/zigtools/lsp-kit.git
```

You can then import the `lsp` module in your `build.zig` with:

```zig
const lsp = b.dependency("lsp_kit", .{}).module("lsp");
const exe = b.addExecutable(...);
exe.root_module.addImport("lsp", lsp);
```
