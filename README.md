# Kampong

Kampong is a Haskell library for building federated server applications powered by ActivityPub and other standards, commonly known as the fediverse. It aims to eliminate the complexity and redundant boilerplate code when building a federated server application, allowing you to focus on your business logic and user experience.

## Features

* Type-safe objects for Activity Vocabulary (including vendor-specific extensions)
* WebFinger client and server implementation
* HTTP Signatures support
* Object Integrity Proofs & Linked Data Signatures
* Middleware for handling webhooks
* NodeInfo protocol support
* Special interoperability features for Mastodon and other popular fediverse software
* Integration with various Haskell web frameworks
* CLI toolchain for testing and debugging

## Installation

Add Kampong to your project's dependencies in your `package.yaml`:

```yaml
dependencies:
  - kampong
```

Or using stack:

```bash
stack install kampong
```

## Quick Start

Here's a simple example of creating a basic ActivityPub server:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Kampong
import Kampong.WebFinger
import Kampong.ActivityPub

main :: IO ()
main = do
  -- Initialize your server configuration
  config <- defaultConfig
  
  -- Start the server
  runServer config
```

## Documentation

For detailed documentation, please visit our [documentation site](https://kampong.dev/docs).

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for more details.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

This project is inspired by [Fedify](https://jsr.io/@fedify/fedify), a TypeScript-based ActivityPub server framework.

## Contact

- GitHub Issues: [Report bugs or request features](https://github.com/yourusername/kampong/issues)
- Discussions: [Join our community discussions](https://github.com/yourusername/kampong/discussions)

## Status

This project is currently in early development. We're actively working on implementing core features and welcome feedback from the community.
