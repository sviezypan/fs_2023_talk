# Fucntional Scala 2023

## Harnessing The Power Of ZIO Schema: Beyond The Limits Of Common Techniques
This repo contains code examples from my Functional Scala 2023 talk 'Harnessing The Power Of ZIO Schema: Beyond The Limits Of Common Techniques'.

It's divided into 5 classes
- MySchema.scala : describes simplest possible standalone schema of a case class with 2 fields.
- MigrationExample.scala : sample of how we could generate sql migration scripts from evolving case classes using ZIO Schema's description of those case classes, `Migrations`  and schema's capabilities around default values.
- InsertExample.scala : contains simple standalone `Insert` module that uses ZIO Schema to verify correctness of DSL query at compile time.
- RenderInsert.scala : renders `Insert` data type into SQL query using ZIO Schema dynamic values.
- ClientProjectExamples.scala : showcases how we can start using ZIO Schema at our daily jobs right away and benefit from number of features that schema provides without any metaprogramming / macros drawbacks:
    - Typeclass derivation e.g. for ZIO Test's `Gen[R, A]`` data type
    - Ordering
    - Validation
    - Json Serialization
    - Optics