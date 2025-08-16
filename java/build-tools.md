# Java build and dependency management tools

## Gradle (seems to be preferred choice)

- newer
- builds and manages dependencies
- uses groovy not XML
- is a bit like rake except written in groovy not ruby
- it is a DSL for doing builds and managing dependencies
- provides convention over configuration like maven but is easier to extend
- can run as a daemon for faster builds
- can pull dependencies from maven and ivy servers
- includes a gui `gradle --gui`
- docs seem quite good

> Language-based build tools like Gradle and Rake continue to offer
> finer-grained abstractions and more flexibility long term than XML and plug-in
> based tools like Ant and Maven. This allows them to grow gracefully as
> projects become more complex.

## Maven

- XML based
- main config is in `pom.xml`
- builds and manages dependencies
- enforces a project structure
- provides the "central" maven repository which is the defacto equivalent of
  rubygems - see http://search.maven.org/

> Ant + ant-contrib is essentially a turing complete programming language that
> no one really wants to program in.
>
> Maven tries to take the opposite approach of trying to be completely
> declarative and forcing you to write and compile a plugin if you need logic.
> It also imposes a project model that is completely inflexible.
>
> Maven is very opinionated; it proposes that a project only contains one Java
> source directory and only produces one single JAR file. This is not
> necessarily reality for many enterprise projects.

## Ant (build) & Ivy (manage dependencies)

- XML based
- quite old
