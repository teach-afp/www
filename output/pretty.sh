#!/bin/sh

cat $1 | sed 's/```\( \)*$/<\/pre>/g;s/```erlang\( \)*/<pre class="prettyprint lang-erlang">/g;s/```bash\( \)*/<pre class="prettyprint lang-bash">/g;s/```haskellln\( \)*/<pre class="prettyprint lang-haskell linenums">/g;s/```haskell\( \)*/<pre class="prettyprint lang-haskell">/g;s/```javaln\( \)*/<pre class="prettyprint lang-java linenums">/g;s/```java\( \)*/<pre class="prettyprint lang-java">/g;s/```yaml\( \)*/<pre class="prettyprint lang-yaml">/g;s/```cabal\( \)*/<pre class="prettyprint lang-yaml">/g' > temp

mv temp $2

# To disable these replacements:
# cp -f $1 $2
