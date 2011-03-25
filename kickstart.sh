#!/bin/sh

dsplugins="http://svn.tracfort.jp/svn/dino-symfony/plugins"
plugins="sfConfigurePlugin sfLighttpdPlugin sfSampleDataGeneratorPlugin sfDinoStandardPlugin"

cd plugins
svn co ${dsplugins}/sfSubversionPlugin
cd ..

for i in $plugins;
do
    symfony svn-checkout-plugin ${dsplugins}/${i}
done

symfony init-bootstrap
symfony init-configure
symfony init-lighttpd

# fix-up
sed -e 's/^#//' < config/databases.yml.in >config/databases.yml.in.tmp
mv config/databases.yml.in.tmp config/databases.yml.in
cat >config/schema.yml <<EOF
---
propel:

EOF
./bootstrap
./configure






