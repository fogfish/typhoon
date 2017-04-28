##
##   Copyright 2015 Zalando SE
##
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##
##       http://www.apache.org/licenses/LICENSE-2.0
##
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
##
## @doc
##    node deployment script
##       ${PREFIX} - root installation folder
##       ${REL}    - absolute path to release
##       ${APP}    - application name
##       ${VSN}    - application version
set -u
set -e

##
## discover version of erlang release
VERSION=`cat ${REL}/releases/start_erl.data`
SYS_VSN=${VERSION% *}
APP_VSN=${VERSION#* }


##
## make alias to current version
rm -f ${PREFIX}/${APP}
ln -s ${PREFIX}/${APP}-${VSN} ${PREFIX}/${APP}


##
## build service wrapper
if [[ $(uname -s) == "Linux" ]] ;
then

cat > /etc/init.d/${APP} <<- EOF
#!/bin/sh
export HOME=/root

FILE=${REL}/releases/${APP_VSN}/vm.args
HOST=\$(curl -s --connect-timeout 5 http://169.254.169.254/latest/meta-data/local-ipv4)
if [ -z "\${HOST}" ] ;
then
HOST=\$({ ip addr show eth0 | sed -n 's/.*inet \([0-9]*.[0-9]*.[0-9]*.[0-9]*\).*/\1/p' ; } || echo "127.0.0.1")
fi

NODE=\$(sed -n -e "s/-name \(.*\)@.*/\1/p" \${FILE})
sed -i -e "s/@\(127.0.0.1\)/@\${HOST}/g" \${FILE}

${PREFIX}/${APP}/bin/${APP} \$1
EOF
   
chmod ugo+x /etc/init.d/${APP}
cp ${PREFIX}/${APP}/lib/typhoon-*/priv/tyrl /usr/bin/tyrl
cp ${PREFIX}/${APP}/lib/typhoon-*/priv/smokeit /usr/bin/smokeit

fi

set +u
set +e

## EOF

