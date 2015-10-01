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
## changes node name
FILE=${REL}/releases/${VSN}/vm.args
HOST=$(curl -s --connect-timeout 1 http://169.254.169.254/latest/meta-data/local-ipv4 || echo "127.0.0.1")
NODE=`sed -n -e "s/-name \(.*\)@.*/\1/p" ${FILE}`
sed -i -e "s/@\(127.0.0.1\)/@${HOST}/g" ${FILE}

##
## build service wrapper
if [ ! -a /etc/init.d/${APP} ] ; 
then
echo -e "#!/bin/bash\nexport HOME=/root\n${PREFIX}/${APP}/bin/${APP}.docker \$1" >  /etc/init.d/${APP}
chmod ugo+x /etc/init.d/${APP}
fi

##
## deploy config
test ! -d /etc/${APP} && mkdir -p /etc/${APP}
test ! -e /etc/${APP}/app.config && cp ${REL}/releases/${VSN}/sys.config /etc/${APP}/app.config
test ! -e /etc/${APP}/vm.args && cp ${REL}/releases/${VSN}/vm.args /etc/${APP}/vm.args

set +u
set +e

## EOF

