# Erlang/OTP environment

The tool development required Erlang/OTP environment 17.3 or later.

See following documentation on the development environment setup.
* http://www.erlang.org/doc/ 
* http://www.erlang.org/doc/installation_guide/INSTALL.html

The project uses Makefile (see https://github.com/fogfish/makefile) to build project and assemble deployment packages.

## Build Erlang/OTP from sources

download sources
```
   curl -O http://www.erlang.org/download/otp_src_${VSN}.tar.gz
```

configure
```
cd /tmp/otp_src_${VSN}
./configure \
   --prefix=/usr/local/otp_${VSN} \
   --enable-threads \
   --enable-smp-support \
   --enable-kernel-poll \
   --enable-hipe \
   --enable-native-libs \
   --disable-dynamic-ssl-lib \
   --with-ssl=/usr \
   CFLAGS="-DOPENSSL_NO_EC=1"
```

build
```
make
make install
ln -s /usr/local/otp_${VSN} /usr/local/otp
```


  

