set -e

SCRIPTPATH=$( cd "$(dirname "$0")" ; pwd -P )

OS=$1

if [ -z $OS ]
then
  echo "Usage: build.sh osname"
  exit 1
fi

pushd ${SCRIPTPATH} > /dev/null

# Make the staging directory
mkdir -p build/psc-ide/

# Strip the binaries
strip ../dist/build/psc-ide/psc-ide
strip ../dist/build/psc-ide-server/psc-ide-server

# Copy files to staging directory
cp ../dist/build/psc-ide/psc-ide                 build/psc-ide/
cp ../dist/build/psc-ide-server/psc-ide-server   build/psc-ide/
cp ../README.md                                  build/psc-ide/
cp ../LICENSE                                    build/psc-ide/

# Make the binary bundle
pushd build > /dev/null
tar -zcvf ../$OS.tar.gz psc-ide
popd > /dev/null

# Calculate the SHA hash
shasum $OS.tar.gz > $OS.sha

# Remove the staging directory
rm -rf build/

popd > /dev/null
