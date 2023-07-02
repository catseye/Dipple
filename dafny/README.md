dafny
=====

```
(see) https://learn.microsoft.com/en-gb/dotnet/core/install/linux-debian
wget https://packages.microsoft.com/config/debian/11/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
sudo apt-get install -y dotnet-sdk-6.0
export DOTNET_CLI_TELEMETRY_OPTOUT=1
nano ~/.bash_aliases
(add) export DOTNET_CLI_TELEMETRY_OPTOUT=1
(save and exit)
dotnet tool install --global dafny
logout
(new terminal)
dafny --help
wget https://github.com/Z3Prover/z3/archive/refs/tags/z3-4.12.1.tar.gz
tar zxvf z3<tab>
cd z3<tab>
(see) https://github.com/Z3Prover/z3#building-z3-using-make-and-gccclang
python3 scripts/mk_make.py
cd build
make
shelf_link .
cd $HERE
dafny trivial.dfy
```
