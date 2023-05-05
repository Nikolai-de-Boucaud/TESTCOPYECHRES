Name: EFITViewer
Version: 1.0
Release: 1
Copyright: GNU GPL
Group: Applications/Visualization
Source: ftp://lithos.gat.com/pub/peng/EFITViewer-1.0.tar.gz
Buildroot: /tmp
Vendor: General Atomics, Data Analysis Applications Group
Summary: EFIT data viewing tool
AutoReqProv: no
prefix: /usr/local
PreReq: /bin/csh 

%description
EFITViewer IDL tool used to visualize EFIT data.

%prep
%setup


%build

%install
mkdir -p $RPM_BUILD_ROOT/usr/local/efitview
cp * $RPM_BUILD_ROOT/usr/local/efitview
    

%pre
if [ ! -x /usr/local/bin/idl ]; then
  echo "Could not find /usr/local/bin/idl"
  echo "Need IDL from Research Systems, Inc. in order to run IDL."
  exit 1
fi

%post

file=$RPM_INSTALL_PREFIX0/efitview/efitviewer

if [ -x /bin/awk ]; then
  tmpfile=$file.tmp
  awk -F" " '/^setenv IDLSOURCE/ {print $1 " " $2 " " "'$RPM_INSTALL_PREFIX0'" } ; ! /^setenv IDLSOURCE/' $file > $tmpfile
  mv $tmpfile $file  
  chmod a+rx $file 
else
  echo Please edit $file to set the environment variable IDLSOURCE to point to 
  echo the directory in which EFITViewer was installed: $RPM_INSTALL_PREFIX0
fi

if [ -x /bin/touch ]; then
  logfile=$RPM_INSTALL_PREFIX0/efitview/efitviewer.log 
  touch $logfile
  chmod 666 $logfile
else
  echo "Create $RPM_INSTALL_PREFIX0/efitview/efitviewer.log"
  echo "and make it group writable."
fi

%clean
rm -rf *
rm -rf $RPM_BUILD_ROOT/usr/local/efitview

%files
/usr/local/efitview


