%define		summary GEDKeeper - program for work with personal genealogical database.

Name:		gedkeeper
Version:	3.8.0
Release:	1%{?dist}
Summary:	%{summary}
License:	GPLv3
Group:		Applications/Editors
Url:		https://github.com/serg-norseman/gedkeeper
BuildArch:	x86_64

AutoReqProv:no
AutoReq:	no
Requires:	dotnet-runtime-6.0
Requires:	sqlite

%install

# main install
mkdir -p %{buildroot}%{_libdir}/%{name}
cp -r bin \
	locales \
	plugins \
	samples \
	scripts %{buildroot}%{_libdir}/%{name}

# clean multi-arch builds (TODO: fix me)
ls -la %{buildroot}%{_libdir}/%{name}/plugins/runtimes/
rm -rf %{buildroot}%{_libdir}/%{name}/plugins/runtimes
install -t %{buildroot}%{_libdir}/%{name}/plugins/runtimes/linux-x64/native/ -D plugins/runtimes/linux-x64/native/*
ls -la %{buildroot}%{_libdir}/%{name}/plugins/runtimes/

# symlink to binary file
mkdir -p %{buildroot}%{_bindir}
ln -fs %{_libdir}/%{name}/bin/GEDKeeper3 %{buildroot}%{_bindir}/%{name}

#rpm --eval %{_metainfodir}	# not found (TODO: check rpm version, make metainfo)
#install -D deploy/%{name}.metainfo.xml %{buildroot}%{_metainfodir}/%{name}.metainfo.xml
#install -D deploy/%{name}.metainfo.xml %{buildroot}%{_datadir}/metainfo/%{name}.metainfo.xml
install -D deploy/application-x-%{name}.xml %{buildroot}%{_datadir}/mime/packages/%{name}.xml
install -D deploy/rpm/%{name}.desktop %{buildroot}%{_datadir}/applications/%{name}.desktop
install -D deploy/%{name}.png %{buildroot}%{_datadir}/pixmaps/%{name}.png

cd %{buildroot}
chmod -Rf a-x,a+rX,u+w,g-w,o-w .
chmod a+x %{buildroot}%{_libdir}/%{name}/bin/GEDKeeper3

%files
%license LICENSE
%{_bindir}/%{name}
%{_libdir}/%{name}
%{_datadir}/mime/packages/%{name}.xml
%{_datadir}/applications/%{name}.desktop
%{_datadir}/pixmaps/%{name}.png

%description
%{summary}

%changelog
* Dec 14 2024 GEDKeeper - 3.8.0
- New upstream release
