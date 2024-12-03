%define	        rname GEDKeeper
%define	        summary GEDKeeper - program for work with personal genealogical database

Name:		gedkeeper
Version:	3.8.0
Release:	1
Summary:	%{summary}
License:	GPLv3
Group:		Applications/Editors
Url:		https://github.com/serg-norseman/gedkeeper
Source:		%{name}-%{version}.tar.gz
BuildArch:	x86_64

Requires:	dotnet-runtime-6.0
Requires:	sqlite

AutoReq:	no
AutoReqProv:	no

%description
%{summary}.

%files
%doc LICENSE
%{_bindir}/gk_run.sh
%{_libdir}/%{name}
%{_datadir}/mime/*.xml
%{_datadir}/applications/%{name}.desktop
%{_datadir}/pixmaps/%{name}.png

%prep
%setup -qc

%install
install -Dm 0755 gk_run.sh %{buildroot}%{_bindir}/gk_run.sh
install -d 0755 %{buildroot}%{_libdir}/%{name}
install -Dm 0644 application-x-%{name}.xml %{buildroot}%{_datadir}/mime/application-x-%{name}.xml
install -Dm 0644 %{name}.desktop %{buildroot}%{_datadir}/applications/%{name}.desktop
install -Dm 0644 %{name}.png %{buildroot}%{_datadir}/pixmaps/%{name}.png
cp -r bin \
	locales \
	plugins \
	samples \
	scripts %{buildroot}%{_libdir}/%{name}

## E: zero-length
#rm -rf %{buildroot}%{_libdir}/%{name}/scripts/readme.txt

%changelog
* Dec 14 2024 GEDKeeper - 3.8.0
- New upstream release
