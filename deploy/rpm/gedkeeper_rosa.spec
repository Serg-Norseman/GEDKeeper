%define		  rname GEDKeeper
%define		  summary GEDKeeper - program for work with personal genealogical database

Name:		    gedkeeper
Version:	  2.12.0
Release:	  1
Summary:	  %{summary}
License:	  GPLv3
Group:		  Office
Url:		    https://github.com/serg-norseman/gedkeeper
Source0:	  https://github.com/serg-norseman/gedkeeper/archive/v%{version}.tar.gz?/%{rname}-%{version}.tar.gz
BuildArch:	noarch

BuildRequires:	pkgconfig(mono)
Requires:	      pkgconfig(lua)

%description
%{summary}.

%files
%doc .md LICENSE
%{_bindir}/gk2_run.sh
%{_libdir}/%{name}
%{_datadir}/mime/.xml
%{_datadir}/applications/%{name}.desktop
%{_datadir}/pixmaps/%{name}.png

%prep
%setup -qn %{rname}-%{version}

%build
xbuild ./projects/GEDKeeper2.linux.sln /p:Configuration=Debug /p:Platform="x86" /p:MonoCS=true

%install
install -Dm 0755 gk2_run.sh %{buildroot}%{_bindir}/gk2_run.sh
install -d 0755 %{buildroot}%{_libdir}/%{name}
install -Dm 0644 deploy/application-x-%{name}.xml %{buildroot}%{_datadir}/mime/application-x-%{name}.xml
install -Dm 0644 deploy/%{name}.desktop %{buildroot}%{_datadir}/applications/%{name}.desktop
install -Dm 0644 deploy/%{name}.png %{buildroot}%{_datadir}/pixmaps/%{name}.png
cp -r *.dll %{rname}2.exe \
	locales \
	plugins \
	samples \
	scripts %{buildroot}%{_libdir}/%{name}

## E: zero-length
rm -rf %{buildroot}%{_libdir}/%{name}/scripts/readme.txt

%changelog
