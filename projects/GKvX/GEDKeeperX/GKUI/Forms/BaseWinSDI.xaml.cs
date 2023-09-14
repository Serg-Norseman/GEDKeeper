/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using GDModel;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class BaseWinSDI : TabbedPage
    {
        public BaseWinSDI()
        {
            InitializeComponent();

            CreatePage("Individuals", GDMRecordType.rtIndividual);
            CreatePage("Families", GDMRecordType.rtFamily);
            CreatePage("Notes", GDMRecordType.rtNote);
            CreatePage("Multimedia", GDMRecordType.rtMultimedia);
            CreatePage("Sources", GDMRecordType.rtSource);
            CreatePage("Repositories", GDMRecordType.rtRepository);
            CreatePage("Groups", GDMRecordType.rtGroup);
            CreatePage("Researches", GDMRecordType.rtResearch);
            CreatePage("Tasks", GDMRecordType.rtTask);
            CreatePage("Communications", GDMRecordType.rtCommunication);
            CreatePage("Locations", GDMRecordType.rtLocation);
        }

        private void CreatePage(string pageText, GDMRecordType recType)
        {
            var tabPage = new ContentPage();
            tabPage.Title = pageText;
            Children.Add(tabPage);
        }
    }
}
