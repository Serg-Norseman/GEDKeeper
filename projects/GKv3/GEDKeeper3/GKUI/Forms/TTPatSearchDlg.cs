/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTPatSearchDlg : CommonDialog, IPatriarchsSearchDlg
    {
        #region Design components

        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pagePatSearch;
        private Button btnPatSearch;
        private Panel panPatriarchsContainer;
        private Label lblMinGenerations;
        private NumericUpDown edMinGens;
        private Button btnSetPatriarch;
        private Button btnPatriarchsDiagram;
        private CheckBox chkWithoutDates;

        #endregion

        private readonly PatriarchsSearchController fController;

        private GKListView ListPatriarchs;

        #region View Interface

        INumericBox IPatriarchsSearchDlg.MinGensNum
        {
            get { return GetControlHandler<INumericBox>(edMinGens); }
        }

        ICheckBox IPatriarchsSearchDlg.WithoutDatesCheck
        {
            get { return GetControlHandler<ICheckBox>(chkWithoutDates); }
        }

        IListView IPatriarchsSearchDlg.PatriarchsList
        {
            get { return ListPatriarchs; }
        }

        #endregion

        public TTPatSearchDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new PatriarchsSearchController(this);
            fController.Init(baseWin);

            ListPatriarchs = new GKListView();
            ListPatriarchs.MouseDoubleClick += ListPatriarchs_DblClick;
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Patriarch), 400, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Birth), 90, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Descendants), 90, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Generations), 90, false);
            panPatriarchsContainer.Content = ListPatriarchs;

            SetLocale();
        }

        public void SetLocale()
        {
            Title = LangMan.LS(LSID.LSID_ToolOp_8);
            pagePatSearch.Text = LangMan.LS(LSID.LSID_ToolOp_8);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            lblMinGenerations.Text = LangMan.LS(LSID.LSID_MinGenerations);
            btnSetPatriarch.Text = LangMan.LS(LSID.LSID_SetPatFlag);
            btnPatSearch.Text = LangMan.LS(LSID.LSID_Search);
            chkWithoutDates.Text = LangMan.LS(LSID.LSID_WithoutDates);
            btnPatriarchsDiagram.Text = LangMan.LS(LSID.LSID_PatriarchsDiagram);
        }

        private void ListPatriarchs_DblClick(object sender, EventArgs e)
        {
            fController.SelectPatriarch();
        }

        private void btnPatSearch_Click(object sender, EventArgs e)
        {
            fController.Search();
        }

        private void btnSetPatriarch_Click(object sender, EventArgs e)
        {
            fController.SetPatriarch();
        }

        private void btnPatriarchsDiagram_Click(object sender, EventArgs e)
        {
            fController.ShowPatriarchsDiagram();
        }
    }
}
