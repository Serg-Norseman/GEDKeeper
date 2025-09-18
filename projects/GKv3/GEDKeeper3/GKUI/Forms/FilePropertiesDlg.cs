/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class FilePropertiesDlg : CommonDialog<IFilePropertiesDlg, FilePropertiesDlgController>, IFilePropertiesDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextBox txtName;
        private TextBox txtTel;
        private TextArea txtAddress;
        private TabPage pageOther;
        private GKListView lvRecordStats;
        private Button btnLangEdit;
        private ComboBox txtLanguage;
        private Label lblLanguage;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public IBaseWindow Base
        {
            get { return fController.Base; }
        }

        #region View Interface

        IListView IFilePropertiesDlg.RecordStats
        {
            get { return lvRecordStats; }
        }

        IComboBox IFilePropertiesDlg.Language
        {
            get { return GetControlHandler<IComboBox>(txtLanguage); }
        }

        ITextBox IFilePropertiesDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox IFilePropertiesDlg.Address
        {
            get { return  GetControlHandler<ITextBox>(txtAddress); }
        }

        ITextBox IFilePropertiesDlg.Tel
        {
            get { return  GetControlHandler<ITextBox>(txtTel); }
        }

        #endregion

        public FilePropertiesDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new FilePropertiesDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }
    }
}
