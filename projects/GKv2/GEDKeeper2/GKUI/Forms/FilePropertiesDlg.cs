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

using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class FilePropertiesDlg : CommonDialog<IFilePropertiesDlg, FilePropertiesDlgController>, IFilePropertiesDlg
    {
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
            InitializeComponent();

            fController = new FilePropertiesDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }
    }
}
