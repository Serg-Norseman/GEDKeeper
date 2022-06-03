﻿/*
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

using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class LanguageEditDlg : CommonDialog<ILanguageEditDlg, LanguageEditDlgController>, ILanguageEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ComboBox cmbLanguage;
        private Label lblLanguage;
        private Button btnCancel;
        private Button btnAccept;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMLanguageID LanguageID
        {
            get { return fController.LanguageID; }
            set { fController.LanguageID = value; }
        }

        #region View Interface

        IComboBox ILanguageEditDlg.LanguageCombo
        {
            get { return GetControlHandler<IComboBox>(cmbLanguage); }
        }

        #endregion

        public LanguageEditDlg()
        {
            XamlReader.Load(this);

            fController = new LanguageEditDlgController(this);
        }
    }
}
