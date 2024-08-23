/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;

namespace GKUI.Forms
{
    public sealed partial class SourceCallNumberEditDlg : CommonDialog<ISourceCallNumberEditDlg, SourceCallNumberDlgController>, ISourceCallNumberEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblCallNumber;
        private TextBox txtNumber;
        private Label lblMediaType;
        private ComboBox cmbMediaType;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMSourceCallNumber CallNumber
        {
            get { return fController.CallNumber; }
            set { fController.CallNumber = value; }
        }

        #region View Interface

        ITextBox ISourceCallNumberEditDlg.CallNumberText
        {
            get { return GetControlHandler<ITextBox>(txtNumber); }
        }

        IComboBox ISourceCallNumberEditDlg.MediaTypeCombo
        {
            get { return GetControlHandler<IComboBox>(cmbMediaType); }
        }

        #endregion

        public SourceCallNumberEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new SourceCallNumberDlgController(this);
            fController.Init(baseWin);
        }
    }
}
