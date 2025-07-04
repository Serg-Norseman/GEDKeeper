﻿/*
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

using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;

namespace GKUI.Forms
{
    public sealed partial class SourceCallNumberEditDlg : CommonDialog<ISourceCallNumberEditDlg, SourceCallNumberDlgController>, ISourceCallNumberEditDlg
    {
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
            InitializeComponent();

            fController = new SourceCallNumberDlgController(this);
            fController.Init(baseWin);
        }
    }
}
