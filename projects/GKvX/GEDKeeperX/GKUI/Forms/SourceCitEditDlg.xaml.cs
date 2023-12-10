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

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;

namespace GKUI.Forms
{
    public sealed partial class SourceCitEditDlg : CommonDialog<ISourceCitEditDlg, SourceCitEditDlgController>, ISourceCitEditDlg
    {
        public GDMSourceCitation SourceCitation
        {
            get { return fController.SourceCitation; }
            set { fController.SourceCitation = value; }
        }

        #region View Interface

        ITextBox ISourceCitEditDlg.Page
        {
            get { return GetControlHandler<ITextBox>(txtPage); }
        }

        IComboBox ISourceCitEditDlg.Certainty
        {
            get { return GetControlHandler<IComboBox>(txtCertainty); }
        }

        IComboBox ISourceCitEditDlg.Source
        {
            get { return GetControlHandler<IComboBox>(cmbSource); }
        }

        IDateControl ISourceCitEditDlg.DataDate
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        ITextBox ISourceCitEditDlg.DataText
        {
            get { return GetControlHandler<ITextBox>(txtText); }
        }

        #endregion

        public SourceCitEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new SourceCitEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnSourceAdd_Click(object sender, EventArgs e)
        {
            fController.AddSource();
        }
    }
}
