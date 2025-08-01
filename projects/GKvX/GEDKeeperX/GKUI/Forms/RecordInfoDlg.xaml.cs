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
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class RecordInfoDlg : CommonDialog, IRecordInfoDlg
    {
        private readonly RecordInfoDlgController fController;

        public GDMRecord Record
        {
            get { return fController.Record; }
            set { fController.Record = value; }
        }

        #region View Interface

        IHyperView IRecordInfoDlg.HyperView
        {
            get { return hyperView1; }
        }

        #endregion

        public RecordInfoDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new RecordInfoDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }

        private void HyperViewLink(object sender, string linkName)
        {
            fController.SelectLink(linkName);
        }
    }
}
