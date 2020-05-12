﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class RecordInfoDlg : CommonDialog, IRecordInfoDlg
    {
        private readonly RecordInfoDlgController fController;

        public IBaseWindow Base
        {
            get { return fController.Base; }
        }

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

            Text = LangMan.LS(LSID.LSID_MIFileProperties);
            hyperView1.OnLink += HyperViewLink;

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
