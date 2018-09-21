/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupEditDlgController : DialogController<IGroupEditDlg>
    {
        private GEDCOMGroupRecord fGroup;

        public GEDCOMGroupRecord Group
        {
            get { return fGroup; }
            set {
                if (fGroup != value) {
                    fGroup = value;
                    UpdateView();
                }
            }
        }


        public GroupEditDlgController(IGroupEditDlg view) : base(view)
        {
            fView.Name.Activate();
        }

        public override bool Accept()
        {
            try {
                fGroup.GroupName = fView.Name.Text;

                fBase.NotifyRecord(fGroup, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("GroupEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Name.Text = (fGroup == null) ? "" : fGroup.GroupName;

            fView.MembersList.ListModel.DataOwner = fGroup;
            fView.NotesList.ListModel.DataOwner = fGroup;
            fView.MediaList.ListModel.DataOwner = fGroup;
        }
    }
}
