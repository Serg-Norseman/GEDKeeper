/*
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
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RepositoryEditDlgController : DialogController<IRepositoryEditDlg>
    {
        private GDMRepositoryRecord fRepository;

        public GDMRepositoryRecord Repository
        {
            get { return fRepository; }
            set {
                if (fRepository != value) {
                    fRepository = value;
                    UpdateView();
                }
            }
        }


        public RepositoryEditDlgController(IRepositoryEditDlg view) : base(view)
        {
            fView.Name.Activate();
        }

        public override bool Accept()
        {
            try {
                fRepository.RepositoryName = fView.Name.Text;

                fBase.NotifyRecord(fRepository, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("RepositoryEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Name.Text = fRepository.RepositoryName;

            fView.NotesList.ListModel.DataOwner = fRepository;
        }

        public void ModifyAddress()
        {
            BaseController.ModifyAddress(fBase, fRepository.Address);
        }
    }
}
